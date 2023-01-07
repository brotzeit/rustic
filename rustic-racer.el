;;; rustic-racer.el --- Racer support -*-lexical-binding: t-*-

;;; Code:

(require 'f)
(require 's)

(require 'button)
(require 'etags)
(require 'help-mode)
(require 'thingatpt)

(require 'rustic)

(defvar rustic-racer-args nil)

(defgroup racer nil
  "Docs browsing for Rust via racer."
  :link '(url-link "https://github.com/racer-rust/emacs-racer/")
  :group 'rustic-mode)

;;; Customization

(defcustom rustic-racer-cmd
  (or (executable-find "racer")
      (f-expand "~/.cargo/bin/racer")
      "/usr/local/bin/racer")
  "Path to the racer binary."
  :type 'file
  :group 'racer)

(defcustom rustic-racer-rust-src-path
  (or
   (getenv "RUST_SRC_PATH")
   (when (executable-find "rustc")
     (let* ((sysroot (s-trim-right
                      (shell-command-to-string
                       (format "%s --print sysroot" (executable-find "rustc")))))
            (src-path (f-join sysroot "lib/rustlib/src/rust/src")))
       (when (file-exists-p src-path)
         src-path)
       src-path))
   "/usr/local/src/rust/src")

  "Path to the rust source tree.
If nil, we will query $RUST_SRC_PATH at runtime.
If $RUST_SRC_PATH is not set, look for rust source
in rustup's install directory."
  :type 'file
  :group 'racer)

(defcustom rustic-racer-cargo-home
  (or
   (getenv "CARGO_HOME")
   "~/.cargo")
  "Path to your current cargo home. Usually `~/.cargo'.
If nil, we will query $CARGO_HOME at runtime."
  :type 'file
  :group 'racer)

;;; Faces

(define-obsolete-face-alias 'rustic-racer-help-heading-face
  'rustic-racer-help-heading "1.2")

(defface rustic-racer-help-heading
  '((t :weight bold))
  "Face for markdown headings in *Racer Help* buffers.")

(defface rustic-racer-tooltip
  '((((min-colors 16777216))
     :background "#292C33" :foreground "white")
    (t
     :background "black" :foreground "white"))
  "Face used for the tooltip with `racer-describe-tooltip'")

;;; Help-mode

(defvar rustic-racer-help-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    map)
  "Keymap for racer help mode.")

(define-derived-mode rustic-racer-help-mode fundamental-mode
  "Racer-Help"
  "Major mode for *Racer Help* buffers.
Commands:
\\{racer-help-mode-map}")

(defun rustic-racer-header (text)
  "Helper function for adding text properties to TEXT."
  (propertize text 'face 'rustic-racer-help-heading))

(defun rustic-racer-button-go-to-src (button)
  (rustic-racer-find-file
   (button-get button 'path)
   (button-get button 'line)
   (button-get button 'column)))

(define-button-type 'racer-src-button
  'action 'rustic-racer-button-go-to-src
  'follow-link t
  'help-echo "Go to definition")

(defun rustic-racer-url-button (text url)
  "Return a button that opens a browser at URL."
  (with-temp-buffer
    (insert-text-button
     text
     :type 'help-url
     'help-args (list url))
    (buffer-string)))

(defun rustic-racer-src-button (path line column)
  "Return a button that navigates to PATH at LINE number and
COLUMN number."
  ;; Convert "/foo/bar/baz/foo.rs" to "baz/foo.rs"
  (let* ((filename (f-filename path))
         (parent-dir (f-filename (f-parent path)))
         (short-path (f-join parent-dir filename)))
    (with-temp-buffer
      (insert-text-button
       short-path
       :type 'racer-src-button
       'path path
       'line line
       'column column)
      (buffer-string))))

(defun rustic-racer-help-buf (contents)
  "Create a *Racer Help* buffer with CONTENTS."
  (rustic--inheritenv
   (let ((buf (get-buffer-create "*Racer Help*"))
         ;; If the buffer already existed, we need to be able to
         ;; override `buffer-read-only'.
         (inhibit-read-only t))
     (with-current-buffer buf
       (erase-buffer)
       (insert contents)
       (setq buffer-read-only t)
       (goto-char (point-min))
       (rustic-racer-help-mode))
     buf)))

;;; Racer

(defvar rustic-racer-prev-state nil)

(defun rustic-racer-call (command &rest args)
  "Call racer command COMMAND with args ARGS.
Return stdout if COMMAND exits normally, otherwise show an
error."
  (let ((rust-src-path (or rustic-racer-rust-src-path (getenv "RUST_SRC_PATH")))
        (cargo-home (or rustic-racer-cargo-home (getenv "CARGO_HOME"))))
    (when (null rust-src-path)
      (user-error "You need to set `rustic-racer-rust-src-path' or `RUST_SRC_PATH'"))
    (unless (file-exists-p rust-src-path)
      (user-error "No such directory: %s. Please set `rustic-racer-rust-src-path' or `RUST_SRC_PATH'"
                  rust-src-path))
    (let ((default-directory (funcall rustic-compile-directory-method))
          (process-environment (append (list
                                        (format "RUST_SRC_PATH=%s" (expand-file-name rust-src-path))
                                        (format "CARGO_HOME=%s" (expand-file-name cargo-home)))
                                       process-environment)))
      (-let [(exit-code stdout _stderr)
             (rustic-racer-shell-command rustic-racer-cmd (cons command args))]
        ;; Use `equal' instead of `zero' as exit-code can be a string
        ;; "Aborted" if racer crashes.
        (unless (equal 0 exit-code)
          (user-error "%s exited with %s. `M-x rustic-racer-debug' for more info"
                      rustic-racer-cmd exit-code))
        stdout))))

(defun rustic-racer-doc (name)
  "Return a *Racer Help* buffer for the function or type at point.
If there are multiple candidates at point, use NAME to find the
correct value."
  (let ((description (rustic-racer-describe-at-point name)))
    (when description
      (let* ((name (plist-get description :name))
             (raw-docstring (plist-get description :docstring))
             (docstring (if raw-docstring
                            (rustic-racer-propertize-docstring raw-docstring)
                          "Not documented."))
             (kind (plist-get description :kind)))
        (rustic-racer-help-buf
         (format
          "%s is %s defined in %s.\n\n%s%s"
          name
          (rustic-racer-kind-description kind)
          (rustic-racer-src-button
           (plist-get description :path)
           (plist-get description :line)
           (plist-get description :column))
          (if (equal kind "Module")
              ;; No point showing the 'signature' of modules, which is
              ;; just their full path.
              ""
            (format "    %s\n\n" (rustic-racer-syntax-highlight (plist-get description :signature))))
          docstring))))))

(defun rustic-racer-describe-at-point (name)
  "Get a description of the symbol at point matching NAME.
If there are multiple possibilities with this NAME, prompt
the user to choose."
  (let* ((output-lines (save-excursion
                         ;; Move to the end of the current symbol, to
                         ;; increase racer accuracy.
                         (skip-syntax-forward "w_")
                         (rustic-racer-call-at-point "complete-with-snippet")))
         (all-matches (--map (when (s-starts-with-p "MATCH " it)
                               (rustic-racer-split-snippet-match it))
                             output-lines))
         (relevant-matches (--filter (equal (plist-get it :name) name)
                                     all-matches)))
    (if (> (length relevant-matches) 1)
        ;; We might have multiple matches with the same name but
        ;; different types. E.g. Vec::from.
        (let ((signature
               (completing-read "Multiple matches: "
                                (--map (plist-get it :signature) relevant-matches))))
          (--first (equal (plist-get it :signature) signature) relevant-matches))
      (-first-item relevant-matches))))

(defmacro rustic-racer-with-temporary-file (path-sym &rest body)
  "Create a temporary file, and bind its path to PATH-SYM.
Evaluate BODY, then delete the temporary file."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,path-sym (make-temp-file "racer")))
     (unwind-protect
         (progn ,@body)
       (delete-file ,path-sym))))

(defun rustic-racer-call-at-point (command)
  "Call racer command COMMAND at point of current buffer.
Return a list of all the lines returned by the command."
  (rustic-racer-with-temporary-file tmp-file
    (write-region nil nil tmp-file nil 'silent)
    (let ((racer-args (list
                       command
                       (number-to-string (line-number-at-pos))
                       (number-to-string (rustic-racer-current-column)))))
      ;; If this buffer is backed by a file, pass that to racer too.
      (-when-let (file-name (buffer-file-name (buffer-base-buffer)))
        (setq rustic-racer-args
              (append racer-args (list file-name))))

      (setq rustic-racer-args (append rustic-racer-args (list tmp-file)))
      (s-lines
       (s-trim-right
        (apply #'rustic-racer-call rustic-racer-args))))))

(defun rustic-racer-shell-command (program args)
  "Execute PROGRAM with ARGS.
Return a list (exit-code stdout stderr)."
  (rustic-racer-with-temporary-file tmp-file-for-stderr
    (let (exit-code stdout stderr)
      ;; Create a temporary buffer for `call-process` to write stdout
      ;; into.
      (rustic--with-temp-process-buffer
        (setq exit-code
              (apply #'call-process program nil
                     (list (current-buffer) tmp-file-for-stderr)
                     nil args))
        (setq stdout (buffer-string)))
      (setq stderr (rustic-racer-slurp tmp-file-for-stderr))
      (setq rustic-racer-prev-state
            (list
             :program program
             :args args
             :exit-code exit-code
             :stdout stdout
             :stderr stderr
             :default-directory default-directory
             :process-environment process-environment))
      (list exit-code stdout stderr))))

;;; Utility Functions

(defun rustic-racer-slurp (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun rustic-racer-read-rust-string (string)
  "Convert STRING, a rust string literal, to an elisp string."
  (when string
    (->> string
      ;; Remove outer double quotes.
      (s-chop-prefix "\"")
      (s-chop-suffix "\"")
      ;; Replace escaped characters.
      (s-replace "\\n" "\n")
      (s-replace "\\\"" "\"")
      (s-replace "\\'" "'")
      (s-replace "\\;" ";"))))

(defun rustic-racer-url-p (target)
  "Return t if TARGET looks like a fully qualified URL."
  (not (null
        (string-match-p (rx bol "http" (? "s") "://") target))))

(defun rustic-racer-propertize-links (markdown)
  "Propertize links in MARKDOWN."
  (replace-regexp-in-string
   ;; Text of the form [foo](http://example.com)
   (rx "[" (group (+? (not (any "]")))) "](" (group (+? anything)) ")")
   ;; For every match:
   (lambda (whole-match)
     ;; Extract link and target.
     (let ((link-text (match-string 1 whole-match))
           (link-target (match-string 2 whole-match)))
       ;; If it's a web URL, use a clickable link.
       (if (rustic-racer-url-p link-target)
           (rustic-racer-url-button link-text link-target)
         ;; Otherwise, just discard the target.
         link-text)))
   markdown))

(defun rustic-racer-propertize-all-inline-code (markdown)
  "Given a single line MARKDOWN, replace all instances of `foo` or
\[`foo`\] with a propertized string."
  (let ((highlight-group
         (lambda (whole-match)
           (rustic-racer-syntax-highlight (match-string 1 whole-match)))))
    (->> markdown
      (replace-regexp-in-string
       (rx "[`" (group (+? anything)) "`]")
       highlight-group)
      (replace-regexp-in-string
       (rx "`" (group (+? anything)) "`")
       highlight-group))))

(defun rustic-racer-indent-block (str)
  "Indent every line in STR."
  (s-join "\n" (--map (concat "    " it) (s-lines str))))

(defun rustic-racer-trim-newlines (str)
  "Remove newlines from the start and end of STR."
  (->> str
    (s-chop-prefix "\n")
    (s-chop-suffix "\n")))

(defun rustic-racer-remove-footnote-links (str)
  "Remove footnote links from markdown STR."
  (->> (s-lines str)
    (--remove (string-match-p (rx bol "[`" (+? anything) "`]: ") it))
    (s-join "\n")
    ;; Collapse consecutive blank lines caused by removing footnotes.
    (s-replace "\n\n\n" "\n\n")))

(defun rustic-racer-docstring-sections (docstring)
  "Split DOCSTRING into text, code and heading sections."
  (let* ((sections nil)
         (current-section-lines nil)
         (section-type :text)
         ;; Helper function.
         (finish-current-section
          (lambda ()
            (when current-section-lines
              (let ((current-section
                     (s-join "\n" (nreverse current-section-lines))))
                (unless (s-blank? current-section)
                  (push (list section-type current-section) sections))
                (setq current-section-lines nil))))))
    (dolist (line (s-lines docstring))
      (cond
       ;; If this is a closing ```
       ((and (s-starts-with-p "```" line) (eq section-type :code))
        (push line current-section-lines)
        (funcall finish-current-section)
        (setq section-type :text))
       ;; If this is an opening ```
       ((s-starts-with-p "```" line)
        (funcall finish-current-section)
        (push line current-section-lines)
        (setq section-type :code))
       ;; Headings
       ((and (not (eq section-type :code)) (s-starts-with-p "# " line))
        (funcall finish-current-section)
        (push (list :heading line) sections))
       ;; Normal text.
       (t
        (push line current-section-lines))))
    (funcall finish-current-section)
    (nreverse sections)))

(defun rustic-racer-clean-code-section (section)
  "Given a SECTION, a markdown code block, remove
fenced code delimiters and code annotations."
  (->> (s-lines section)
    (-drop 1)
    (-drop-last 1)
    ;; Ignore annotations like # #[allow(dead_code)]
    (--remove (s-starts-with-p "# " it))
    (s-join "\n")))

(defun rustic-racer-propertize-docstring (docstring)
  "Replace markdown syntax in DOCSTRING with text properties."
  (let* ((sections (rustic-racer-docstring-sections docstring))
         (propertized-sections
          (--map (-let [(section-type section) it]
                   ;; Remove trailing newlines, so we can ensure we
                   ;; have consistent blank lines between sections.
                   (rustic-racer-trim-newlines
                    (pcase section-type
                      (:text
                       (rustic-racer-propertize-all-inline-code
                        (rustic-racer-propertize-links
                         (rustic-racer-remove-footnote-links
                          section))))
                      (:code
                       (rustic-racer-indent-block
                        (rustic-racer-syntax-highlight
                         (rustic-racer-clean-code-section section))))
                      (:heading
                       (rustic-racer-header
                        (s-chop-prefix "# " section))))))
                 sections)))
    (s-join "\n\n" propertized-sections)))

(defun rustic-racer-find-file (path line column)
  "Open PATH and move point to LINE and COLUMN."
  (find-file path)
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char column))

(defun rustic-racer-kind-description (raw-kind)
  "Human friendly description of a rust kind.
For example, 'EnumKind' -> 'an enum kind'."
  (let* ((parts (s-split-words raw-kind))
         (description (s-join " " (--map (downcase it) parts)))
         (a (if (string-match-p (rx bos (or "a" "e" "i" "o" "u")) description)
                "an" "a")))
    (format "%s %s" a description)))

(defun rustic-racer-current-column ()
  "Get the current column based on underlying character representation."
  (length (buffer-substring-no-properties
           (line-beginning-position) (point))))

(defun rustic-racer-syntax-highlight (str)
  "Apply font-lock properties to a string STR of Rust code."
  (let (result)
    ;; Load all of STR in a rustic-mode buffer, and use its
    ;; highlighting.
    (with-temp-buffer
      (insert str)
      (delay-mode-hooks (rustic-mode))
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings
          (font-lock-fontify-buffer)))
      (setq result (buffer-string)))
    (when (and
           ;; If we haven't applied any properties yet,
           (null (text-properties-at 0 result))
           ;; and if it's a standalone symbol, then assume it's a
           ;; variable.
           (string-match-p (rx bos (+ (any lower "_")) eos) str))
      (setq result (propertize str 'face 'font-lock-variable-name-face)))
    result))

(defun rustic-racer-split-parts (raw-output)
  "Given RAW-OUTPUT from racer, split on semicolons and doublequotes.
Unescape strings as necessary."
  (let ((parts nil)
        (current "")
        (i 0))
    (while (< i (length raw-output))
      (let ((char (elt raw-output i))
            (prev-char (and (> i 0) (elt raw-output (1- i)))))
        (cond
         ;; A semicolon that wasn't escaped, start a new part.
         ((and (equal char ?\;) (not (equal prev-char ?\\)))
          (push current parts)
          (setq current ""))
         (t
          (setq current (concat current (string char))))))
      (setq i (1+ i)))
    (push current parts)
    (mapcar #'rustic-racer-read-rust-string (nreverse parts))))

(defun rustic-racer-split-snippet-match (line)
  "Given LINE, a string \"MATCH ...\" from complete-with-snippet,
split it into its constituent parts."
  (let* ((match-parts (rustic-racer-split-parts line))
         (docstring (nth 7 match-parts)))
    (when (and match-parts (equal (length match-parts) 8))
      (list :name (s-chop-prefix "MATCH " (nth 0 match-parts))
            :line (string-to-number (nth 2 match-parts))
            :column (string-to-number (nth 3 match-parts))
            :path (nth 4 match-parts)
            ;; Struct or Function:
            :kind (nth 5 match-parts)
            :signature (nth 6 match-parts)
            :docstring (if (> (length docstring) 0) docstring nil)))))

;;; Interactive

;;;###autoload
(defun rustic-racer-describe ()
  "Show a *Racer Help* buffer for the function or type at point."
  (interactive)
  (let ((buf (rustic-racer-doc (thing-at-point 'symbol))))
    (if buf
        (pop-to-buffer buf)
      (user-error "No function or type found at point"))))

;;; _
(provide 'rustic-racer)
;;; rustic-racer.el ends here
