;;; rustic-transient.el --- Transient based popup-*-lexical-binding: t-*-

;;; Code:

(require 'rustic-cargo)

(defvar trace-me nil "Trace level")

(define-infix-command rustic-transient:-trace ()
  :description "Trace level"
  :class 'transient-switch
  :key "-t"
  :shortarg "-t"
  :argument "--trace-level= "
  ;; :variable 'trace-me
  :choices '("0" "1" "Full"))

(defun rustic-transient--get-args (menu)
  "Retrieve the arguments provided to the transient called MENU."
  (transient-args menu))

(defun rustic--transient-help (command)
  "Display the help menu for the cargo COMMAND from the Rustic-Transient menu."
  (shell-command (concat "cargo " command " --help") "*Help-Buffer-Rustic*"))


;; Load the transient popup only if transient itself is instaled
(define-transient-command general-menu ()
  "Rustic Cargo Commands"
  [
   ["Quick Commands"
    (rustic-transient:-trace)
    ("b" "Build" rustic-cargo-build)
    ("f" "Format" rustic-cargo-fmt)
    ("r" "Run" rustic-cargo-run)
    ("c" "Clippy" rustic-cargo-clippy)
    ("o" "Outdated" rustic-cargo-outdated)
    ("e" "Clean" rustic-cargo-clean)
    ("k" "Check" rustic-cargo-check)
    ("t" "Test" rustic-cargo-test)
    ]
   ["Cargo Menus"
    ]
   ])

(define-transient-command doc-menu
  "Rustic Cargo Doc Commands"
  [["Arguments"
    ("-q" "Run command without output buffer" ("-q" "--quiet"))
    ("-o" "Open doc" ("-o" "--open"))
    ("-a" "Build all docs" ("-a" "--all"))
    ("-d" "Document private items" ("-e" "--document-private-items"))
    ("-l" "Document Only this package's library" ("-l"
                                                  "--document-private-items"))
    ("-b" "Document all binaries" ("-b" "--bins"))
    ("-r" "Build artifacts in release mode" ("-r" "--release"))
    ("-A" "All features" ("-A" "--all-features"))
    ("-n" "No default features" ("-n" "--no-default-features"))
    ("-V" "Verbose" ("-v" "--verbose"))
    ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
    ("-O" "Run without accessing the network" ("-O" "--offline"))
    ]
   ["Make Docs"
    ("b" "Build docs" test-func-arg)
    ("H" "Help" rustic--transient-doc-help)
    ]
   ])

(define-transient-command rustic--transient-new ()
  "Transient for cargo new command"
  [[ "Options"
     ("-q" "Run command without output buffer" ("-q" "--quiet"))
     ("-V" "Verbose" ("-v" "--verbose"))
     ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
     ("-O" "Run without accessing the network" ("-O" "--offline"))
     ]]
  )

;;;###autoload
(defun rustic--transient-doc-help ()
  (interactive)
  (rustic--transient-help "doc"))

;;;###autoload
(defun rustic--transient-build-help ()
  (rustic--transient-help "build"))

;;;###autoload
(defun rustic--transient-new-help ()
  (rustic--transient-help "new"))

;;;###autoload
(defun rustic--transient-test-help ()
  (rustic--transient-help "test"))

(doc-menu)
;; (transient )
(defun rustic--transient-popup ()
  "Invoke the rustic transient popup."
  (interactive)
  (rustic-mode--transient))

(provide 'rustic-transient)
;;; rustic-transient.el ends here

