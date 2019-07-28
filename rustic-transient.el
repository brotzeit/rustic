;;; rustic-transient.el --- Transient based popup -*-lexical-binding: t-*-

;;; Code:

(require 'rustic-cargo)

;; (defun rustic-transient--get-args (menu)
;;   "Retrieve the arguments provided to the transient called MENU."
;;   (let ((t-args (transient-args menu))
;;         (split-args '())
;;         (curr-split-arg ""))
;;     (dolist (elem t-args)
;;       (if (string-match "^--[[:alnum:]]+=" elem)
;;           (setq split-args (push split-args))
;;         (setq split-args (push elem split-args))
;;         )
;;       )
;;     )
;;   (transient-args menu))

(defun rustic-transient--get-args (menu)
  "Retrieve the arguments"
  (transient-args menu))

;; (string-match "^--[[:alnum:]]+=" "--helloworld=abcfasfsa")
;; (dolist (el
;;          (split-string "--helloworld=onetwo" "="))
;;   (message el))

;;; Help commands
(defun rustic--transient-help (command)
  "Display the help menu for the cargo COMMAND from the Rustic-Transient menu.
A new buffer is created which contains the command."
  (let ((help-buff "*Help-Buffer-Rustic*"))
    ;; Kill help buffer if it exists
    (if (get-buffer help-buff)
        (kill-buffer help-buff))
    (shell-command (concat "cargo " command " --help") help-buff)
    (pop-to-buffer help-buff)
    (read-only-mode)
    (local-set-key (kbd "q") 'kill-buffer-and-window)
    (if (package-installed-p 'evil)
        (evil-local-set-key 'normal (kbd "q") 'kill-buffer-and-window))
    (goto-line 1)))

;;;###autoload
(defun rustic--transient-doc-help ()
  (interactive)
  (rustic--transient-help "doc"))

;;;###autoload
(defun rustic--transient-build-help ()
  (interactive)
  (rustic--transient-help "build"))

;;;###autoload
(defun rustic--transient-new-help ()
  (interactive)
  (rustic--transient-help "new"))

;;;###autoload
(defun rustic--transient-test-help ()
  (interactive)
  (rustic--transient-help "test"))

;;;###autoload
(defun rustic--transient-check-help ()
  (interactive)
  (rustic--transient-help "check"))

;;;###autoload
(defun rustic--transient-clean-help ()
  (interactive)
  (rustic--transient-help "clean"))

;;;###autoload
(defun rustic--transient-clean-help ()
  (interactive)
  (rustic--transient-help "init"))

;;;###autoload
(defun rustic--transient-run-help ()
  (interactive)
  (rustic--transient-help "run"))

;;;###autoload
(defun rustic--transient-bench-help ()
  (interactive)
  (rustic--transient-help "bench"))

;;;###autoload
(defun rustic--transient-update-help ()
  (interactive)
  (rustic--transient-help "update"))

;;;###autoload
(defun rustic--transient-publish-help ()
  (interactive)
  (rustic--transient-help "publish"))

;;;###autoload
(defun rustic--transient-install-help ()
  (interactive)
  (rustic--transient-help "install"))

;;;###autoload
(defun rustic--transient-uninstall-help ()
  (interactive)
  (rustic--transient-help "uninstall"))

;;;###autoload
(defun rustic--transient-cargo-help ()
  (interactive)
  (rustic--transient-help ""))

(defun test-args (&optional args)
  (interactive)
  (message "%s" (rustic-transient--get-args 'rustic--transient-doc)))

(define-infix-command rustic-transient:--trace ()
  :description "Trace level"
  :class 'transient-switches
  :key "-t"
  :argument-format "%s"
  :argument-regexp "0\\|1\\|Full"
  :choices '("0" "1" "Full"))

(define-infix-command rustic--transient--general:-v ()
  :description "Version control system to be used"
  :class 'transient-switches
  :key "-t"
  :argument-format "%s"
  :argument-regexp "git\\|hg\\|pijul\\|fossil\\|none"
  :choices '("git" "hg" "pijul" "fossil" "none"))

(define-infix-argument rustic--transient--general:-j ()
  :description "The number of processors to use"
  :class 'transient-option
  :shortarg "-j"
  :argument "--jobs="
  )

(define-infix-argument rustic--transient--general:-e ()
  :description "Packages to exclude"
  :class 'transient-option
  :shortarg "-e"
  :argument "--exclude="
  )

(define-infix-argument rustic--transient--general:-D ()
  :description "Directory for all generated artifacts"
  :class 'transient-option
  :shortarg "-D"
  :argument "--target-dir="
  )

(define-infix-argument rustic--transient--general:-k ()
  :description "Build for the target triple"
  :class 'transient-option
  :shortarg "-k"
  :argument "--target="
  )

(define-infix-argument rustic--transient--general:-m ()
  :description "Path to Cargo.toml"
  :class 'transient-option
  :shortarg "-m"
  :argument "--manifest-path="
  )

(define-infix-argument rustic--transient--general:-F ()
  :description "Space separated list of features to activate"
  :class 'transient-option
  :shortarg "-F"
  :argument "--features="
  )

;;;###autoload
(define-transient-command rustic--transient-general-menu ()
  "Rustic Cargo Commands"
  [
   ["Quick Commands"
    (rustic-transient:--trace)
    ("b" "Build" rustic-cargo-build)
    ("f" "Format" rustic-cargo-fmt)
    ("r" "Run" rustic-cargo-run)
    ("c" "Clippy" rustic-cargo-clippy)
    ("o" "Outdated" rustic-cargo-outdated)
    ("e" "Clean" rustic-cargo-clean)
    ("k" "Check" rustic-cargo-check)
    ("t" "Test" rustic-cargo-test)
    ]
   ["Advanced Cargo Menus"
    ("D" "Doc" rustic--transient-doc)
    ("N" "New" rustic--transient-new)
    ("C" "Clean" rustic--transient-clean)
    ("T" "Test" rustic--transient-test)
    ]
   ])

(define-infix-argument rustic--transient--doc:-F ()
  :description "Space separated list of features to activate"
  :class 'transient-option
  :shortarg "-F"
  :argument "--features="
  )

(define-infix-argument rustic--transient--doc:-b ()
  :description "Document only the specified binary"
  :class 'transient-option
  :shortarg "-B"
  :argument "--bin="
  )

(define-infix-argument rustic--transient--doc:-p ()
  :description "Package to document"
  :class 'transient-option
  :shortarg "-p"
  :argument "--package="
  )

;;;###autoload
(define-transient-command rustic--transient-doc ()
  "Rustic Cargo Doc Commands"
  [
   ["Documentation And Packages"
    ("-o" "Open doc" ("-o" "--open"))
    ("-N" "No dependiencies" ("-N" "--no-deps"))
    ("-d" "Document private items" ("-e" "--document-private-items"))
    (rustic--transient--doc:-p)
    ("-a" "Build all docs" ("-a" "--all"))
    (rustic--transient--general:-e)
    ]
   ["Targets And Features"
    ("-l" "Document only this package's library" ("-l" "--lib"))
    (rustic--transient--doc:-b)
    ("-b" "Document all binaries" ("-b" "--bins"))
    ("-A" "All features" ("-A" "--all-features"))
    ("-n" "No default features" ("-n" "--no-default-features"))
    (rustic--transient--doc:-F)
    ]
   ]
  [
   ["Display"
    ("-q" "Run quietly" ("-q" "--quiet"))
    ("-V" "Verbose" ("-v" "--verbose"))
    (rustic--transient--general:-j)
    (rustic--transient--general:-D)
    (rustic--transient--general:-k)
    ]
   ["Compilation and Manifest"
    ("-r" "Build artifacts in release mode" ("-r" "--release"))
    (rustic--transient--general:-m)
    ("-M" "Require Cargo.lock is up to date" ("-M" "--locked"))
    ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
    ("-O" "Run without accessing the network" ("-O" "--offline"))
    ]
   ]
  ["Make Docs"
   ("b" "Build docs" test-args)
   ("B" "Build docs default" test-args)
   ("H" "Help" rustic--transient-doc-help)
   ]
  )

(define-infix-argument rustic--transient--clean:-D ()
  :description "Directory for all generated artifacts"
  :class 'transient-option
  :shortarg "-D"
  :argument "--target-dir="
  )

(define-infix-argument rustic--transient--clean:-p ()
  :description "Package to clean artifacts for"
  :class 'transient-option
  :shortarg "-p"
  :argument "--package="
  )

(define-infix-argument rustic--transient--clean:-k ()
  :description "Target triple to clean output for"
  :class 'transient-option
  :shortarg "-k"
  :argument "--target="
  )

;;;###autoload
(define-transient-command rustic--transient-clean ()
  "Rustic Cargo Clean Menu"
  [
   ["Package and Clean Options"
    (rustic--transient--clean:-p)
    (rustic--transient--clean:-D)
    (rustic--transient--clean:-k)
    ("-r" "Clean release artifacts" ("-r" "--release"))
    ("-d" "Clean just the documentation directory" ("-d" "--doc"))
    ]
   ]
  [
   ["Manifest"
    (rustic--transient--general:-m)
    ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
    ("-P" "Require Cargo.lock to be up to date" ("-P" "--locked"))
    ("-O" "Run without accessing the network" ("-O" "--offline"))
    ]
   ]
  ["Display"
   ("-q" "Run quietly" ("-q" "--quiet"))
   ("-V" "Verbose" ("-v" "--verbose"))
   ]
  ["Commands"
   ("H" "Help" rustic--transient-clean-help)
   ]
  )

(define-infix-argument rustic--transient--new:-r ()
  :description "Package to document"
  :class 'transient-option
  :shortarg "-r"
  :argument "--registry= "
  )

(define-infix-argument rustic--transient--new:-e ()
  :description "Edition of the package"
  :class 'transient-switch
  :shortarg "-e"
  :argument "--edition= "
  )

(define-infix-argument rustic--transient--new:-n ()
  :description "Name of the new package"
  :class 'transient-switch
  :shortarg "-n"
  :argument "--name= "
  )

(define-infix-command rustic--transient--new:-v ()
  :description "Version control system to be used"
  :class 'transient-switch
  :shortarg "-v"
  :argument "--vcs="
  :choices '("git" "hg" "pijul" "fossil" "none"))

;;;###autoload
(define-transient-command rustic--transient-new ()
  "Transient for cargo new command"
  [[ "Options"
     (rustic--transient--new:-e)
     (rustic--transient--new:-n)
     (rustic--transient--new:-r)
     (rustic--transient--new:-v)
     ("-b" "Use a binary template(DEFAULT)" ("-b" "--bin"))
     ("-l" "Use a library template" ("-l" "--lib"))
     ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
     ("-O" "Run without accessing the network" ("-O" "--offline"))
     ("-q" "Run command without output buffer" ("-q" "--quiet"))
     ("-V" "Verbose" ("-v" "--verbose"))
     ]
   ["Commands"
    ("H" "Help" rustic--transient-new-help)
    ]
   ]
  )

(define-infix-argument rustic--transient--run:-D ()
  :description "Directory for all generated artifacts"
  :class 'transient-option
  :shortarg "-D"
  :argument "--target-dir="
  )

(define-infix-argument rustic--transient--run:-k ()
  :description "Build for the target triple"
  :class 'transient-option
  :shortarg "-k"
  :argument "--target="
  )

(define-infix-argument rustic--transient--run:-b ()
  :description "Name of the bin target to run"
  :class 'transient-option
  :shortarg "-B"
  :argument "--bin="
  )

(define-infix-argument rustic--transient--run:-p ()
  :description "Package with the target to run"
  :class 'transient-option
  :shortarg "-p"
  :argument "--package="
  )

(define-infix-argument rustic--transient--run:-e ()
  :description "Name of the example target to run"
  :class 'transient-option
  :shortarg "-e"
  :argument "--example="
  )


;;;###autoload
(define-transient-command rustic--transient-run ()
  "Rustic Cargo Run Menu"
  ["Arguments"
   [
    ("-q" "Run quietly" ("-q" "--quiet"))
    ("-r" "Clean release artifacts" ("-r" "--release"))
    (rustic--transient--general:-m)
    (rustic--transient--general:-F)
    (rustic--transient--general:-j)
    (rustic--transient--run:-D)
    (rustic--transient--run:-b)
    (rustic--transient--run:-e)
    (rustic--transient--run:-k)
    (rustic--transient--run:-p)
    ("-A" "All features" ("-A" "--all-features"))
    ("-n" "No default features" ("-n" "--no-default-features"))
    ("-V" "Verbose" ("-v" "--verbose"))
    ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
    ("-P" "Require Cargo.lock to be up to date" ("-P" "--locked"))
    ("-O" "Run without accessing the network" ("-O" "--offline"))
    ]
   ]
  )


(define-infix-argument rustic--transient--bench:-D ()
  :description "Directory for all generated artifacts"
  :class 'transient-option
  :shortarg "-D"
  :argument "--target-dir="
  )

(define-infix-argument rustic--transient--bench:-k ()
  :description "Build for the target triple"
  :class 'transient-option
  :shortarg "-k"
  :argument "--target="
  )

(define-infix-argument rustic--transient--bench:-b ()
  :description "Benchmark only the specified binary"
  :class 'transient-option
  :shortarg "-B"
  :argument "--bin="
  )

(define-infix-argument rustic--transient--bench:-p ()
  :description "Package to run benchmarks fro"
  :class 'transient-option
  :shortarg "-p"
  :argument "--package="
  )

(define-infix-argument rustic--transient--bench:-e ()
  :description "Benchmark only the specified example"
  :class 'transient-option
  :shortarg "-e"
  :argument "--example="
  )
;;;###autoload
(define-transient-command rustic--transient-bench ()
  "Rustic Cargo Bench Menu"
  [
   ["Arguments"
    (rustic--transient--bench:-D)
    (rustic--transient--bench:-e)
    (rustic--transient--bench:-k)
    (rustic--transient--bench:-p)
    (rustic--transient--bench:-b)
    ("-q" "Run quietly" ("-q" "--quiet"))
    ("-V" "Verbose" ("-v" "--verbose"))
    ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
    ("-O" "Run without accessing the network" ("-O" "--offline"))
    ("-b" "Benchmark all binaries" ("-b" "--bins"))
    ("-l" "Benchmark only this package's library" ("-l" "--lib"))
    ("-A" "All features" ("-A" "--all-features"))
    ("-n" "No default features" ("-n" "--no-default-features"))
    ]
   ])

;;;###autoload
(define-transient-command rustic--transient-test ()
  "Rustic Cargo Test Menu"
  [
   ["Arguments"
    ("-q" "Run quietly" ("-q" "--quiet"))
    ("-V" "Verbose" ("-v" "--verbose"))
    ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
    ("-O" "Run without accessing the network" ("-O" "--offline"))
    ("-b" "Document all binaries" ("-b" "--bins"))
    ("-l" "Use a library template" ("-l" "--lib"))
    ("-A" "All features" ("-A" "--all-features"))
    ("-n" "No default features" ("-n" "--no-default-features"))
    ]
   ])

(defun rustic-transient-popup ()
  "Invoke the rustic transient popup."
  (interactive)
  (rustic--transient-general-menu))

(rustic--transient-general-menu)

(provide 'rustic-transient)
;;; rustic-transient.el ends here
