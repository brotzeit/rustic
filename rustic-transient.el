;;; rustic-transient.el --- Transient based popup-*-lexical-binding: t-*-

;;; Code:

(require 'rustic-cargo)

;;;###autoload
(define-infix-command rustic-transient:-trace ()
  :description "Trace level"
  :class 'transient-switch
  :shortarg "-t"
  :argument "--trace-level= "
  :choices '("0" "1" "Full"))


(defun rustic-transient--get-args (menu)
  "Retrieve the arguments provided to the transient called MENU."
  (transient-args menu))

(defun rustic--transient-help (command)
  "Display the help menu for the cargo COMMAND from the Rustic-Transient menu."
  (shell-command (concat "cargo " command " --help") "*Help-Buffer-Rustic*"))


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
  :description "Packages to exclude"
  :class 'transient-option
  :shortarg "-D"
  :argument "--target-dir="
  )


(define-infix-argument rustic--transient--general:-m ()
  :description "Packages to exclude"
  :class 'transient-option
  :shortarg "-m"
  :argument "--manifest-path="
  )

(define-infix-argument rustic--transient--doc:-b ()
  :description "Packages to exclude"
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
  [["Arguments"
    (rustic--transient--general:-j)
    (rustic--transient--doc:-p)
    (rustic--transient--doc:-b)
    ("-q" "Run quietly" ("-q" "--quiet"))
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
    ("b" "Build docs" rustic-cargo-build)
    ("H" "Help" rustic--transient-doc-help)
    ]
   ])

;;;###autoload
(define-transient-command rustic--transient-new ()
  "Transient for cargo new command"
  [[ "Options"
     ("-b" "Use a binary template(DEFAULT)" ("-b" "--bin"))
     ("-l" "Use a library template" ("-l" "--lib"))
     ("-f" "Require Cargo.lock and cache to be up to date" ("-f" "--frozen"))
     ("-O" "Run without accessing the network" ("-O" "--offline"))
     ("-q" "Run command without output buffer" ("-q" "--quiet"))
     ("-V" "Verbose" ("-v" "--verbose"))
     ]]
  )

;;;###autoload
(define-transient-command rustic--transient-general-menu ()
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
   ["Advanced Cargo Menus"
    ("D" "Doc" rustic--transient-doc)
    ("N" "New" rustic--transient-new)
    ]
   ])


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

;; (transient )
(defun rustic--transient-popup ()
  "Invoke the rustic transient popup."
  (interactive)
  (rustic--transient-general-menu))

(rustic--transient-general-menu)
(provide 'rustic-transient)
;;; rustic-transient.el ends here
