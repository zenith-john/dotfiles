;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! ox-hugo)
(package! cdlatex)
(package! org-edit-latex)
(package! keyfreq)
(package! emacs-snippets :disable t)
(package! org-noter)
(package! ms-python)
(package! org-ref)
(package! fd-dired :recipe (:fetcher github :repo "yqrashawn/fd-dired"))
(package! graphviz-dot-mode)
