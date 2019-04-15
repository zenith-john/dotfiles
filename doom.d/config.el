;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Set the font
(setq doom-font (font-spec :family "Sarasa Mono SC" :size 16 :weight 'bold))

;; Change leader key to ,
(setq doom-leader-key ","
      doom-leader-alt-key "M-,"
      doom-localleader-key ", m"
      doom-localleader-alt-key "M-, m")

(setq-default truncate-lines nil)

(def-package! ox-hugo
  :after ox
  :init
  (require 'ox-hugo))

(def-package! cdlatex
  :commands (org-cdlatex-mode)
  :after org
  :init
  (add-hook! org-mode
    (org-cdlatex-mode 1))
  (add-hook! latex-mode
    (cdlatex-mode 1)))

(def-package! org-edit-latex
  :after org
  :init
  (require 'org-edit-latex))

(def-package! keyfreq
  :commands (keyfreq-mode keyfreq-show keyfreq-reset)
  :config
  (keyfreq-mode 1))

(def-package! org-noter
  :commands (org-noter)
  :after pdf-tools
  :config
  ;; Overriding the function due to make visual-line mode no effect.
  (defun org-noter--set-notes-scroll (window &rest ignored)
    nil)
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil)
  (map! :map pdf-view-mode-map :gn "C-i" #'org-noter-insert-note-toggle-no-questions)
  (map! :map pdf-view-mode-map :gn "q" #'org-noter-kill-session)
  (map! :map pdf-view-mode-map :gn "i" #'org-noter-insert-note))

(def-package! ms-python
  :init
  (setq ms-python-server-install-dir (concat doom-private-dir "third_party/python-language-server/output/bin/Release/"))
  (add-hook! python-mode (lsp)))

(def-package! org-ref
  :after org
  :init
  (require 'bibtex-completion)
  (setq org-ref-completion-library 'org-ref-reftex)
  (defhydra org-ref-cite-hydra (:color blue)
    "
_p_: Open pdf     _w_: WOS          _g_: Google Scholar _K_: Copy citation to clipboard
_u_: Open url     _r_: WOS related  _P_: Pubmed         _k_: Copy key to clipboard
_n_: Open notes   _c_: WOS citing   _C_: Crossref       _f_: Copy formatted entry
_o_: Open entry   _e_: Email entry  ^ ^                 _q_: quit
"
    ("o" org-ref-open-citation-at-point nil)
    ("p" org-ref-open-pdf-at-point nil)
    ("n" org-ref-open-notes-at-point nil)
    ("u" org-ref-open-url-at-point nil)
    ("w" org-ref-wos-at-point nil)
    ("r" org-ref-wos-related-at-point nil)
    ("c" org-ref-wos-citing-at-point nil)
    ("g" org-ref-google-scholar-at-point nil)
    ("P" org-ref-pubmed-at-point nil)
    ("C" org-ref-crossref-at-point nil)
    ("K" org-ref-copy-entry-as-summary nil)
    ("k" (progn
	       (kill-new
	        (car (org-ref-get-bibtex-key-and-file))))
     nil)
    ("f" (kill-new
	      (org-ref-format-entry (org-ref-get-bibtex-key-under-cursor)))
     nil)

    ("e" (kill-new (save-excursion
		             (org-ref-open-citation-at-point)
		             (org-ref-email-bibtex-entry)))
     nil)
    ("q" nil))
  :config
  (setq org-ref-cite-onclick-function (lambda (_) (org-ref-cite-hydra/body)))
  (setq org-ref-pdf-directory "~/Documents/Library/")
  (setq org-ref-default-bibliography '("~/Dropbox/Library.bib"))

  ;; Make citation work
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
	      "biber %b"
	      "%latex -interaction nonstopmode -output-directory %o %f"
	      "%latex -interaction nonstopmode -output-directory %o %f")))

(def-package! fd-dired
  :commands (fd-dired)
  :init
  (when (executable-find "fd")
      (evil-define-command +evil:fd (query)
        "Ex interface for fd-dired"
        (interactive "<a>")
        (fd-dired (file-name-directory (buffer-file-name)) query))
      (evil-ex-define-cmd "fd" #'+evil:fd)))

;; Add hook for change input method
(after! evil
  (defvar +zenith/previous-input-method nil "The previous input method")
  (defvar +zenith/english-input-method "xkb:us::eng" "The english method used")
  (defun +zenith/restore-input-method ()
    (interactive)
    (when +zenith/previous-input-method
      (shell-command (concat "ibus engine " +zenith/previous-input-method))))

  (defun +zenith/change-to-english-input-method ()
    (interactive)
    (setq +zenith/previous-input-method (shell-command-to-string "ibus engine"))
    (shell-command (concat "ibus engine " +zenith/english-input-method)))

  (add-hook 'evil-insert-state-exit-hook #'+zenith/change-to-english-input-method)
  (add-hook 'evil-insert-state-entry-hook #'+zenith/restore-input-method))


;; Reconfigure ivy
(after! ivy
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected))

;; Configure persp-mode
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override nil))

;; Reconfigure company
(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (map! :i "C-j" #'company-complete-common))

(after! lsp-ui
  (setq lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-max-width 50
        lsp-ui-doc-max-height 30
        lsp-ui-doc-position 'at-point)
  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 0)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t))))

;; configure yasnippet disable tab expand
(after! yasnippet
  (map! :map yas-minor-mode-map
        "<tab>" nil
        "TAB" nil
        :i
        "M-j" yas-maybe-expand))

;; Reconfigure org
(after! org

  (sp-with-modes 'org-mode
    (sp-local-pair "=" "=" :actions :rem))

  (add-hook! org-mode
    ;; Enable cdlatex mode
    ;; TODO configure cdlatex-command-alist
    (setq-local company-idle-delay nil)
    (flycheck-mode 0)
    (visual-line-mode 1)
    (display-line-numbers-mode 0)
    (LaTeX-math-mode 1)
    (setq truncate-lines nil))

  (remove-hook! org-mode
    #'auto-fill-mode)

  ;; Make emphasis clear when using bold font
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "pink")))

  (setf org-highlight-latex-and-related '(latex))

  ;; Make latex formulation more clear
  (plist-put org-format-latex-options :scale 2)

  (require 'org-edit-latex)

  (setq org-directory "~/Dropbox/"
        org-agenda-files '("~/Dropbox/")
        org-default-notes-file "~/Dropbox/refile.org")

  ;; Org-todo and org-capture
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "PAUSE(p)" "SOMEDAY(s)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(c@)")
          (sequence "[ ](T)" "[-](P)" "[?](m)" "|" "[X](D)")))

  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("NEXT" ("WAITING" . nil) ("SOMEDAY" . nil))
          (done ("WAITING" . nil))
          ("PAUSE" ("WAITING" . nil) ("CANCELLED" . nil))
          ("TODO" ("WAITING" . nil) ("CANCELLED" . nil) ("SOMEDAY" . nil))
          ("DONE" ("WAITING" . nil) ("TODO" . nil) ("SOMEDAY" . nil))
          ("SOMEDAY" ("TODO" . nil))))

  ;; Org Capture
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

  (setq org-capture-templates
        '(("p" "Protocol" entry (file+headline "~/Documents/Notes/notes.org" "Bookmarks")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	      ("L" "Protocol Link" entry (file+headline "~/Documents/Notes/notes.org" "Bookmarks")
           "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
          ("h" "Homework" entry (file+headline "~/Dropbox/task.org"  "Homework")
           "* TODO %? :Homework:\n")
          ("s" "Schedule" entry (file+headline "~/Dropbox/task.org" "Schedule")
           "* TODO %?\n")
          ("r" "Project" entry (file+headline "~/Dropbox/task.org" "Project")
           "* TODO %?\n")
          ("q" "Question" entry (file+headline "~/Dropbox/task.org" "Question")
           "* TODO %? :Question:\n")
          ("d" "Idea" entry (file+headline "~/Dropbox/task.org" "Idea")
           "* TODO %? :Idea:\n")))

  ;; For org-protocol from https://github.com/sprig/org-capture-extension
  (defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  (defun kk/delete-frame-if-neccessary (&rest r)
    (cond
     ((= kk/delete-frame-after-capture 0) nil)
     ((> kk/delete-frame-after-capture 1)
      (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
     (t
      (setq kk/delete-frame-after-capture 0)
      (delete-frame))))

  (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)
  ;; Org tag
  (setq org-tag-alist
        '(("Improvement" . ?i) ("Homework" . ?h) ("Personal" . ?p) ("Question" . ?q) ("Idea" . ?d)))


  ;; Org agenda settings
  (setq org-enable-table-editor 'optimized
        org-agenda-start-on-weekday nil
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-span 14
        org-agenda-compact-blocks t
        org-agenda-show-all-dates nil
        org-deadline-warning-days 365
        org-agenda-show-future-repeats nil)


  (setq org-agenda-custom-commands
        '(("b" "My Agenda View" ((tags-todo "Question/!-Waiting/!-Pause"
                                       ((org-agenda-overriding-header "Unsolved Questions:")))
                                 (tags "AGENDAHEADER" ((org-agenda-overriding-header "========================================\nToday's Schedule:")))
                                 (agenda ""
                                         ((org-agenda-show-all-dates t)
                                          (org-agenda-span 'day)
                                          (org-deadline-warning-days 0)
                                          (org-agenda-start-day "+0d")))
                                 (todo "NEXT"
                                       ((org-agenda-overriding-header "========================================\nNext Tasks:")))
                                 (tags "BEFOREWEEKGLANCE" ((org-agenda-overriding-header "========================================\nNext Week Glance:")))
                                 (agenda ""
                                         ((org-agenda-show-all-dates t)
                                          (org-agenda-span 6)
                                          (org-agenda-start-day "+1d")))
                                 (tags-todo "Improvement/!-NEXT" ((org-agenda-overriding-header "========================================\nImprove Yourself:")))
                                 (tags-todo "Idea+TODO<>\"NEXT\"|Personal+TODO<>\"NEXT\"" ((org-agenda-overriding-header "\nPersonal Project:")))
                                 (tags "BEFOREDEADLINE" ((org-agenda-overriding-header "========================================\nFar Away Tasks:")))
                                 (agenda ""
                                         ((org-agenda-span 180)
                                          (org-agenda-time-grid nil)
                                          (org-agenda-show-all-dates nil)
                                          (org-agenda-entry-types '(:deadline :scheduled))
                                          (org-agenda-start-day "+7d")))))))

  ;; Org latex export
  (setq org-latex-with-hyperref t)
  (setq org-latex-compiler "xelatex")
  (setq org-latex-default-packages-alist
        '(("" "hyperref" nil)
          ("AUTO" "inputenc" t)
          ("" "fixltx2e" nil)
          ("" "graphicx" t)
          ("" "longtable" nil)
          ("" "float" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "textcomp" t)
          ("" "marvosym" t)
          ("" "wasysym" t)
          ("" "multicol" t)
          ("" "amssymb" t)
          "\\tolerance=1000"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)))



  (eval-after-load 'ox-latex
    '(add-to-list 'org-latex-classes
                  '("ctexart"
                    "\\documentclass{ctexart}"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

;; Latex support
(after! tex
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-bibpath-environment-variables '("/home/zenith-john/Dropbox/"))
  (setq reftex-bibliography-commands '("bibliography" "nobibiliography" "addbibresource"))
  (setq-default TeX-engine 'xetex
                TeX-show-compilation t)

  (add-to-list 'TeX-command-list
               '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
                 TeX-run-command t t :help "Run xelatex") t))
