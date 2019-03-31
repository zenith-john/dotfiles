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

(def-package! keyfreq
  :commands (keyfreq-mode keyfreq-show keyfreq-reset)
  :config
  (keyfreq-mode 1))

(def-package! evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines
             comment-or-uncomment-region
             evilnc-comment-operator)
  :init
  (map! :nv
        "gc" 'evilnc-comment-or-uncomment-lines)
  (map! :leader
        :prefix "c"
        "c" #'evilnc-comment-or-uncomment-lines
        "R" #'comment-or-uncomment-region
        "\\" #'evilnc-comment-operator))

(def-package! interleave
  :commands (interleave-mode)
  :after pdf-tools
  :config
  (defun +maybe-interleave-quit ()
    (interactive)
    (if interleave-pdf-mode
        (interleave-quit)
      (kill-this-buffer)))
  (map! :map pdf-view-mode-map :gn "q" #'+maybe-interleave-quit)
  (map! :map pdf-view-mode-map :gn "i" #'interleave-add-note))

(def-package! org-ref
  :after org
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (require 'org-ref)
  :config
  (setq org-ref-default-bibliography '("~/Dropbox/Library.bib")))

(def-package! fd-dired
  :commands (fd-dired)
  :init
  (evil-define-command +evil:fd (query)
    "Ex interface for fd-dired"
    (interactive "<a>")
    (fd-dired (file-name-directory (buffer-file-name)) query))
  (evil-ex-define-cmd "fd" #'+evil:fd))

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

;; configure yasnippet disable tab expand
(after! yasnippet
  (map! :map yas-minor-mode-map
        "<tab>" nil
        "TAB" nil
        :i
        [tab] nil
        "M-j" yas-maybe-expand))

;; Reconfigure org
(after! org

  ;; Make emphasis clear when using bold font
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "pink")))

  (add-hook! org-mode
    ;; Enable cdlatex mode
    ;; TODO configure cdlatex-command-alist
    (visual-line-mode 1)
    (display-line-numbers-mode 0)
    (org-cdlatex-mode 1)
    (setq truncate-lines nil))


  ;; Make latex formulation more clear
  (plist-put org-format-latex-options :scale 2)

  (require 'org-edit-latex)

  (remove-hook! org-mode
    #'auto-fill-mode)

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
  (setq org-capture-templates
        '(("h" "Homework" entry (file+headline "~/Dropbox/task.org"  "Homework")
           "* TODO %?\n")
          ("s" "Schedule" entry (file+headline "~/Dropbox/task.org" "Schedule")
           "* TODO %?\n")
          ("p" "Project" entry (file+headline "~/Dropbox/task.org" "Project")
           "* TODO %?\n")))

  ;; Org tag
  (setq org-tag-alist
        '(("Improvement" . ?i) ("Homework" . ?h) ("Personal" . ?p)))


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
        '(("b" "My Agenda View" (
                                 (tags "AGENDAHEADER" ((org-agenda-overriding-header "Today's Schedule:")))
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
                                 (tags-todo "Personal/!-NEXT" ((org-agenda-overriding-header "\nPersonal Project:")))
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


  (with-eval-after-load 'ox
    (require 'ox-hugo))


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
