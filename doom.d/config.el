;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Set the font
(setq doom-font (font-spec :family "Sarasa Mono SC" :size 14 :weight 'bold))

;; Change leader key to ,
(setq doom-leader-key ","
      doom-leader-alt-key "M-,"
      doom-localleader-key ", m"
      doom-localleader-alt-key "M-, m")

;; Reconfigure ivy
(after! ivy
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected))

;; Reconfigure company
(after! company-mode
  (map! :i "C-j" #'company-complete-common)
  (setq-default company-idle-delay 0
                company-minimum-prefix-length 2))

;; Reconfigure org
(after! org                                                  
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

  (setq org-capture-templates
        '(("h" "Homework" entry (file+headline "~/Dropbox/task.org"  "Homework")
           "* TODO %?\n%U\n%a\n")
          ("s" "Schedule" entry (file+headline "~/Dropbox/task.org" "Schedule")
           "* TODO %?\n%U\n%a\n")
          ("p" "Project" entry (file+headline "~/Dropbox/task.org" "Project")
           "* TODO %?\n%U\n%a\n")))

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
  (add-to-list '+latex-viewers 'evince)
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
