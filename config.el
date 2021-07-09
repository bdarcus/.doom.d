;;; user info

(require 'dash)
(require 'bibtex-completion)
(require 'bibtex-actions)
(require 'vertico-crm)

(setq user-full-name "Bruce D'Arcus"
      user-mail-address "bdarcus@gmail.com")

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;;; biblio
(setq! bibtex-completion-library-path "~/org/pdf/"
       bibtex-completion-bibliography '("~/org/bib/newer.bib"
                                            "~/org/bib/me.bib")
       bibtex-completion-notes-path "~/org/roam/biblio/"
       bibtex-completion-additional-search-fields '(tags doi url journal booktitle))

;; use vertico-crm prototype for multi-selection
(vertico-crm-mode)

;;; Visuals

(setq doom-font (font-spec :family "JetBrainsMono" :size 12))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
(setq-default line-spacing 0.1)

(setq projectile-project-search-path "~/Projects")

;;; Spelling and Grammar
(require 'flycheck-aspell)
;(use-package! flycheck
;  :ensure t
;  :init (global-flycheck-mode))
(add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
(add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
(flycheck-add-next-checker 'markdown-aspell-dynamic 'proselint)
(setq ispell-program-name (executable-find "aspell")
      ispell-dictionary "en_US")
(setq flyspell-correct-popup t)
(setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")

(define-key key-translation-map (kbd "<f8> n")   (kbd "–"))  ; en dash
(define-key key-translation-map (kbd "<f8> m")   (kbd "—"))  ; em dash

;;; Folding

(use-package! origami
  :commands (origami-toggle-node origami-toggle-all-nodes)
  :hook (text-mode . origami-mode)
  :init
  :config
  (map! :leader
        :prefix "t"
        :desc "Origami-Toggle All Nodes" "O" #'origami-toggle-all-nodes
        :desc "Origami-Toggle Node" "o" #'origami-toggle-node))

;;; nxml mode

(add-hook 'nxml-mode
          (lambda ()
            (setq company-backends '(company-nxml))))
(setq auto-complete-nxml-popup-help-key "C-:")
(setq auto-complete-nxml-toggle-automatic-key "C-c C-t")
(setq auto-complete-nxml-automatic-p t)

;;; org-cite
;(require 'oc-basic)
;(require 'oc-natbib)
;(require 'oc-biblatex)
;(require 'oc-csl)

;(setq org-cite-csl-styles-dir "~/.local/share/csl")
;(setq org-cite-activate-processor 'basic)
;(setq org-cite-follow-processor 'basic)
;(setq org-cite-global-bibliography '("~/org/bib/newer.bib" "~/Code/org-mode/test.bib"))
;(setq bibtex-completion-bibliography "~/org/bib/newer.bib")


(after! ox-latex
  (add-to-list 'org-latex-classes
;; TODO change this
               '("scr-article"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
             '("memoir-article"
               "\\documentclass[11pt,oneside,article]{memoir}
                [PACKAGES]
                \\usepackage{memoir-article-style}
                [NO-DEFAULT-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-default-class "scr-article"
      org-latex-tables-booktabs t
      org-latex-hyperref-template "
\\colorlet{greenyblue}{blue!70!green}
\\colorlet{blueygreen}{blue!40!green}
\\providecolor{link}{named}{greenyblue}
\\providecolor{cite}{named}{blueygreen}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=,
  urlcolor=link,
  citecolor=cite\n}
\\urlstyle{same}
")

;; org-roam v2
(use-package! org-roam
  :after org
  :commands
  (org-roam-buffer
   org-roam-setup
   org-roam-capture
   org-roam-node-find)
  :config
  (setq org-roam-directory "~/org/roam")
  (org-roam-setup))

(add-to-list 'completion-at-point-functions #'bibtex-actions-complete-key-at-point)


;;; aliases

(defalias 'bb 'bibtex-actions-insert-bibtex "bibtex-actions-insert-bibtex")
(defalias 'bc 'bibtex-actions-insert-citation "bibtex-actions-insert-citation")
(defalias 'bk 'bibtex-actions-insert-key "bibtex-actions-insert-key")
(defalias 'bo 'bibtex-actions-open "bibtex-actions-open")
(defalias 'bn 'bibtex-actions-open-notes "bibtex-actions-open-notes")
(defalias 'be 'bibtex-actions-open-entry "bibtex-actions-open-entry")

;;; embark
;;(setq prefix-help-command #'embark-prefix-help-command)
;;(setq embark-action-indicator nil)

;;; for nativecomp
(setq native-comp-async-report-warnings-errors nil)

;;; keycast and screencast; from tecosaur

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))

(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-start-or-stop)
  (setq gif-screencast-program "flameshot_bash"
        ; the above script is just:
        ; /usr/bin/flameshot full -r>"$1"
        gif-screencast-args '()
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
  (defun gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string
      "\n+" "\n"
      (mapconcat (lambda (c) (if (listp (cdr c))
                                 (cadr c))) doom-themes--colors "\n"))
     'utf-8
     "/tmp/doom-color-theme" ))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))
