;;; user info

(setq user-full-name "Bruce D'Arcus"
      user-mail-address "bdarcus@gmail.com")

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;;; biblio
(setq! bibtex-completion-library-path "~/org/pdf/"
       bibtex-completion-bibliography '("~/org/bib/newer.bib"
                                            "~/org/bib/me.bib")
       bibtex-completion-notes-path "~/org/roam/biblio/"
       bibtex-completion-additional-search-fields '(tags doi url journal booktitle))

(setq embark-general-map `(keymap (?G . ,embark-general-map)))
(define-key bibtex-actions-org-cite-map "G" '("general actions >" . (embark-general-map)))
(setq which-key-sort-order 'which-key-description-order)


;; use vertico-crm prototype for multi-selection
;(vertico-crm-mode)

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

;;; org latex

(setq org-latex-compiler "lualatex")

(customize-set-value 'org-latex-hyperref-template "
\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},
 pdfsubject={%d},\n pdfcreator={%c},\n pdflang={%L},\n colorlinks=true}\n")

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


;;; aliases

(defalias 'bd/cite 'bibtex-actions-insert-citation "bibtex-actions-insert-citation")

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
