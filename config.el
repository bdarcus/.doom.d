;;; user info

(setq user-full-name "Bruce D'Arcus"
      user-mail-address "bdarcus@gmail.com")

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(setq auth-sources '("~/.authinfo"))

;;; biblio

;(use-package! oc-csl-ns :after oc)

;(after! org-roam
;(org-roam-bibtex-mode +1))

(defvar bd/bibliography '("~/org/bib/newer.bib"))
(defvar bd/notes '("~/org/roam/biblio/"))
(defvar bd/library-files '("~/org/pdf/"))

(use-package! pdf-occur)

(after! oc
  (setq!
   org-cite-global-bibliography bd/bibliography))

(custom-set-faces!
  `(embark-target :background ,(doom-blend (doom-color 'bg) (doom-color 'highlight) 0.75)))

(after! citar
  (setq citar-bibliography bd/bibliography
        citar-notes-paths bd/notes
        citar-library-paths bd/library-files
        citar-symbol-separator "  "
        citar-format-reference-function 'citar-citeproc-format-reference
        bd/csl-styles-dir "~/.local/share/csl/styles"
        org-cite-csl-styles-dir bd/csl-styles-dir
        citar-citeproc-csl-styles-dir bd/csl-styles-dir
        citar-citeproc-csl-locales-dir "~/.local/share/csl/locales"
        citar-citeproc-csl-style (file-name-concat org-cite-csl-styles-dir "apa-6th-edition.csl")
        bibtex-dialect 'biblatex
        citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))

  (set-face-attribute 'citar-highlight nil
                      :foreground "Lightblue"
                      :weight 'bold))

(defun bd/search-pdf-contents (keys-entries &optional str)
  "Search pdfs."
  (interactive (list (citar-select-refs)))
  (let ((files (citar-file--files-for-multiple-entries
                (citar--ensure-entries keys-entries)
                citar-library-paths
                '("pdf")))
        (search-str (or str (read-string "Search string: "))))
    (pdf-occur-search files search-str t)))

(after! embark
  (add-to-list 'embark-multitarget-actions #'bd/search-pdf-contents))

;; define the keymap
(defvar bd/citar-embark-become-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'citar-open-library-files)
    (define-key map (kbd "o") #'citar-open)
    (define-key map (kbd "i") #'citar-insert-citation)
    (define-key map (kbd "c") #'biblio-crossref-lookup)
    (define-key map (kbd "s") #'biblio-dissemin-lookup)
  map)
  "Citar Embark become keymap for biblio lookup.")

;; tell embark about the keymap
(after! embark
  (add-to-list 'embark-become-keymaps 'bd/citar-embark-become-map))

(after! oc
  (defun org-ref-to-org-cite ()
    "Attempt to convert org-ref citations to org-cite syntax."
    (interactive)
    (let* ((cite-conversions '(("cite" . "//b") ("Cite" . "//bc")
                               ("nocite" . "/n")
                               ("citep" . "") ("citep*" . "//f")
                               ("parencite" . "") ("Parencite" . "//c")
                               ("citeauthor" . "/a/f") ("citeauthor*" . "/a")
                               ("citeyear" . "/na/b")
                               ("Citep" . "//c") ("Citealp" . "//bc")
                               ("Citeauthor" . "/a/cf") ("Citeauthor*" . "/a/c")
                               ("autocite" . "") ("Autocite" . "//c")
                               ("notecite" . "/l/b") ("Notecite" . "/l/bc")
                               ("pnotecite" . "/l") ("Pnotecite" . "/l/bc")))
           (cite-regexp (rx (regexp (regexp-opt (mapcar #'car cite-conversions) t))
                            ":" (group (+ (not (any "\n     ,.)]}")))))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cite-regexp nil t)
          (message (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2)))
          (replace-match (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2))))))))

;; use vertico-crm prototype for multi-selection
(use-package! vertico-crm)

(use-package! consult-dir
  :after consult
  :bind (:map vertico-map))

(use-package! oxr)

;;; Visuals

(setq doom-font (font-spec :family "JetBrainsMono" :size 12))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
(setq-default line-spacing 0.1)

(set-language-environment "UTF-8")

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

;(use-package! origami
;  :commands (origami-toggle-node origami-toggle-all-nodes)
;  :hook (text-mode . origami-mode)
;  :init
;  :config
;  (map! :leader
;        :prefix "t"
;        :desc "Origami-Toggle All Nodes" "O" #'origami-toggle-all-nodes
;        :desc "Origami-Toggle Node" "o" #'origami-toggle-node))

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

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

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

(setq ispell-dictionary "en-custom")

(setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))
