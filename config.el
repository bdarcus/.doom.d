;;; user info

(require 'dash)

(setq user-full-name "Bruce D'Arcus"
      user-mail-address "bdarcus@gmail.com")

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;;; biblio
(setq! bibtex-completion-library-path "~/org/pdf/"
       bibtex-completion-bibliography '("~/org/bib/newer.bib"
                                            "~/org/bib/me.bib")
       bibtex-completion-notes-path "~/org/roam/biblio/")
       bibtex-completion-additional-search-fields '(tags doi url journal booktitle))

;;; Visuals

(setq doom-font (font-spec :family "JetBrainsMono" :size 12))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)

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

;;; aliases

(defalias 'bb 'bibtex-actions-insert-bibtex "bibtex-actions-insert-bibtex")
(defalias 'bc 'bibtex-actions-insert-citation "bibtex-actions-insert-citation")
(defalias 'bk 'bibtex-actions-insert-key "bibtex-actions-insert-key")
(defalias 'bo 'bibtex-actions-open "bibtex-actions-open")
(defalias 'bn 'bibtex-actions-open-notes "bibtex-actions-open-notes")
(defalias 'be 'bibtex-actions-open-entry "bibtex-actions-open-entry")
