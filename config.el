;;; user info

(setq user-full-name "Bruce D'Arcus"
      user-mail-address "bdarcus@gmail.com")


;;; biblio
(setq! +biblio-pdf-library-dir "~/org/pdf/"
       +biblio-default-bibliography-files '("~/org/bib/newer.bib")
       +biblio-notes-path "~/org/roam/biblio/")
(setq bibtex-completion-additional-search-fields '(tags doi url journal booktitle))

(setq which-key-sort-order 'which-key-description-order)


;; zotero notes output to org

(defun my-orb-latex-note-to-org (citekey)
  (let* ((entry (bibtex-completion-get-entry citekey))
         (note (bibtex-completion-get-value "note" entry ""))
         (pandoc-command "pandoc --from latex --to org")
         result)
    (with-temp-buffer
      (shell-command (format "echo \"%s\" | %s" note pandoc-command)
                     (current-buffer))
      (setq result (buffer-substring-no-properties (point-min) (point-max))))))

(setq orb-preformat-keywords   '(("citekey" . "=key=") "note" "title" "url" "file" "author-or-editor" "keywords"))
(setq orb-templates
      '(("r" "ref" plain #'org-roam-capture--get-point
         ""
         :file-name "${citekey}"
         :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n
- tags ::
- keywords :: ${keywords}
- author(s) :: ${author-or-editor}
\n* Annotations (zotero)\n\n
%(my-orb-latex-note-to-org \"${citekey}\")"
         :unnarrowed t)))

;;; latex

(setq org-latex-compiler "lualatex")

;;; org-roam
(setq org-roam-tag-sources '(prop all-directories))
(setq org-roam-verbose t)

(use-package! org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))
(unless (server-running-p)
  (org-roam-server-mode))

;;; Visuals

(setq doom-font (font-spec :family "JetBrainsMono" :size 14))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)

(setq projectile-project-search-path "~/Projects")

;;; Spelling and Grammar

(setq ispell-program-name (executable-find "hunspell")
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
