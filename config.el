;;; user info

(require 'dash)

(setq user-full-name "Bruce D'Arcus"
      user-mail-address "bdarcus@gmail.com")

;;; biblio
(setq! +biblio-pdf-library-dir "~/org/pdf/"
       +biblio-default-bibliography-files '("~/org/bib/newer.bib"
                                            "~/org/bib/me.bib")
       +biblio-notes-path "~/org/roam/biblio/")
(setq bibtex-completion-additional-search-fields '(tags doi url journal booktitle))


;(defun my/add-bib-watches (paths)
;  "Add path watches for all PATHS."
;  ;; TODO add message for success/failure; what about removal?
;  (let ((flat-paths (-flatten paths)))
;    (cl-loop
;     for path in flat-paths
;     do
;     (file-notify-rm-watch path)
;     (file-notify-add-watch
;      path '(change) 'bibtex-actions-refresh))))

(setq my/bib-paths
 (list
  bibtex-completion-bibliography
  bibtex-completion-notes-path
  bibtex-completion-library-path
  "My bib paths."))

;; Add watches for all bib paths.
;(my/add-bib-watches my/bib-paths)


(setq which-key-sort-order 'which-key-description-order)

(load "bindings.el")

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

;;; org-roam; v2

(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
  (setq org-roam-directory (file-truename "~/org/roam")
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (org-roam-setup)
  (setq org-roam-mode-sections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section))
  (setq display-buffer-alist
        '(;; Left side window
          ("org-roam:.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . left)
           (slot . 0))))

  ;; this one  is for org-roam-buffer-toggle
  (setq display-buffer-alist
        '(;; Left side window
          (".org-roam.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . left)
           (slot . 0))))
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :head "#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+roam_key: ${ref}
#+roam_tags: website
#+title: ${title}
- source :: ${ref}"
           :unnarrowed t)))
  (add-to-list 'org-capture-templates `("c" "org-protocol-capture" entry (file+olp ,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory) "The List")
                                         "* TO-READ [[%:link][%:description]] %^g"
                                         :immediate-finish t))
  (add-to-list 'org-agenda-custom-commands `("r" "Reading"
                                             ((todo "WRITING"
                                                    ((org-agenda-overriding-header "Writing")
                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory)))))
                                              (todo "READING"
                                                    ((org-agenda-overriding-header "Reading")
                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory)))))
                                              (todo "TO-READ"
                                                    ((org-agenda-overriding-header "To Read")
                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory))))))))
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n\n")))
  (set-company-backend! 'org-mode '(company-capf)))

;; org-roam v2 keymap

(defvar my-org-roam-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") 'org-roam-buffer-toggle)
    (define-key map (kbd "i") 'org-roam-node-insert)
    (define-key map (kbd "f") 'org-roam-node-find)
    (define-key map (kbd "r") 'org-roam-ref-find)
    (define-key map (kbd "g") 'org-roam-show-graph)
    (define-key map (kbd "c") 'org-roam-capture)
    (define-key map (kbd "j") 'org-roam-dailies-capture-today)
    map)
  "Keymap for 'org-roam' v2.")

; make available my-org-roam-map to embark-act
(add-to-list 'embark-keymap-alist '(org-roam-node . my-org-roam-map))


;; from vulpea; auto add org-ids to note file

(defun +org-auto-id-add-to-headlines-in-file ()
  "Add ID property to the current file and all its headlines."
  (when (and (or (eq major-mode 'org-mode)
                 (eq major-mode 'org-journal-mode))
             (eq buffer-read-only nil))
    (save-excursion
      (widen)
      (goto-char (point-min))
      (org-id-get-create)
      (org-map-entries #'org-id-get-create))))

;(add-hook 'before-save-hook #'+org-auto-id-add-to-headlines-in-file)

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
