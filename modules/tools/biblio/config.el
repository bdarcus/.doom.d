;;; tools/biblio/config.el -*- lexical-binding: t; -*-

(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :commands (ivy-bibtex)
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  (map! :leader
    :map (org-mode-map markdown-mode-map latex-mode-map)
    (:prefix("i" . "insert")
    :desc "Citation" "c" #'ivy-bibtex)))

(use-package! helm-bibtex
  :when (featurep! :completion helm)
  :commands helm-bibtex
  :config
  ; modify so that inserting the citation is the default action
  ; FIXME doesn't work; not sure why
  (helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
  (helm-add-action-to-source "Insert BibTeX key" 'bibtex-completion-insert-key helm-source-bibtex 0)
  (map! :leader
    :map (org-mode-map markdown-mode-map latex-mode-map)
    (:prefix("i" . "insert")
    :desc "Citation" "c" #'helm-bibtex)))
