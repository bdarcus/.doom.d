;;; tools/biblio/config.el -*- lexical-binding: t; -*-

(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :commands (ivy-bibtex)
  :config
;  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus))
  (map! :leader
    :map (org-mode-map markdown-mode-map latex-mode-map)
    (:prefix("n" . "note")
    :desc "Bibliograpic entries" "b" #'ivy-bibtex)))

(use-package! helm-bibtex
  :when (featurep! :completion helm)
  :commands helm-bibtex
  :config
  (map! :leader
    :map (org-mode-map markdown-mode-map latex-mode-map)
    (:prefix("n" . "note")
    :desc "Bibliographic entries" "b" #'helm-bibtex)))


