;;; tools/biblio/config.el -*- lexical-binding: t; -*-

(use-package! bibtex-completion
  :defer t
  :config
  (setq bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-field "file"));; This tell bibtex-completion to look at the File field of the bibtex to figure out which pdf to open

(use-package! ivy-bibtex
  :when (featurep! :completion ivy)
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

(use-package! bibtex-actions
  :when (featurep! :completion vertico)
  :after embark
  :defer t
  :config
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map)))

(use-package! citeproc
  :defer t)

;;; Org-Cite configuration

(use-package! oc
  :after org
  :config
  (setq org-cite-global-bibliography '("~/bib/references.bib"))
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((beamer natbib)
            (latex biblatex)
            (t csl))))

(use-package! oc-bibtex-actions
;  :when (featurep! :completion vertico)
  :after (embark oc bibtex-actions)
  :config
  ;; We need a completing-read front-end, so use bibtex-actions for the
  ;; insert processor.
  ;;
  ;; We could also use bibtex-actions for the follow processor, but let's
  ;; see if we can use org-ref-cite instead.
  (setq org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'org-ref-cite-follow-activate))

  ;;; Org-cite processors
(use-package! oc-basic
  :after oc)

(use-package! oc-biblatex
  :after oc)

(use-package! oc-csl
  :after oc
  :config
  ;; optional; add to docs instead?
  (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
  (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

(use-package! oc-natbib
  :after oc)

(use-package! org-ref-cite
  ;; we only want to load the whole package if we use ivy
  :when (featurep! :completion ivy))

(use-package! org-ref-cite-follow
  :config
  (setq org-cite-follow-processor 'org-ref-cite-follow-activate))

(use-package! org-ref-cite-activate
  :config
  (setq org-cite-activate-processor 'org-ref-cite-follow-activate))

(use-package! org-ref-cite-insert
  :when (featurep! :completion ivy)
  :config
  (setq org-cite-activate-processor 'org-ref-cite-insert))

;; register this processor so we can refer to it outside ivy
;; not sure this is the best place to put it
(org-cite-register-processor 'org-ref-cite-follow-activate
  :follow  #'org-ref-cite-follow
  :activate  #'org-ref-cite-activate)
