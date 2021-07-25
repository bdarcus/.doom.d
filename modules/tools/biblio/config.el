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
  :after embark bibtex-completion
  :demand t
  :config
  (add-to-list 'embark-keymap-alist '(bibtex . bibtex-actions-map)))

(use-package! citeproc
  :defer t)

;;; Org-Cite configuration

(use-package! oc
  :after org
  :init
  (lambda ()
    (let ((paths bibtex-completion-bibliography))
    ;; Always return bibliography PATHS as list for org-cite.
      (if (stringp paths) (list paths)) paths))

  (setq org-cite-global-bibliography
        (+biblio-bibliography-paths bibtex-completion-bibliography))
  :config
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((latex natbib)
          (t csl))))

;;; Org-cite processors

;;;; Core

(use-package! oc-basic
  :after oc)

(use-package! oc-biblatex
  :after oc)

(use-package! oc-csl
  :after oc)

(use-package! oc-natbib
  :after oc)

;;;; Third-party

(use-package! org-ref-cite-insert
  :when (featurep! :completion ivy))

(use-package! org-ref-cite-follow
  ;; REVIEW we really need to turn this feature on to use this.  How?
  :when (featurep! :ui hydra)
  :after oc)

(use-package! org-ref-cite-activate
  ;; Make available everywhere, but document how to change.
  :after oc)

(use-package! oc-bibtex-actions
  :when (featurep! :completion vertico)
  :after (oc bibtex-actions)
  :demand t
  :commands oc-bibtex-actions-select-style
  :config
  (setq org-cite-insert-processor 'oc-bibtex-actions
        org-cite-follow-processor 'oc-bibtex-actions
        org-cite-activate-processor 'org-ref-cite-activate))

