;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(when (featurep! :completion ivy)
  (package! ivy-bibtex)))
(when (featurep! :completion helm)
  (package! helm-bibtex)))
