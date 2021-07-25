;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "12079bb09f203dda5cc2dd003bd60a6ad490f762")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "12079bb09f203dda5cc2dd003bd60a6ad490f762"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "12079bb09f203dda5cc2dd003bd60a6ad490f762"))
(when (featurep! :completion vertico)
  (package! bibtex-actions :pin "1119f98d5d31dc715957b6c52e6b51d59122f601"))

(package! org-ref-cite :recipe
  (:host github
   :repo "jkitchin/org-ref-cite"
   ;; exclude the main file with package-requires line, since we don't
   ;; need it, and don't want it forcing installation of ivy, etc.
   :files ("*.el" (:exclude "org-ref-cite.el"))) :pin "c1347c48a20134c3aa498297719c0a4e9a340671")

(package! citeproc :pin "34f6311058aeb0ff36ae9f3c4920fb51fae6abff")
