;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
;(when (featurep! :completion selectrum)
;  (package! bibtex-actions :recipe (:host github :repo bdarcus/bibtex-actions) :pin "446c4a71e302be6c7a86cb2e66e127de705ba253")

(when (featurep! :lang org)
  (package! org-ref :pin "7b0ebdd2e34b3a509c7f043cb9d919a0069491c2"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "ffe0cdbcfd7421c10268fb3437fc5bd6ceadcbd2"))
