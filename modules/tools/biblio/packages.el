;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "ca09076c3d6e51cc4ffe208c8176fccf3710fcc6"))
(when (featurep! :completion selectrum)
  (package! bibtex-actions
    :recipe (:branch "main")
    :pin "2352657114190906c3d9deba6cd1ea489821fb44"))
(when (featurep! :lang org)
  (package! org-ref :pin "7b0ebdd2e34b3a509c7f043cb9d919a0069491c2"))
(when (featurep! :lang org +roam)
  (package! org-roam-bibtex :pin "ffe0cdbcfd7421c10268fb3437fc5bd6ceadcbd2"))
