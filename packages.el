;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;(package! origami)
(package! rnc-mode)
(package! org-roam 
  :recipe (:host github :repo "org-roam/org-roam" :branch "v2"))
(unpin! selectrum embark consult consult-flycheck)
;(package! org-roam-server)
;; this is what I need; display like auto-fill, but allows
;; one-line-per-sentence
(package! visual-fill-column)
(package! writeroom-mode)
(package! editorconfig)
(package! auto-complete-nxml)
(package! citeproc-org)
(package! package-lint)
(package! flycheck-aspell)

(unpin! doom-themes)

(package! emacs-origami
  :recipe (:host github
           :repo "emacs-origami/origami.el"))

;(package! bibtex-actions 
;  :recipe (:host github
;           :repo "bdarcus/bibtex-actions"))
;  :recipe (:local-repo "~/Code/bibtex-actions"))
