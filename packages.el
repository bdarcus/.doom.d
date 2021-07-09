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
(package! citeproc)
(package! package-lint)
(package! flycheck-aspell)
(unpin! bibtex-actions)
(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))
(package! bookmark-view :recipe (:host github :repo "minad/bookmark-view"))

(unpin! doom-themes)

(package! origami)

;(package! org-re-reveal)

(package! keycast :pin "a3a0798349adf3e33277091fa8dee63173b68edf")
(package! gif-screencast :pin "fa81e915c256271fa10b807a2935d5eaa4700dff")

;; use fork that include vertico-crm
(package! vertico :recipe (:host github :repo "bdarcus/vertico"))
(unpin! vertico)
;(package! bibtex-actions 
;  :recipe (:host github
;           :repo "bdarcus/bibtex-actions"))
;  :recipe (:local-repo "~/Code/bibtex-actions"))
