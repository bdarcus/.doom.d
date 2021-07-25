;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el


(package! pretty-hydra)
;(package! origami)
(package! rnc-mode)
(package! org-roam)
;  :recipe (:host github :repo "org-roam/org-roam"))
(unpin! selectrum consult consult-flycheck)
;(package! org-roam-server)
;; this is what I need; display like auto-fill, but allows
;; one-line-per-sentence
(package! visual-fill-column)
(package! writeroom-mode)
(package! editorconfig)
(package! auto-complete-nxml)
(unpin! citeproc)
(package! package-lint)
(package! flycheck-aspell)
;(unpin! bibtex-actions)
(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))
(package! bookmark-view :recipe (:host github :repo "minad/bookmark-view"))

(package! hercules)

(unpin! org-mode doom-themes)
(package! embark :pin "9d56be162badbbfee405595f2ebdfe16a5bca47d")
(package! origami)

;(package! org-re-reveal)

(package! keycast :pin "a3a0798349adf3e33277091fa8dee63173b68edf")
(package! gif-screencast :pin "fa81e915c256271fa10b807a2935d5eaa4700dff")

;; use fork that include vertico-crm
(package! vertico 
  :recipe (:host github :repo "bdarcus/vertico" :files ("*.el" "extensions/*.el")) 
  :pin "cd6f85a600036ea3b797cd8d1734c3c6feafcd6b")
;(unpin! vertico)
(package! bibtex-actions 
;  :recipe (:host github
;           :repo "bdarcus/bibtex-actions"))
  :recipe (:local-repo "~/Code/bibtex-actions"))
