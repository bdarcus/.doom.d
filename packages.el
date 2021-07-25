;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;(package! org-roam-bibtex)

;(package! org-mode :recipe (:local-repo "~/Code/org-mode"))

(package! biblio)

(package! pretty-hydra)
;(package! origami)
(package! rnc-mode)

;(package! org-roam
;  :recipe (:host github 
;           :repo "org-roam/org-roam"
;           :branch "cite")
;  :pin "70fd4bde730dcccf334259d91030171371aba9a0")

(unpin! org)
(unpin! magit forge)
(unpin! parsebib)
(unpin! org-roam consult consult-flycheck)
(unpin! org-roam-ui)


(package! pdf-tools)
(package! consult-dir)
;(package! org-roam-server)
;; this is what I need; display like auto-fill, but allows
;; one-line-per-sentence
(package! visual-fill-column)
(package! writeroom-mode)
(package! editorconfig)
(package! auto-complete-nxml)
(package! package-lint)
(package! flycheck-aspell)
;(unpin! bibtex-actions)
;(unpin! bibtex-completion)

(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))
(package! bookmark-view :recipe (:host github :repo "minad/bookmark-view"))

;(package! hercules)

(unpin! doom-themes)
(package! origami)

;(package! org-re-reveal)

(package! keycast :pin "a3a0798349adf3e33277091fa8dee63173b68edf")
(package! gif-screencast :pin "fa81e915c256271fa10b807a2935d5eaa4700dff")

;; use fork that include vertico-crm
(package! vertico 
  :recipe (:host github :repo "bdarcus/vertico" :files ("*.el" "extensions/*.el")) 
   :pin "8419a06c8f29698b9584ab55e452b3d31b2d969d")
;  :pin "cd6f85a600036ea3b797cd8d1734c3c6feafcd6b")
;(unpin! vertico)
(package! citar
;  :recipe (:host github
;           :repo "bdarcus/bibtex-actions"))
  :recipe (:local-repo "~/Code/citar" :files ("*.el")))
;(package! citeproc :recipe (:local-repo "~/Code/citeproc-el"))
;(package! org-ref-cite :recipe (:local-repo "~/Code/org-ref-cite"))

;(package! meow)

(package! emacs-svg-icon :recipe (:host github :repo "rougier/emacs-svg-icon"))
(package! oc-csl :recipe (:host github :repo "andras-simonyi/oc-csl-ns" :files ("*.el")))
;(package! oer/org-re-reveal-citeproc)
(package! org-ref :recipe (:host github 
                           :repo "jkitchin/org-ref" 
                           :files ("org-ref-ref-links.el" "org-ref-refproc.el")))

(package! oxr :recipe (:host github :repo "bdarcus/oxr"))
(package! embark 
  :recipe (:local-repo "~/Code/embark"))
