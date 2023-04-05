;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;(package! org-roam-bibtex)

;(package! org-mode :recipe (:local-repo "~/Code/org-mode"))
(package! websocket)
(package! org-roam-ui)

(package! org-glossary
  :recipe (:host github :repo "tecosaur/org-glossary"))

(package! org-modern)
;(package! spdx)
(package! biblio)
(package! pandoc-mode)
(package! pretty-hydra)
;(package! origami)
(package! rnc-mode)

;(package! org-roam
;  :recipe (:host github 
;           :repo "org-roam/org-roam"
;           :branch "cite")
;  :pin "70fd4bde730dcccf334259d91030171371aba9a0")

;; $DOOMDIR/packages.el
(package! org-glossary
  :recipe (:host github :repo "tecosaur/org-glossary"))

(package! org-sidebar)

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

;(package! citar-capf :recipe (:host github :repo "mclear-tools/citar-capf"))
(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))
(package! bookmark-view :recipe (:host github :repo "minad/bookmark-view"))

;(package! hercules)

(package! origami)

;(package! org-re-reveal)

(package! keycast :pin "a3a0798349adf3e33277091fa8dee63173b68edf")
(package! gif-screencast :pin "fa81e915c256271fa10b807a2935d5eaa4700dff")

;; use fork that include vertico-crm
(package! vertico 
  :recipe (:host github :repo "bdarcus/vertico" :files ("*.el" "extensions/*.el")) 
   :pin "034ee0c86fb61220f54d5ef76a49655135f97cff")
;(unpin! vertico)
(package! citar
  :recipe (:local-repo "~/Code/citar"))
(unpin! citar-embark)

(package! svg-lib
  :recipe (:host github :repo "rougier/svg-lib"))
(package! svg-tag-mode)

;(package! meow)

(package! emacs-svg-icon :recipe (:host github :repo "rougier/emacs-svg-icon"))
(package! oc-csl :recipe (:host github :repo "andras-simonyi/oc-csl-ns" :files ("*.el")))
;(package! oer/org-re-reveal-citeproc)
(package! org-ref :recipe (:host github 
                           :repo "jkitchin/org-ref" 
                           :files ("org-ref-ref-links.el" "org-ref-refproc.el" "org-ref-citation-links.el")))

(package! oxr :recipe (:host github :repo "bdarcus/oxr"))
;(package! embark :recipe (:host github :repo "oantolin/embark") :pin "1842d6b182c4fe7b6aa4d06f4957fbeabaa96de3")
;(package! embark
;  :recipe (:local-repo "~/Code/embark"))
