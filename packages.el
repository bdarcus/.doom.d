;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! origami)
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
; stettberger/ospl-mode
(when (featurep! :completion selectrum)
  (package! bibtex-actions :recipe (:local-repo "~/Code/bibtex-actions")))

; 
(unpin! doom-themes)
;(package! bibtex-completion
; seems there's a bug where doom is ignoring branch, so do this locally 
;  :recipe (:local-repo "~/Code/forks/helm-bibtex"
;           :host github
;           :type git
;           :repo "bdarcus/helm-bibtex"
;           :branch "interactive-bibtex-completion"
;           :files ("bibtex-completion.el")))

;(package! bibtex-actions 
;  :recipe (:host github :repo "bdarcus/bibtex-actions"))
;  :recipe (:local-repo "~/Code/bibtex-actions"))
