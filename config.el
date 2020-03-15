;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(load! "bindings")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bruce D'Arcus"
      user-mail-address "bdarcus@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))
;(setq doom-font (font-spec :family "Attribute Mono" :size 14))
;(unless (find-font doom-font)
;  (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 14)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(use-package! org-ref
  :after org
  :init)

(setq deft-extensions '("txt" "tex" "org" "md"))
(setq deft-recursive t)
(setq deft-directory "~/org")


; TODO this is now working, but it's placing screenshots in ../.attach, which I don't want
(setq-default org-download-image-dir "~/org/roam/images/"
              org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s"
              org-download-method 'directory)

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/org/roam")
  (map! :leader
        :prefix "n"
        :desc "Org-Roam-Insert" "i" #'org-roam-insert
        :desc "Org-Roam-Find"   "/" #'org-roam-find-file
        :desc "Org-Roam-Show-Graph" "g" #'org-roam-show-graph
        :desc "Org-Roam-Buffer" "r" #'org-roam)
  :config
  (org-roam-mode +1))

(add-hook 'org-roam-backlinks-mode-hook (lambda () (flyspell-mode -1)))

(after! (company org-roam)
  (set-company-backend! 'org-mode
    '(company-org-roam :with company-dabbrev :with company-yasnippet)))

(setq bibtex-completion-bibliography
      '("~/org/bib/academic.bib"
        "~/org/bib/me.bib"))

(setq bibtex-dialect 'biblatex)

(setq bibtex-completion-format-citation-functions
      '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))

(setq bibtex-completion-notes-path "~/org/roam/biblio")

(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

(setq markdown-command "pandoc")

;; save session after 10s of idle time
;; https://emacs.stackexchange.com/questions/46963/how-i-can-make-persp-mode-save-my-workspace-on-every-change
                                        ;(run-with-idle-timer 10 t #'doom-save-session)

                                        ; add pandoc-mode, though hopefully a module will get added to doom in time
                                        ; https://github.com/Guillawme/doom.d

;; TODO confirm this is correct
;(use-package! pandoc-mode
;  :config
;  (setq pandoc-data-dir (concat doom-cache-dir "pandoc-mode"))
;  :hook
;  (;; Activate pandoc-mode on top of common text modes.
;   (org-mode markdown-mode text-mode)
   ;; Load default settings file `default.pandoc' in `pandoc-data-dir'.
;   (pandoc-mode . pandoc-load-default-settings))


(setq projectile-project-search-path '("~/Projects/" "~/org"))

;; based on org-chef documentation, adapted for doom
;(after! org (add-to-list 'org-capture-templates
;      '("c" "Cookbook" entry (file "~/org/cookbook.org")
;         "%(org-chef-get-recipe-from-url)"
;         :empty-lines 1)))


;(use-package! golden-ratio
;  :after-call pre-command-hook
;  :config
;  (golden-ratio-mode +1)
  ;; Using this hook for resizing windows is less precise than
  ;; `doom-switch-window-hook'.
;  (remove-hook 'window-configuration-change-hook #'golden-ratio)
;  (add-hook 'doom-switch-window-hook #'golden-ratio))


(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups  '((:name "Today"
                                          :time-grid t
                                          :scheduled today)
                                   (:name "Due today"
                                          :deadline today)
                                   (:name "Important"
                                          :priority "A")
                                   (:name "Overdue"
                                          :deadline past)
                                   (:name "Due soon"
                                          :deadline future)
                                   (:name "Big Outcomes"
                                          :tag "bo")))
  :config
  (org-super-agenda-mode))

(after! org
  (set-popup-rule! "^ \\*Org tags" :side 'bottom :size 0.80 :select t :ttl nil)

  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (setq org-agenda-skip-scheduled-if-done t
        org-log-done 'time
        org-priority-faces '((65 :foreground "#e45649")
                             (66 :foreground "#da8548")
                             (67 :foreground "#0098dd"))
        org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(s)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
          ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
          ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
          ("DONE" :foreground "#50a14f" :weight normal :underline t)
          ("CANCELLED" :foreground "#ff6480" :weight normal :underline t))
        org-agenda-files (list "~/org/TODOs.org")
        )
)

(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("◼" "◼" "◼")))

(setq flyspell-correct-popup t)

(setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*")

(setq org-html-html5-fancy t)
(setq org-display-inline-images t)

; TODO Is this loading correctly? Should work in any text mode, including org.
; TODO Ideally, I also want this incorporated into org visibility cycling.
(use-package! origami
  :commands (origami-toggle-node origami-toggle-all-nodes)
  :hook (text-mode . origami-mode)
  :init
  :config
  (map! :leader
        :prefix "t"
        :desc "Origami-Toggle All Nodes" "O" #'origami-toggle-all-nodes
        :desc "Origami-Toggle Node" "o" #'origami-toggle-node)
  )

(setq org-latex-compiler "lualatex")
