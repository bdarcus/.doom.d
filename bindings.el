;;; ~/.doom.d/bindings.el -*- lexical-binding: t; -*-

; basic org-roam keybindings
(map!
 :map (org-mode-map)
 :localleader
 (:prefix ("R" . "org-roam")
   "r" #'org-roam
   "f" #'org-roam-find-file
   "i" #'org-roam-insert
   "g" #'org-roam-show-graph))

; adding a command to bring up ivy-bibtex for citation insertion
(map! 
 :map (markdown-mode-map)
 :localleader
 :desc "insert citation" "ic" #'ivy-bibtex)
