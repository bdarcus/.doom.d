;;; ~/.doom.d/bindings.el -*- lexical-binding: t; -*-


; adding a command to bring up ivy-bibtex for citation insertion
(map! 
 :map (markdown-mode-map)
 :localleader
 :desc "insert citation" "ic" #'ivy-bibtex)
