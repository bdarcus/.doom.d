;;; bindings.el -*- lexical-binding: t; -*-

(map!
 :leader
 :map biblio-actions-map
 :prefix ("B" . "biblio")
 :desc "Insert bibtex"    "b" #'bibtex-actions-insert-bibtex
 :desc "Insert citation"  "c" #'bibtex-actions-insert-citation
 :desc "Insert citekey"   "k" #'bibtex-actions-insert-key
 :desc "Open pdf or link" "o" #'bibtex-actions-open
 :desc "Open link"        "l" #'bibtex-actions-open-link
 :desc "Open pdf"         "p" #'bibtex-actions-open-pdf
 :desc "Open bibtex"      "e" #'bibtex-actions-open-entry
 :desc "Open notes"       "n" #'bibtex-actions-open-notes)
