;;; bindings.el -*- lexical-binding: t; -*-

(map!
 :leader
 :prefix ("B" . "biblio")
 :desc "Search entries"        "s" #'bibtex-actions-insert-key
 :desc "Insert bibtex"         "b" #'bibtex-actions-insert-bibtex
 :desc "Insert citation"       "c" #'bibtex-actions-insert-citation
 :desc "Insert citekey"        "k" #'bibtex-actions-insert-key
 :desc "Open pdf or link"      "o" #'bibtex-actions-open
 :desc "Open link"             "l" #'bibtex-actions-open-link
 :desc "Open pdf"              "p" #'bibtex-actions-open-pdf
 :desc "Open bibtex entries"   "e" #'bibtex-actions-open-entry
 :desc "Open notes"            "n" #'bibtex-actions-open-notes)
