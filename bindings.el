;;; bindings.el -*- lexical-binding: t; -*-

(map!
 :leader
 :map bibtex-actions-map ; I don't understand what this line does exactly
 :prefix ("B" . "biblio")
 :desc "Insert bibtex"         "b" '("Insert bibtex"       . bibtex-actions-insert-bibtex)
 :desc "Insert citation"       "c" '("Insert citation"     . bibtex-actions-insert-citation)
 :desc "Insert citekey"        "k" '("Insert citekey"      . bibtex-actions-insert-key)
 :desc "Open pdf or link"      "o" '("Open pdf or link"    . bibtex-actions-open)
 :desc "Open link"             "l" '("Open link"           . bibtex-actions-open-link)
 :desc "Open pdf"              "p" '("Open pdf"            . bibtex-actions-open-pdf)
 :desc "Open bibtex entries"   "e" '("Open bibtex entries" . bibtex-actions-open-entry)
 :desc "Open notes"            "n" '("Open notes"          . bibtex-actions-open-notes))
