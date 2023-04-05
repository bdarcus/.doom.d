;;; bindings.el -*- lexical-binding: t; -*-

(map!
 :leader
 :map citar-map ; I don't understand what this line does exactly
 :prefix ("B" . "biblio")
 :desc "Insert bibtex"         "b" '("Insert bibtex"       . citar-insert-bibtex)
 :desc "Insert citation"       "c" '("Insert citation"     . citar-insert-citation)
 :desc "Insert citekey"        "k" '("Insert citekey"      . citar-insert-key)
 :desc "Open pdf or link"      "o" '("Open pdf or link"    . citar-open)
 :desc "Open link"             "l" '("Open link"           . citar-open-link)
 :desc "Open pdf"              "p" '("Open pdf"            . citar-open-pdf)
 :desc "Open bibtex entries"   "e" '("Open bibtex entries" . citar-open-entry)
 :desc "Open notes"            "n" '("Open notes"          . citar-open-notes))
