;;; bindings.el -*- lexical-binding: t; -*-

(map! :after citar
      :map citar-map ; I don't understand what this line does exactly
      :localleader
      :prefix ("B" . "biblio")
      :desc "Insert bibtex"         "b" '("Insert bibtex"       . citar-insert-bibtex)
      :desc "Insert citation"       "c" '("Insert citation"     . citar-insert-citation)
      :desc "Insert citekey"        "k" '("Insert citekey"      . citar-insert-key)
      :desc "Open related"          "o" '("Open related"        . citar-open)
      :desc "Open link"             "l" '("Open link"           . citar-open-link)
      :desc "Open file"             "p" '("Open file"           . citar-open-files)
      :desc "Open entry"            "e" '("Open entry"          . citar-open-entry)
      :desc "Open notes"            "n" '("Open notes"          . citar-open-notes))
