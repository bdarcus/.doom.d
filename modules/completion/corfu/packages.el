(package! corfu
  :recipe (:files (:defaults "extensions/*.el")))
(when (modulep! +icons)
  (package! kind-icon))
(when (modulep! +orderless)
  (package! orderless))
(package! cape)
(package! popon
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon"))
(package! corfu-terminal
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
