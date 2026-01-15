(require 'my-simple-theme-base)

(my-simple-theme-deftheme
 my-gruvbox-mono-light
 "A mono gruvbox colour theme"

 ((((class color) (min-colors #xFFFFFF))) ; col 2 Xterm/256

  (bg3 "#bdae93")
  (bg2 "#d5c4a1")
  (bg1 "#ebdbb2")
  (bg "#f2e5bc") ;; Default bg

  (fg2 "#150f05")
  (fg1 "#2e2412")
  (fg "#4a3c25") ;; Default fg
  (fg0 "#695a40")
  ))
