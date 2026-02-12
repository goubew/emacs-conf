(require 'my-simple-theme-base)

(my-simple-theme-deftheme
 my-charcoal-light
 "A charcoal colour theme"

 ((((class color) (min-colors #xFFFFFF))) ; col 2 Xterm/256

  (bg3 "#ad9c74")
  (bg2 "#bcab85")
  (bg1 "#c9ba96")
  (bg "#daceb0") ;; Default bg

  (fg2 "#150f05")
  (fg1 "#2e2412")
  (fg "#4a3c25") ;; Default fg
  (fg0 "#695a40")
  ))
