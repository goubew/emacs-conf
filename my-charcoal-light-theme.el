(require 'my-simple-theme-base)

(my-simple-theme-deftheme
 my-charcoal-light
 "A charcoal colour theme"

 ((((class color) (min-colors #xFFFFFF))) ; col 2 Xterm/256

  (bg3 "#9f8f69")
  (bg2 "#ad9c74")
  (bg1 "#bcab85")
  (bg "#c9ba96") ;; Default bg

  (fg2 "#150f05")
  (fg1 "#2e2412")
  (fg "#4a3c25") ;; Default fg
  (fg0 "#695a40")
  ))
