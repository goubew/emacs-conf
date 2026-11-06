;; -*- lexical-binding: t; -*-
(require 'my-simple-theme-base)

(my-simple-theme-deftheme
 my-charcoal-light
 "A charcoal colour theme"

 ((((class color) (min-colors #xFFFFFF))) ; col 2 Xterm/256

  (bg3 "#bcab85")
  (bg2 "#d2c4a4")
  (bg1 "#e7ddc4")
  (bg "#fdf6e3") ;; Default bg

  (fg2 "#2e2412")
  (fg1 "#2e2412")
  (fg "#2e2412") ;; Default fg
  (fg0 "#695a40") ;; Color accent

  (bgred "#fbcfca")
  (bggreen "#cecfa4")
  (bgyellow "#fae3af")
  (bgblue "#bbbfbd")
  (bgpurple "#d3b6be")
  (bgaqua "#b1c0ac")
  (bgorange "#fedbbe")

  (fgred "#9d0006")
  (fggreen "#79740e")
  (fgyellow "#b57614")
  (fgblue "#076678")
  (fgpurple "#8f3f71")
  (fgaqua "#427b58")
  (fgorange "#af3a03")
  ))
