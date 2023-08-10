(require 'autothemer)

(autothemer-deftheme
 my-solarized-light "A solarized light theme with personal mode support"

 ((((class color) (min-colors #xFFFFFF))) ;; Supports 24bit colors only

  (base03    "#002b36") ;; Darkest blue background tone
  (base02    "#073642") ;; Dark blue background tone
  (base01    "#586e75") ;; Darkest grey tone        (bold)
  (base00    "#657b83") ;; Dark grey tone           (foreground)
  (base0     "#839496") ;; Light grey tone
  (base1     "#93a1a1") ;; Lighest grey tone        (accent)
  (base2     "#eee8d5") ;; Light background tone    (alt background)
  (base3     "#fdf6e3") ;; Lightest background tone (background)
  (yellow    "#b58900")
  (orange    "#cb4b16")
  (red       "#dc322f")
  (magenta   "#d33682")
  (violet    "#6c71c4")
  (blue      "#268bd2")
  (cyan      "#2aa198")
  (green     "#859900"))

 ;; Faces
 (
  ;; basics
  (default (:background base3 :foreground base00))

  (cursor              (:background base01 :foreground base3))
  (fringe              (:background base2))
  (highlight           (:background base2))
  (hl-line             (:background base2))
  (link                (:foreground blue :underline t))
  (link-visited        (:foreground magenta :underline t))
  (minibuffer-prompt   (:foreground blue))
  (mode-line           (:background base2))
  (mode-line-inactive  (:background base2))
  (region              (:background base2))
  (secondary-selection (:background base2))
  (shadow              (:foreground base1))
  (window-divider      (:foreground base01))

  ;; Built in syntax
  (font-lock-builtin-face       (:foreground magenta))
  (font-lock-constant-face      (:foreground magenta))
  (font-lock-comment-face       (:foreground base1))
  (font-lock-function-name-face (:foreground yellow))
  (font-lock-keyword-face       (:foreground green))
  (font-lock-string-face        (:foreground cyan))
  (font-lock-number-face        (:foreground base3))
  (font-lock-variable-name-face (:foreground blue))
  (font-lock-type-face          (:foreground yellow))
  (font-lock-property-face      (:foreground blue))
  (font-lock-warning-face       (:foreground yellow))

  ;; Basic faces
  (error               (:foreground red))
  (success             (:foreground green))
  (warning             (:foreground yellow))
  (trailing-whitespace (:background red :foreground base3))
  (escape-glyph        (:foreground cyan))
  (header-line         (:inherit 'mode-line))
  (homoglyph           (:foreground yellow))
  (match               (:foreground green))

  ;;; whitespace-mode
  (whitespace-empty            (:inherit 'default))
  (whitespace-hspace           (:inherit 'default))
  (whitespace-indentation      (:inherit 'default))
  (whitespace-line             (:inherit 'default))
  (whitespace-newline          (:inherit 'default))
  (whitespace-space            (:inherit 'default))
  (whitespace-space-after-tab  (:background red :foreground base3))
  (whitespace-space-before-tab (:background red :foreground base3))
  (whitespace-tab              (:inherit 'default))
  (whitespace-trailing         (:background red :foreground base3))

  ;; RainbowDelimiters
  (rainbow-delimiters-depth-1-face   (:foreground red))
  (rainbow-delimiters-depth-2-face   (:foreground orange))
  (rainbow-delimiters-depth-3-face   (:foreground yellow))
  (rainbow-delimiters-depth-4-face   (:foreground green))
  (rainbow-delimiters-depth-5-face   (:foreground blue))
  (rainbow-delimiters-depth-6-face   (:foreground violet))
  (rainbow-delimiters-depth-7-face   (:foreground red))
  (rainbow-delimiters-depth-8-face   (:foreground orange))
  (rainbow-delimiters-depth-9-face   (:foreground yellow))
  (rainbow-delimiters-depth-10-face  (:foreground green))
  (rainbow-delimiters-depth-11-face  (:foreground blue))
  (rainbow-delimiters-depth-12-face  (:foreground violet))
  (rainbow-delimiters-unmatched-face (:background red :foreground base3))

  ;;; line numbers
  (line-number              (:inherit 'mode-line))
  (line-number-current-line (:inherit 'mode-line :bold 't))

  ;; show-paren
  (show-paren-match    (:background green :foreground base3))
  (show-paren-mismatch (:background red :foreground base3))

  ;; avy
  (avy-background-face (:foreground base01))
  (avy-lead-face       (:background red :foreground base3))
  (avy-lead-face-0     (:background green :foreground base3))
  (avy-lead-face-1     (:background yellow :foreground base3))
  (avy-lead-face-2     (:background cyan :foreground base3))

  ;; isearch
  (isearch        (:background blue :foreground base3))
  (lazy-highlight (:background cyan :foreground base3))
  (isearch-fail   (:background red :foreground base3))

  ;; highlight indent guides
  (highlight-indent-guides-character-face (:foreground yellow))
  ))

(provide-theme 'my-solarized-light)
