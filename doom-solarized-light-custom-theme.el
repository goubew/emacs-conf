;;; doom-solarized-light-custom-theme.el --- a custom light variant of Solarized -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: Oct 31, 2022
;; Author: goubew <https://github.com/goubew>
;; Maintainer:
;; Source: https://github.com/bbatsov/solarized-emacs
;; Source: https://ethanschoonover.com/solarized
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-solarized-light-custom-theme nil
  "Options for the `doom-solarized-light' theme."
  :group 'doom-themes)

(defcustom doom-solarized-light-custom-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-solarized-light-custom-theme
  :type 'boolean)

(defcustom doom-solarized-light-custom-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-solarized-light-custom-theme
  :type 'boolean)

(defcustom doom-solarized-light-custom-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-solarized-light-custom-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-solarized-light-custom
  "A custom light theme inspired by Solarized light"

  ;; name        default   256       16
  ((bg         '("#FDF6E3" "#FDF6E3" "white"        ))
   (fg         '("#657B83" "#657B83" "black"        ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#EEE8D5" "#EEE8D5" "white"        ))
   (fg-alt     '("#586E75" "#586E75" "brightwhite"  ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#FDF6E3" "#FDF6E3" "white"        ))
   (base1      '("#FDF6E3" "#FDF6E3" "brightblack"  ))
   (base2      '("#EEE8D5" "#EEE8D5" "brightblack"  ))
   (base3      '("#EEE8D5" "#EEE8D5" "brightblack"  ))
   (base4      '("#93A1A1" "#93A1A1" "brightblack"  ))
   (base5      '("#839496" "#839496" "brightblack"  ))
   (base6      '("#657B83" "#657B83" "brightblack"  ))
   (base7      '("#586E75" "#586E75" "brightblack"  ))
   (base8      '("#586E75" "#586E75" "black"        ))

   (grey       base4)
   (red        '("#DC322F" "#DC322F" "red"          ))
   (orange     '("#CB4B16" "#CB4B16" "brightred"    ))
   (green      '("#859900" "#859900" "green"        ))
   (teal       '("#859900" "#859900" "brightgreen"  ))
   (yellow     '("#B58900" "#B58900" "yellow"       ))
   (blue       '("#268BD2" "#268BD2" "brightblue"   ))
   (dark-blue  '("#268BD2" "#268BD2" "blue"         ))
   (magenta    '("#D33682" "#D33682" "magenta"      ))
   (violet     '("#6C71C4" "#6C71C4" "brightmagenta"))
   (cyan       '("#2AA198" "#2AA198" "brightcyan"   ))
   (dark-cyan  '("#2AA198" "#2AA198" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base4)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base4)
   (doc-comments   base4)
   (constants      violet)
   (functions      magenta)
   (keywords       green)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        cyan)
   (variables      blue)
   (numbers        violet)
   (region         bg-alt)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-solarized-light-custom-brighter-modeline)
   (-modeline-pad
    (when doom-solarized-light-custom-padded-modeline
      (if (integerp doom-solarized-light-custom-padded-modeline) doom-solarized-light-custom-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base6)

   (modeline-bg
    (if -modeline-bright
        (doom-lighten bg 0.7)
      (doom-darken bg 0.05)))
   (modeline-bg-alt
    (if -modeline-bright
        (doom-lighten bg 0.7)
      (doom-lighten base3 0.2)))
   (modeline-bg-inactive     (doom-darken bg 0.025))
   (modeline-bg-inactive-alt (doom-darken bg 0.02)))


  ;;;; Base theme face overrides
  ((cursor  :background base8)
   ((font-lock-comment-face &override)
    :slant 'italic
    :background (if doom-solarized-light-custom-brighter-comments
                    (doom-blend teal base0 0.07)))
   ((font-lock-type-face &override) :slant 'italic)
   ((font-lock-builtin-face &override) :slant 'italic)
   ((font-lock-function-name-face &override) :foreground type)
   (fringe  :inherit 'default :foreground base4 :background base3)
   (hl-line :background base3)
   ((line-number &override) :foreground base4 :background base3)
   ((line-number-current-line &override) :foreground fg :background bg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-evil-emacs-state  :foreground magenta)
   (doom-modeline-evil-insert-state :foreground blue)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; lsp-ui
   (lsp-ui-sideline-code-action :foreground blue)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; ivy
   (ivy-current-match :background (doom-lighten yellow 0.65) :distant-foreground fg)
   (ivy-minibuffer-match-face-1 :foreground blue :background base3)
   (ivy-minibuffer-match-face-2 :foreground magenta :background base3)
   (ivy-minibuffer-match-face-3 :foreground green   :background base3)
   (ivy-minibuffer-match-face-4 :foreground yellow  :background base3)
   (ivy-minibuffer-match-highlight :foreground violet)
   ;;;; ivy-posframe
   (ivy-posframe :background modeline-bg-alt)
   ;;;; swiper
   (swiper-match-face-1 :inherit 'ivy-minibuffer-match-face-1)
   (swiper-match-face-2 :inherit 'ivy-minibuffer-match-face-2)
   (swiper-match-face-3 :inherit 'ivy-minibuffer-match-face-3)
   (swiper-match-face-4 :inherit 'ivy-minibuffer-match-face-4)
   ;;;; helm
   (helm-selection :foreground base0 :background blue)
   ;;;; company
   (company-tooltip-selection :background blue :foreground base3)
   ;;;; org <built-in>
   (org-block :background (doom-blend yellow bg 0.04) :extend t)
   (org-block-background :background (doom-blend yellow bg 0.04))
   (org-block-begin-line :background (doom-blend yellow bg 0.08) :extend t)
   (org-block-end-line :background (doom-blend yellow bg 0.08) :extend t)
   ;;;; widget
   (widget-field :foreground fg :background base3)
   (widget-single-line-field :foreground fg :background base3)
   ;;;; latex
   (font-latex-sedate-face :foreground base6)
   ;;;; notmuch
   (notmuch-message-summary-face :foreground teal)
   (notmuch-wash-cited-text :foreground base6))

  ;;;; Base theme variable overrides-
  ;; ()
  )

;;; doom-solarized-light-custom-theme.el ends here
