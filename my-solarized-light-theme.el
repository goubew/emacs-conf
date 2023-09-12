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
  (region              (:background violet :foreground base3))
  (secondary-selection (:background base2))
  (shadow              (:foreground base1))
  (window-divider      (:foreground base01))

  ;; Built in syntax
  (font-lock-builtin-face       (:foreground violet))
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

  ;; git-gutter
  (git-gutter:modified (:background base2 :foreground green))
  (git-gutter:added    (:background base2 :foreground green))
  (git-gutter:deleted  (:background base2 :foreground red))

  ;; flyspell
  (flyspell-duplicate (:underline (:color orange :style 'wave)))
  (flyspell-incorrect (:underline (:color red :style 'wave)))

  ;; dired
  (dired-broken-symlink (:background red :foreground base3))

  ;; eshell
  (eshell-prompt        (:foreground green))
  (eshell-ls-backup     (:foreground base00))
  (eshell-ls-archive    (:foreground base00))
  (eshell-ls-clutter    (:foreground base00))
  (eshell-ls-missing    (:foreground base00))
  (eshell-ls-product    (:foreground base00))
  (eshell-ls-special    (:foreground base00))
  (eshell-ls-symlink    (:foreground green))
  (eshell-ls-readonly   (:foreground base00))
  (eshell-ls-directory  (:foreground orange))
  (eshell-ls-executable (:foreground violet))
  (eshell-ls-unreadable (:foreground base00))

  ;; tab bar
  (tab-bar                    (:background base2 :foreground base00))
  (tab-bar-tab                (:foreground base01 :weight 'bold))
  (tab-bar-tab-inactive       (:foreground base00))
  (tab-bar-tab-ungrouped      (:foreground base00))
  (tab-bar-tab-group-current  (:foreground base2 :weight 'bold))
  (tab-bar-tab-group-inactive (:foreground base00))

  ;; wgrep
  (wgrep-reject-face (:foreground red))
  (wgrep-face        (:background green :foreground base3))
  (wgrep-done-face   (:foreground blue))
  (wgrep-file-face   (:background green :foreground base3))
  (wgrep-delete-face (:background red :foreground base3))

  ;; hydra
  (hydra-face-red (:foreground red))
  (hydra-face-blue (:foreground blue))
  (hydra-face-amaranth (:foreground magenta))
  (hydra-face-pink (:foreground cyan))
  (hydra-face-teal (:foreground orange))

  ;; magit
  (magit-blame-highlight             (:extend t :foreground base2 :background base01))
  (magit-reflog-other                (:foreground cyan))
  (magit-reflog-remote               (:foreground cyan))
  (magit-reflog-cherry-pick          (:foreground green))
  (magit-reflog-rebase               (:foreground magenta))
  (magit-reflog-reset                (:foreground red))
  (magit-reflog-checkout             (:foreground blue))
  (magit-reflog-merge                (:foreground green))
  (magit-reflog-amend                (:foreground magenta))
  (magit-reflog-commit               (:foreground green))
  (magit-bisect-bad                  (:foreground red))
  (magit-bisect-skip                 (:foreground yellow))
  (magit-bisect-good                 (:foreground green))
  (magit-sequence-drop               (:foreground red))
  (magit-sequence-head               (:foreground base01))
  (magit-sequence-part               (:foreground yellow))
  (magit-sequence-stop               (:foreground green))
  (magit-cherry-equivalent           (:foreground magenta))
  (magit-cherry-unmatched            (:foreground cyan))
  (magit-signature-error             (:foreground blue))
  (magit-signature-revoked           (:foreground magenta))
  (magit-signature-expired           (:foreground yellow))
  (magit-signature-untrusted         (:foreground cyan))
  (magit-signature-bad               (:weight 'bold :foreground red))
  (magit-signature-good              (:foreground green))
  (magit-refname-pullreq             (:foreground base01))
  (magit-branch-upstream             (:slant 'italic))
  (magit-branch-local                (:foreground base01))
  (magit-branch-remote               (:foreground green))
  (magit-tag                         (:foreground yellow))
  (magit-hash                        (:foreground base1))
  (magit-refname                     (:foreground yellow))
  (magit-log-date                    (:foreground yellow))
  (magit-log-author                  (:foreground red))
  (magit-log-graph                   (:foreground base01))
  (magit-diffstat-removed            (:foreground red))
  (magit-diffstat-added              (:foreground green))
  (magit-diff-context-highlight      (:background base2 :foreground base00))
  (magit-diff-base-highlight         (:background yellow :foreground base3))
  (magit-diff-removed-highlight      (:background red :foreground base3))
  (magit-diff-added-highlight        (:background green :foreground base3))
  (magit-diff-context                (:foreground base01))
  (magit-diff-base                   (:background yellow :foreground base3))
  (magit-diff-removed                (:background red :foreground base3))
  (magit-diff-added                  (:background green :foreground base3))
  (magit-diff-hunk-heading-highlight (:background base2 :foreground base01 :weight 'bold))
  (magit-diff-hunk-heading           (:background base2 :foreground base01))
  (magit-section-heading-selection   (:foreground orange))
  (magit-section-heading             (:weight 'bold :foreground orange))
  (magit-section-highlight           (:background base2))
  (magit-dimmed                      (:foreground base1))

  ;; orderless
  (orderless-match-face-0 (:foreground blue))
  (orderless-match-face-1 (:foreground violet))
  (orderless-match-face-2 (:foreground green))
  (orderless-match-face-3 (:foreground yellow))

  ;; ansi
  (ansi-color-black          (:foreground base01 :background base01))
  (ansi-color-red            (:foreground red :background red))
  (ansi-color-green          (:foreground green :background green))
  (ansi-color-yellow         (:foreground yellow :background yellow))
  (ansi-color-blue           (:foreground blue :background blue))
  (ansi-color-magenta        (:foreground magenta :background magenta))
  (ansi-color-cyan           (:foreground cyan :background cyan))
  (ansi-color-white          (:foreground base1 :background base1))
  (ansi-color-bright-black   (:foreground base01 :background base01))
  (ansi-color-bright-red     (:foreground red :background red))
  (ansi-color-bright-green   (:foreground green :background green))
  (ansi-color-bright-yellow  (:foreground yellow :background yellow))
  (ansi-color-bright-blue    (:foreground blue :background blue))
  (ansi-color-bright-magenta (:foreground magenta :background magenta))
  (ansi-color-bright-cyan    (:foreground cyan :background cyan))
  (ansi-color-bright-white   (:foreground base2 :background base2))

  ;; org
  (org-table (:foreground blue))

  ;; eglot
  (eglot-inlay-hint-face (:foreground base1 :background base2 :height 0.8))
))

(provide-theme 'my-solarized-light)
