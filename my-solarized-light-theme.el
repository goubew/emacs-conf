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

  (cursor              (:background base02))
  (fringe              (:background base2))
  (highlight           (:background base2))
  (hl-line             (:background base2))
  (link                (:foreground blue :underline t))
  (link-visited        (:foreground magenta :underline t))
  (minibuffer-prompt   (:foreground blue))
  (mode-line           (:background base2))
  (mode-line-inactive  (:background base2))
  (region              (:background yellow :foreground base3))
  (secondary-selection (:background base2))
  (shadow              (:foreground base1))

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
  (rainbow-delimiters-depth-1-face   (:foreground orange))
  (rainbow-delimiters-depth-2-face   (:foreground yellow))
  (rainbow-delimiters-depth-3-face   (:foreground green))
  (rainbow-delimiters-depth-4-face   (:foreground blue))
  (rainbow-delimiters-depth-5-face   (:foreground violet))
  (rainbow-delimiters-depth-6-face   (:foreground orange))
  (rainbow-delimiters-depth-7-face   (:foreground yellow))
  (rainbow-delimiters-depth-8-face   (:foreground green))
  (rainbow-delimiters-depth-9-face   (:foreground blue))
  (rainbow-delimiters-depth-10-face  (:foreground violet))
  (rainbow-delimiters-depth-11-face  (:foreground orange))
  (rainbow-delimiters-depth-12-face  (:foreground yellow))
  (rainbow-delimiters-unmatched-face (:background red :foreground base3))

  ;;; line numbers
  (line-number              (:inherit 'mode-line))
  (line-number-current-line (:inherit 'mode-line :bold 't))

  ;; show-paren
  (show-paren-match    (:underline t))
  (show-paren-mismatch (:background red :foreground base3))

  ;; ace-window
  (aw-key-face                     (:inherit 'font-lock-builtin-face))
  (aw-mode-line-face               (:inherit 'mode-line-buffer-id))
  (aw-background-face              (:foreground base1))
  (aw-minibuffer-leading-char-face (:inherit 'aw-leading-char-face))
  (aw-leading-char-face            (:background orange :foreground base3))

  ;; avy
  (avy-background-face (:foreground base01))
  (avy-lead-face       (:background red :foreground base3))
  (avy-lead-face-0     (:background green :foreground base3))
  (avy-lead-face-1     (:background yellow :foreground base3))
  (avy-lead-face-2     (:background cyan :foreground base3))

  ;; circe
  (circe-fool-face               (:foreground base01))
  (circe-topic-diff-removed-face (:inherit 'diff-removed))
  (circe-topic-diff-new-face     (:inherit 'diff-added))
  (circe-originator-face         nil)
  (circe-my-message-face         nil)
  (circe-highlight-nick-face     (:weight 'bold :foreground violet))
  (circe-server-face             (:foreground blue))
  (circe-prompt-face             (:weight 'bold :foreground cyan :background base2))


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

  ;; diff-hl
  (diff-hl-change (:foreground blue :background blue))
  (diff-hl-delete (:foreground red :background red))
  (diff-hl-insert (:foreground green :background green))

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
  (tab-bar-tab                (:foreground cyan :weight 'bold))
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
  (hydra-face-red      (:foreground red))
  (hydra-face-blue     (:foreground blue))
  (hydra-face-amaranth (:foreground magenta))
  (hydra-face-pink     (:foreground cyan))
  (hydra-face-teal     (:foreground orange))

  ;; lui
  (lui-button-face               (:underline t :foreground green))
  (lui-time-stamp-face           (:weight 'bold :foreground violet))
  (lui-irc-colors-monospace-face nil)

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
  (magit-diffstat-removed            (:foreground orange))
  (magit-diffstat-added              (:foreground green))
  (magit-diff-context                (:foreground base01))
  (magit-diff-context-highlight      (:background base2 :foreground base00))
  (magit-diff-base                   (:background yellow :foreground base3))
  (magit-diff-base-highlight         (:background yellow :foreground base3))
  (magit-diff-removed                (:foreground red :background base3))
  (magit-diff-removed-highlight      (:foreground red :background base2))
  (magit-diff-added                  (:foreground green :background base3))
  (magit-diff-added-highlight        (:foreground green :background base2))
  (magit-diff-hunk-heading           (:background base2 :foreground base01 :weight 'bold))
  (magit-diff-hunk-heading-highlight (:background violet :foreground base3 :weight 'bold))
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
  (org-agenda-calendar-event      (:inherit 'default))
  (org-agenda-calendar-sexp       (:inherit 'default))
  (org-agenda-clocking            (:inherit 'secondary-selection))
  (org-agenda-column-dateline     (:inherit 'org-column))
  (org-agenda-current-time        (:inherit 'org-time-grid))
  (org-agenda-date                (:inherit 'org-agenda-structure))
  (org-agenda-date-today          (:weight 'bold :slant 'italic :inherit 'org-agenda-date))
  (org-agenda-date-weekend-today  (:inherit 'org-agenda-date-today))
  (org-agenda-date-weekend        (:weight 'bold :inherit 'org-agenda-date))
  (org-agenda-diary               (:inherit 'default))
  (org-agenda-dimmed-todo-face    (:foreground base00))
  (org-agenda-done                (:foreground green))
  (org-agenda-filter-category     (:inherit 'mode-line))
  (org-agenda-filter-effort       (:inherit 'mode-line))
  (org-agenda-filter-regexp       (:inherit 'mode-line))
  (org-agenda-filter-tags         (:inherit 'mode-line))
  (org-agenda-restriction-lock    (:background cyan :foreground base3))
  (org-agenda-structure-filter    (:inherit  ('org-warning 'org-agenda-structure)))
  (org-agenda-structure           (:foreground blue))
  (org-agenda-structure-secondary (:inherit 'org-agenda-structure))
  (org-archived                   (:inherit 'shadow))
  (org-block-begin-line           (:extend t :inherit 'org-meta-line))
  (org-block-end-line             (:extend t :inherit 'org-block-begin-line))
  (org-block                      (:foreground base00))
  (org-checkbox                   (:inherit 'bold))
  (org-checkbox-statistics-done   (:inherit 'org-done))
  (org-checkbox-statistics-todo   (:inherit 'org-todo))
  (org-cite                       (:inherit 'link))
  (org-cite-key                   (:inherit 'link))
  (org-clock-overlay              (:background base2))
  (org-code                       (:foreground green))
  (org-column-title               (:weight 'bold :underline t :background base2))
  (org-column                     (:weight 'normal :slant 'normal :underline nil :strike-through nil :background base2))
  (org-date-selected              (:inverse-video t :foreground red))
  (org-date                       (:underline t :foreground violet))
  (org-default                    (:inherit 'default))
  (org-dispatcher-highlight       (:weight 'bold :foreground base3 :background blue))
  (org-document-info              (:foreground base01))
  (org-document-info-keyword      (:inherit 'shadow))
  (org-document-title             (:weight 'bold :foreground blue))
  (org-done                       (:weight 'bold :foreground green))
  (org-drawer                     (:foreground blue))
  (org-ellipsis                   (:underline t :foreground yellow))
  (org-footnote                   (:underline t :foreground violet))
  (org-formula                    (:foreground red))
  (org-headline-done              (:foreground base1))
  (org-headline-todo              (:foreground red))
  (org-hide                       (:foreground base3))
  (org-imminent-deadline          (:inherit 'org-warning))
  (org-inline-src-block           (:inherit 'org-block))
  (org-latex-and-related          (:foreground orange))
  (org-level-1                    (:extend nil :inherit 'outline-1))
  (org-level-2                    (:extend nil :inherit 'outline-2))
  (org-level-3                    (:extend nil :inherit 'outline-3))
  (org-level-4                    (:extend nil :inherit 'outline-4))
  (org-level-5                    (:extend nil :inherit 'outline-5))
  (org-level-6                    (:extend nil :inherit 'outline-6))
  (org-level-7                    (:extend nil :inherit 'outline-7))
  (org-level-8                    (:extend nil :inherit 'outline-8))
  (org-link                       (:inherit 'link))
  (org-list-dt                    (:weight 'bold))
  (org-macro                      (:inherit 'org-latex-and-related))
  (org-meta-line                  (:inherit 'font-lock-comment-face))
  (org-mode-line-clock            (:inherit 'mode-line))
  (org-mode-line-clock-overrun    (:background red :inherit 'mode-line))
  (org-priority                   (:inherit 'font-lock-keyword-face))
  (org-property-value             nil)
  (org-quote                      (:inherit 'org-block))
  (org-scheduled                  (:foreground green))
  (org-scheduled-previously       (:foreground red))
  (org-scheduled-today            (:foreground green))
  (org-sexp-date                  (:foreground violet))
  (org-special-keyword            (:inherit 'font-lock-keyword-face))
  (org-table                      (:foreground blue))
  (org-table-header               (:foreground blue :background base2 :inherit 'org-table))
  (org-tag-group                  (:inherit 'org-tag))
  (org-tag                        (:weight 'bold))
  (org-target                     (:underline t))
  (org-time-grid                  (:foreground yellow))
  (org-todo                       (:weight 'bold :foreground red))
  (org-upcoming-deadline          (:foreground red))
  (org-upcoming-distant-deadline  (:inherit 'org-default))
  (org-verbatim                   (:inherit 'shadow))
  (org-verse                      (:inherit 'org-block))
  (org-warning                    (:inherit 'font-lock-warning-face))

  ;; eglot
  (eglot-inlay-hint-face       (:foreground base1 :background base2 :height 0.8))
  (eglot-highlight-symbol-face (:underline t))

  ;; sh-mode
  (sh-escaped-newline (:inherit 'font-lock-string-face))
  (sh-quoted-exec     (:foreground violet))
  (sh-heredoc         (:foreground yellow))

  ;; Window dividers
  (window-divider             (:foreground base02))
  (window-divider-last-pixel  (:foreground base02))
  (window-divider-first-pixel (:foreground base02))

  ;; Corfu
  (corfu-bar          (:background base02))
  (corfu-border       (:background base03))
  (corfu-current      (:inverse-video t))
  (corfu-default      (:background base2))
  (corfu-deprecated   (:strike-through t :inherit 'shadow))
  (corfu-annotations  (:inherit 'completions-annotations))

  ;; completions
  (completions-common-part (:foreground blue))

  ;; imenu-list
  (imenu-list-entry-face nil)
  (imenu-list-entry-face-0 (:foreground violet :inherit 'imenu-list-entry-face))
  (imenu-list-entry-face-1 (:foreground green :inherit 'imenu-list-entry-face))
  (imenu-list-entry-face-2 (:foreground blue :inherit 'imenu-list-entry-face))
  (imenu-list-entry-face-3 (:foreground orange :inherit 'imenu-list-entry-face))
  (imenu-list-entry-subalist-face-0 (:weight 'bold :underline t :inherit 'imenu-list-entry-face-0))
  (imenu-list-entry-subalist-face-1 (:weight 'bold :underline t :inherit 'imenu-list-entry-face-1))
  (imenu-list-entry-subalist-face-2 (:weight 'bold :underline t :inherit 'imenu-list-entry-face-2))
  (imenu-list-entry-subalist-face-3 (:weight 'bold :underline t :inherit 'imenu-list-entry-face-3))

  ;; diff-refine
  (diff-refine-added    (:background green :foreground base3))
  (diff-refine-changed  (:background yellow :foreground base3))
  (diff-refine-removed  (:background red :foreground base3))

  ;; markdown
  (markdown-pre-face (:foreground base00))
  (markdown-inline-code-face (:foreground magenta))

  ;; doom modeline
  (doom-modeline-panel (:foreground blue))

  ;; evil
  (evil-ex-substitute-replacement (:underline t :foreground red))
))

(provide-theme 'my-solarized-light)
