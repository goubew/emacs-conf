(require 'autothemer)

(defmacro my-theme-deftheme (name description palette)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette

    (;; basics
     (default (:background bg :foreground fg))

     (fixed-pitch (:family nil))
     (fixed-pitch-serif (:family nil))
     (variable-pitch (:family nil))
     (variable-pith-text (:family nil :inherit))

     (cursor              (:background fg0))
     (fringe              (:background bg0_s))
     (highlight           (:background bg1))
     (hl-line             (:background bg0_s))
     (link                (:foreground bright-blue :underline t))
     (link-visited        (:foreground bright-purple :underline t))
     (minibuffer-prompt   (:foreground bright-blue))
     (mode-line           (:background bg1))
     (mode-line-inactive  (:background bg0_s))
     (region              (:background bg1))
     (secondary-selection (:background bg2))
     (shadow              (:foreground fg3))

     ;; Built in syntax bad
     (font-lock-builtin-face       (:foreground bright-red))
     (font-lock-constant-face      (:foreground bright-purple))
     (font-lock-comment-face       (:foreground fg3))
     (font-lock-function-name-face (:foreground bright-yellow))
     (font-lock-keyword-face       (:foreground bright-red))
     (font-lock-string-face        (:foreground bright-green))
     (font-lock-number-face        (:foreground bright-blue))
     (font-lock-variable-name-face (:foreground bright-blue))
     (font-lock-type-face          (:foreground bright-purple))
     (font-lock-warning-face       (:foreground bright-red))

     ;; Basic faces
     (error               (:foreground bright-red))
     (success             (:foreground bright-green))
     (warning             (:foreground bright-yellow))
     (trailing-whitespace (:background bright-red :foreground bg0))
     (escape-glyph        (:foreground bright-aqua))
     (header-line         (:inherit 'mode-line))
     (homoglyph           (:foreground bright-yellow))
     (match               (:foreground bright-green))

     ;; whitespace-mode
     (whitespace-empty            (:inherit 'default))
     (whitespace-hspace           (:inherit 'default))
     (whitespace-indentation      (:inherit 'default))
     (whitespace-line             (:inherit 'default))
     (whitespace-newline          (:inherit 'default))
     (whitespace-space            (:inherit 'default))
     (whitespace-space-after-tab  (:background bright-red :foreground bg0))
     (whitespace-space-before-tab (:background bright-red :foreground bg0))
     (whitespace-tab              (:inherit 'default))
     (whitespace-trailing         (:background bright-red :foreground bg0))

     ;; RainbowDelimiters
     (rainbow-delimiters-depth-1-face   (:foreground bright-orange))
     (rainbow-delimiters-depth-2-face   (:foreground bright-yellow))
     (rainbow-delimiters-depth-3-face   (:foreground bright-green))
     (rainbow-delimiters-depth-4-face   (:foreground bright-blue))
     (rainbow-delimiters-depth-5-face   (:foreground bright-purple))
     (rainbow-delimiters-depth-6-face   (:foreground bright-orange))
     (rainbow-delimiters-depth-7-face   (:foreground bright-yellow))
     (rainbow-delimiters-depth-8-face   (:foreground bright-green))
     (rainbow-delimiters-depth-9-face   (:foreground bright-blue))
     (rainbow-delimiters-depth-10-face  (:foreground bright-purple))
     (rainbow-delimiters-depth-11-face  (:foreground bright-orange))
     (rainbow-delimiters-depth-12-face  (:foreground bright-yellow))
     (rainbow-delimiters-unmatched-face
      (:background bright-red :foreground bg0))

     ;; line numbers
     (line-number              (:foreground fg4))
     (line-number-current-line (:foreground fg :bold 't))

     ;; show-paren
     (show-paren-match    (:underline t))
     (show-paren-mismatch (:background bright-red :foreground bg0))

     ;; ace-window
     (aw-key-face                     (:inherit 'font-lock-builtin-face))
     (aw-mode-line-face               (:inherit 'mode-line-buffer-id))
     (aw-background-face              (:foreground bg))
     (aw-minibuffer-leading-char-face (:inherit 'aw-leading-char-face))
     (aw-leading-char-face
      (:background bg4 :foreground bright-orange))

     ;; avy
     (avy-background-face (:foreground bg))
     (avy-lead-face       (:background bright-orange :foreground bg))
     (avy-lead-face-0     (:background bright-green :foreground bg))
     (avy-lead-face-1     (:background bright-yellow :foreground bg))
     (avy-lead-face-2     (:background bright-aqua :foreground bg))

     ;; circe
     (circe-fool-face               (:foreground fg3))
     (circe-topic-diff-removed-face (:inherit 'diff-removed))
     (circe-topic-diff-new-face     (:inherit 'diff-added))
     (circe-originator-face         nil)
     (circe-my-message-face         nil)
     (circe-highlight-nick-face     (:weight 'bold :foreground bright-purple))
     (circe-server-face             (:foreground bright-blue))
     (circe-prompt-face
      (:weight 'bold :foreground bright-aqua :background bg1))

     ;; isearch
     (isearch        (:background bg1))
     (lazy-highlight (:background bg0_s))
     (isearch-fail   (:foreground bright-red))

     ;; highlight indent guides
     (highlight-indent-guides-character-face (:foreground bright-yellow))

     ;; flyspell
     (flyspell-duplicate (:underline (:color bright-aqua :style 'wave)))
     (flyspell-incorrect (:underline (:color bright-red :style 'wave)))

     ;; diff-hl
     (diff-hl-change (:foreground bright-blue :background bright-blue))
     (diff-hl-delete (:foreground bright-red :background bright-red))
     (diff-hl-insert (:foreground bright-green :background bright-green))

     ;; dibright-red
     (dibright-red-broken-symlink (:background bright-red :foreground bg0))

     ;; eshell
     (eshell-prompt        (:foreground bright-aqua))
     (eshell-ls-archive    (:foreground fg3))
     (eshell-ls-backup     (:foreground fg4))
     (eshell-ls-clutter    (:foreground bright-orange))
     (eshell-ls-directory  (:foreground bright-yellow))
     (eshell-ls-executable (:weight 'bold))
     (eshell-ls-missing    (:foreground bright-red))
     (eshell-ls-product    (:foreground bright-red))
     (eshell-ls-readonly   (:foreground fg3))
     (eshell-ls-special    (:foreground bright-yellow))
     (eshell-ls-symlink    (:foreground bright-red))
     (eshell-ls-unreadable (:foreground bright-red))

     ;; tab bar
     (tab-bar                    (:background bg2 :foreground fg))
     (tab-bar-tab                (:foreground bright-aqua :weight 'bold))
     (tab-bar-tab-inactive       (:foreground fg))
     (tab-bar-tab-ungrouped      (:foreground fg))
     (tab-bar-tab-group-current  (:foreground bg2 :weight 'bold))
     (tab-bar-tab-group-inactive (:foreground fg))

     ;; wgrep
     (wgrep-reject-face (:foreground bright-red))
     (wgrep-face        (:background bright-green :foreground bg0))
     (wgrep-done-face   (:foreground bright-blue))
     (wgrep-file-face   (:background bright-green :foreground bg0))
     (wgrep-delete-face (:background bright-red :foreground bg0))

     ;; hydra
     (hydra-face-bright-red      (:foreground bright-red))
     (hydra-face-bright-blue     (:foreground bright-blue))
     (hydra-face-amaranth (:foreground bright-purple))
     (hydra-face-pink     (:foreground bright-blue))
     (hydra-face-teal     (:foreground bright-aqua))

     ;; lui
     (lui-button-face               (:underline t :foreground bright-green))
     (lui-time-stamp-face           (:weight 'bold :foreground bright-purple))
     (lui-irc-colors-monospace-face nil)

     ;; magit
     (magit-blame-highlight
      (:extend t :foreground fg2 :background bg2))
     (magit-reflog-other                (:foreground bright-aqua))
     (magit-reflog-remote               (:foreground bright-aqua))
     (magit-reflog-cherry-pick          (:foreground bright-green))
     (magit-reflog-rebase               (:foreground bright-purple))
     (magit-reflog-reset                (:foreground bright-red))
     (magit-reflog-checkout             (:foreground bright-blue))
     (magit-reflog-merge                (:foreground bright-green))
     (magit-reflog-amend                (:foreground bright-purple))
     (magit-reflog-commit               (:foreground bright-green))
     (magit-bisect-bad                  (:foreground bright-red))
     (magit-bisect-skip                 (:foreground bright-yellow))
     (magit-bisect-good                 (:foreground bright-green))
     (magit-sequence-drop               (:foreground bright-red))
     (magit-sequence-head               (:foreground fg2))
     (magit-sequence-part               (:foreground bright-yellow))
     (magit-sequence-stop               (:foreground bright-green))
     (magit-cherry-equivalent           (:foreground bright-purple))
     (magit-cherry-unmatched            (:foreground bright-aqua))
     (magit-signature-error             (:foreground bright-blue))
     (magit-signature-revoked           (:foreground bright-purple))
     (magit-signature-expired           (:foreground bright-yellow))
     (magit-signature-untrusted         (:foreground bright-aqua))
     (magit-signature-bad               (:weight 'bold :foreground bright-red))
     (magit-signature-good              (:foreground bright-green))
     (magit-refname-pullreq             (:foreground fg2))
     (magit-branch-upstream             (:slant 'italic))
     (magit-branch-local                (:foreground fg2))
     (magit-branch-remote               (:foreground bright-green))
     (magit-tag                         (:foreground bright-yellow))
     (magit-hash                        (:foreground fg2))
     (magit-refname                     (:foreground bright-yellow))
     (magit-log-date                    (:foreground bright-yellow))
     (magit-log-author                  (:foreground bright-red))
     (magit-log-graph                   (:foreground fg2))
     (magit-diffstat-removed            (:foreground bright-orange))
     (magit-diffstat-added              (:foreground bright-green))
     (magit-diff-context                (:foreground fg2))
     (magit-diff-context-highlight      (:background bg1 :foreground fg))
     (magit-diff-base
      (:background bright-yellow :foreground bg0))
     (magit-diff-base-highlight
      (:background bright-yellow :foreground bg0))
     (magit-diff-removed
      (:background bg0 :foreground bright-red))
     (magit-diff-removed-highlight
      (:background bg1 :foreground bright-red))
     (magit-diff-added
      (:background bg0 :foreground bright-green))
     (magit-diff-added-highlight
      (:background bg1 :foreground bright-green))
     (magit-diff-hunk-heading
      (:background bg3 :foreground fg2 :weight 'bold))
     (magit-diff-hunk-heading-highlight
      (:background bg3 :foreground fg :weight 'bold))
     (magit-section-heading-selection   (:foreground bright-red))
     (magit-section-heading             (:weight 'bold :foreground bright-red))
     (magit-section-highlight           (:background bg1))
     (magit-dimmed                      (:foreground fg2))

     ;; orderless
     (orderless-match-face-0 (:foreground bright-blue))
     (orderless-match-face-1 (:foreground bright-purple))
     (orderless-match-face-2 (:foreground bright-green))
     (orderless-match-face-3 (:foreground bright-yellow))

     ;; ansi
     (ansi-color-black          (:foreground gray :background gray))
     (ansi-color-red            (:foreground bright-red :background bright-red))
     (ansi-color-green
      (:foreground bright-green :background bright-green))
     (ansi-color-yellow
      (:foreground bright-yellow :background bright-yellow))
     (ansi-color-blue
      (:foreground bright-blue :background bright-blue))
     (ansi-color-magenta
      (:foreground bright-purple :background bright-purple))
     (ansi-color-cyan
      (:foreground bright-aqua :background bright-aqua))
     (ansi-color-white          (:foreground bg :background bg))
     (ansi-color-bright-black   (:foreground fg :background fg))
     (ansi-color-bright-red     (:foreground bright-red :background bright-red))
     (ansi-color-bright-green
      (:foreground bright-green :background bright-green))
     (ansi-color-bright-yellow
      (:foreground bright-yellow :background bright-yellow))
     (ansi-color-bright-blue
      (:foreground bright-blue :background bright-blue))
     (ansi-color-bright-magenta
      (:foreground bright-purple :background bright-purple))
     (ansi-color-bright-cyan
      (:foreground bright-aqua :background bright-aqua))
     (ansi-color-bright-white
      (:foreground bright-gray :background bright-gray))

     ;; org
     (org-agenda-calendar-event      (:inherit 'default))
     (org-agenda-calendar-sexp       (:inherit 'default))
     (org-agenda-clocking            (:inherit 'secondary-selection))
     (org-agenda-column-dateline     (:inherit 'org-column))
     (org-agenda-current-time        (:inherit 'org-time-grid))
     (org-agenda-date                (:inherit 'org-agenda-structure))
     (org-agenda-date-today
      (:weight 'bold :slant 'italic :inherit 'org-agenda-date))
     (org-agenda-date-weekend-today  (:inherit 'org-agenda-date-today))
     (org-agenda-date-weekend        (:weight 'bold :inherit 'org-agenda-date))
     (org-agenda-diary               (:inherit 'default))
     (org-agenda-dimmed-todo-face    (:foreground fg))
     (org-agenda-done                (:foreground bright-green))
     (org-agenda-filter-category     (:inherit 'mode-line))
     (org-agenda-filter-effort       (:inherit 'mode-line))
     (org-agenda-filter-regexp       (:inherit 'mode-line))
     (org-agenda-filter-tags         (:inherit 'mode-line))
     (org-agenda-restriction-lock    (:background bright-aqua :foreground bg0))
     (org-agenda-structure-filter
      (:inherit  ('org-warning 'org-agenda-structure)))
     (org-agenda-structure           (:foreground bright-blue))
     (org-agenda-structure-secondary (:inherit 'org-agenda-structure))
     (org-archived                   (:inherit 'shadow))
     (org-block-begin-line           (:extend t :inherit 'org-meta-line))
     (org-block-end-line             (:extend t :inherit 'org-block-begin-line))
     (org-block                      (:foreground fg))
     (org-checkbox                   (:inherit 'bold))
     (org-checkbox-statistics-done   (:inherit 'org-done))
     (org-checkbox-statistics-todo   (:inherit 'org-todo))
     (org-cite                       (:inherit 'link))
     (org-cite-key                   (:inherit 'link))
     (org-clock-overlay              (:background bg1))
     (org-code                       (:foreground bright-green))
     (org-column-title
      (:weight 'bold :underline t :background bg1))
     (org-column
      (:weight 'normal :slant 'normal :underline nil
               :strike-through nil :background bg1))
     (org-date-selected              (:inverse-video t :foreground bright-red))
     (org-date                       (:underline t :foreground bright-purple))
     (org-default                    (:inherit 'default))
     (org-dispatcher-highlight
      (:weight 'bold :foreground bg0 :background bright-blue))
     (org-document-info              (:foreground fg2))
     (org-document-info-keyword      (:inherit 'shadow))
     (org-document-title             (:weight 'bold :foreground bright-blue))
     (org-done                       (:weight 'bold :foreground bright-green))
     (org-drawer                     (:foreground bright-blue))
     (org-ellipsis                   (:underline t :foreground bright-yellow))
     (org-footnote                   (:underline t :foreground bright-purple))
     (org-formula                    (:foreground bright-red))
     (org-headline-done              (:foreground fg2))
     (org-headline-todo              (:foreground bright-red))
     (org-hide                       (:foreground bg0))
     (org-imminent-deadline          (:inherit 'org-warning))
     (org-inline-src-block           (:inherit 'org-block))
     (org-latex-and-related          (:foreground bright-orange))
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
     (org-mode-line-clock-overrun
      (:background bright-red :inherit 'mode-line))
     (org-priority                   (:inherit 'font-lock-keyword-face))
     (org-property-value             nil)
     (org-quote                      (:inherit 'org-block))
     (org-scheduled                  (:foreground bright-green))
     (org-scheduled-previously       (:foreground bright-red))
     (org-scheduled-today            (:foreground bright-green))
     (org-sexp-date                  (:foreground bright-purple))
     (org-special-keyword            (:inherit 'font-lock-keyword-face))
     (org-table                      (:foreground bright-blue))
     (org-table-header
      (:foreground bright-blue :background bg1 :inherit 'org-table))
     (org-tag-group                  (:inherit 'org-tag))
     (org-tag                        (:weight 'bold))
     (org-target                     (:underline t))
     (org-time-grid                  (:foreground bright-yellow))
     (org-todo                       (:weight 'bold :foreground bright-red))
     (org-upcoming-deadline          (:foreground bright-red))
     (org-upcoming-distant-deadline  (:inherit 'org-default))
     (org-verbatim                   (:inherit 'shadow))
     (org-verse                      (:inherit 'org-block))
     (org-warning                    (:inherit 'font-lock-warning-face))

     ;; eglot
     (eglot-inlay-hint-face       (:foreground fg2 :background bg1 :height 0.8))
     (eglot-highlight-symbol-face (:underline t))

     ;; sh-mode
     (sh-escaped-newline (:inherit 'font-lock-string-face))
     (sh-quoted-exec     (:foreground bright-purple))
     (sh-hebright-redoc         (:foreground bright-yellow))

     ;; Window dividers
     (window-divider             (:foreground bg4))
     (window-divider-last-pixel  (:foreground bg4))
     (window-divider-first-pixel (:foreground bg4))

     ;; Corfu
     (corfu-bar          (:background fg2))
     (corfu-border       (:background bg4))
     (corfu-current      (:inverse-video t))
     (corfu-default      (:background bg1))
     (corfu-deprecated   (:strike-through t :inherit 'shadow))
     (corfu-annotations  (:inherit 'completions-annotations))

     ;; completions
     (completions-common-part (:foreground bright-blue))

     ;; imenu-list
     (imenu-list-entry-face nil)
     (imenu-list-entry-face-0
      (:foreground bright-purple :inherit 'imenu-list-entry-face))
     (imenu-list-entry-face-1
      (:foreground bright-green :inherit 'imenu-list-entry-face))
     (imenu-list-entry-face-2
      (:foreground bright-blue :inherit 'imenu-list-entry-face))
     (imenu-list-entry-face-3
      (:foreground bright-orange :inherit 'imenu-list-entry-face))
     (imenu-list-entry-subalist-face-0
      (:weight 'bold :underline t :inherit 'imenu-list-entry-face-0))
     (imenu-list-entry-subalist-face-1
      (:weight 'bold :underline t :inherit 'imenu-list-entry-face-1))
     (imenu-list-entry-subalist-face-2
      (:weight 'bold :underline t :inherit 'imenu-list-entry-face-2))
     (imenu-list-entry-subalist-face-3
      (:weight 'bold :underline t :inherit 'imenu-list-entry-face-3))

     ;; diff-refine
     (diff-refine-added    (:background bg2 :foreground bright-green))
     (diff-refine-changed  (:background bg2 :foreground bright-blue))
     (diff-refine-removed  (:background bg2 :foreground bright-red))

     ;; markdown
     (markdown-pre-face (:foreground fg))
     (markdown-inline-code-face (:foreground bright-purple))

     ;; doom modeline
     (doom-modeline-panel (:foreground bright-blue))

     ;; evil
     (evil-ex-substitute-replacement (:underline t :foreground bright-red))

     ;; transient
     (transient-key-exit (:foreground bright-red :inherit 'transient-key))
     (transient-key-return (:foreground bright-green :inherit 'transient-key))
     (transient-key-noop (:foreground bg2 :inherit 'transient-key))
     (transient-key-stay (:foreground bright-aqua :inherit 'transient-key))
     (transient-key (:inherit 'font-lock-builtin-face))

     ;; meow
     (meow-cheatsheet-highlight
      (:foreground fg0 :inherit 'meow-cheatsheet-command))
     (meow-cheatsheet-command (:inherit 'default))
     (meow-search-indicator (:foreground fg4))
     (meow-kmacro-cursor (:underline t))
     (meow-keypad-cannot-display (:height 0.7 :foreground fg0))
     (meow-position-highlight-number (:background bright-blue :foreground bg0))
     (meow-position-highlight-number-3
      (:background bright-blue :foreground bg0))
     (meow-position-highlight-number-2
      (:background bright-purple :foreground bg0))
     (meow-position-highlight-number-1
      (:background bright-aqua :foreground bg0))
     (meow-position-highlight-reverse-number-3
      (:inherit 'meow-position-highlight-number-3))
     (meow-position-highlight-reverse-number-2
      (:inherit 'meow-position-highlight-number-2))
     (meow-position-highlight-reverse-number-1
      (:inherit 'meow-position-highlight-number-1))

     ;; neo-tree
     (neo-banner-face              (:foreground bright-purple))
     (neo-button-face              (:underline nil))
     (neo-dir-link-face            (:foreground bright-blue))
     (neo-expand-btn-face          (:foreground bright-aqua))
     (neo-file-link-face           (:foreground fg0))
     (neo-header-face              (:foreground bright-purple))
     (neo-root-dir-face            (:foreground bright-purple))
     (neo-vc-added-face            (:foreground bright-aqua))
     (neo-vc-conflict-face         (:foreground bright-red))
     (neo-vc-default-face          (:foreground fg0))
     (neo-vc-edited-face           (:foreground bright-purple))
     (neo-vc-ignored-face          (:foreground fg4))
     (neo-vc-missing-face          (:foreground bright-red))
     (neo-vc-needs-merge-face      (:foreground bright-red))
     (neo-vc-needs-update-face     (:underline t))
     (neo-vc-removed-face          (:strike-through t))
     (neo-vc-unlocked-changes-face (:foreground bright-orange))
     (neo-vc-unregistered-face     nil)
     (neo-vc-up-to-date-face       (:foreground fg4))
     (neo-vc-user-face             (:slant 'italic :foreground bright-orange))

     )))

(provide 'my-theme-base)
