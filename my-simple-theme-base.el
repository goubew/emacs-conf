(require 'autothemer)

(defmacro my-simple-theme-deftheme (name description palette)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette

    (;; basics
     (default (:background bg :foreground fg))

     (fixed-pitch (:family nil))
     (fixed-pitch-serif (:family nil))
     (variable-pitch (:family nil))
     (variable-pitch-text (:family nil))

     (cursor              (:background fg2 :foreground bg))
     (fringe              (:background bg1))
     (highlight           (:background bg1))
     (hl-line             (:background bg1))
     (link                (:foreground fg :underline t))
     (link-visited        (:foreground fg :underline t))
     (minibuffer-prompt   (:foreground fg0))
     (mode-line           (:background fg :foreground bg))
     (mode-line-inactive  (:background bg1))
     (region              (:background bg2))
     (secondary-selection (:background bg2))
     (shadow              (:foreground fg0))

     ;; Built in syntax bad
     (font-lock-builtin-face       (:foreground fg1))
     (font-lock-constant-face      (:foreground fg2))
     (font-lock-comment-face       (:foreground fg0))
     (font-lock-function-name-face (:foreground fg2))
     (font-lock-keyword-face       (:foreground fg1))
     (font-lock-string-face        (:foreground fg0))
     (font-lock-number-face        (:foreground fg1))
     (font-lock-variable-name-face (:foreground fg2))
     (font-lock-type-face          (:foreground fg2))
     (font-lock-warning-face       (:underline (:color fg0 :style 'line)))

     ;; Basic faces
     (blink-matching-paren-offscreen (:foreground fg2))
     (bookmark-face (:foreground fg))
     (confusingly-reordered (:inherit 'warning))
     (edmacro-label (:foreground fg1))
     (elisp-shorthand-font-lock-face (:foreground fg1))
     (error               (:underline (:color fg2 :style 'line)))
     (success             (:inherit 'default))
     (warning             (:underline (:color fg :style 'line)))
     (trailing-whitespace (:background fg :foreground bg))
     (escape-glyph        (:foreground fg1))
     (header-line         (:inherit 'mode-line))
     (homoglyph           (:foreground fg1))
     (match               (:foreground fg1))

     ;; whitespace-mode
     (whitespace-empty            (:inherit 'shadow))
     (whitespace-hspace           (:inherit 'shadow))
     (whitespace-indentation      (:inherit 'shadow))
     (whitespace-line             (:inherit 'shadow))
     (whitespace-newline          (:inherit 'shadow))
     (whitespace-space            (:inherit 'shadow))
     (whitespace-space-after-tab  (:inherit 'shadow))
     (whitespace-space-before-tab (:inherit 'shadow))
     (whitespace-tab              (:inherit 'shadow))
     (whitespace-trailing         (:inherit 'shadow))

     ;; RainbowDelimiters
     (rainbow-delimiters-depth-1-face   (:foreground fg0))
     (rainbow-delimiters-depth-2-face   (:foreground fg2))
     (rainbow-delimiters-depth-3-face   (:foreground fg0))
     (rainbow-delimiters-depth-4-face   (:foreground fg2))
     (rainbow-delimiters-depth-5-face   (:foreground fg0))
     (rainbow-delimiters-depth-6-face   (:foreground fg2))
     (rainbow-delimiters-depth-7-face   (:foreground fg0))
     (rainbow-delimiters-depth-8-face   (:foreground fg2))
     (rainbow-delimiters-depth-9-face   (:foreground fg0))
     (rainbow-delimiters-depth-10-face  (:foreground fg2))
     (rainbow-delimiters-depth-11-face  (:foreground fg0))
     (rainbow-delimiters-depth-12-face  (:foreground fg2))
     (rainbow-delimiters-unmatched-face (:underline (:color fg1 :style 'line)))

     ;; line numbers
     (line-number              (:foreground fg0))
     (line-number-current-line (:foreground fg0 :bold 't))

     ;; show-paren
     (show-paren-match    (:underline (:color fg0 :style 'line)))
     (show-paren-mismatch (:underline (:color fg2 :style 'line)))

     ;; ace-window
     (aw-key-face                     (:inherit 'font-lock-builtin-face))
     (aw-mode-line-face               (:inherit 'mode-line-buffer-id))
     (aw-background-face              (:foreground bg))
     (aw-minibuffer-leading-char-face (:inherit 'aw-leading-char-face))
     (aw-leading-char-face
      (:background bg1 :foreground fg2))

     ;; avy
     (avy-background-face (:foreground bg))
     (avy-lead-face       (:background bg1 :foreground fg2))
     (avy-lead-face-0     (:background bg1 :foreground fg1))
     (avy-lead-face-1     (:background bg1 :foreground fg))
     (avy-lead-face-2     (:background bg1 :foreground fg0))

     ;; circe
     (circe-fool-face               (:foreground fg0))
     (circe-topic-diff-removed-face (:inherit 'diff-removed))
     (circe-topic-diff-new-face     (:inherit 'diff-added))
     (circe-originator-face         (:foreground fg0))
     (circe-my-message-face         (:foreground fg))
     (circe-highlight-nick-face     (:underline t))
     (circe-server-face             (:foreground fg1))
     (circe-prompt-face             (:weight 'bold))

     ;; consult
     (consult-async-running (:foreground fg1))
     (consult-separator (:foreground bg))
     (consult-narrow-indicator (:foreground fg1))

     ;; completion-preview
     (completion-preview-common (:foreground fg0))
     (completion-preview-exact (:foreground fg0 (:underline (:color fg0 :style 'line))))

     ;; custom
     (custom-button
      (:box (:line-width 2 :style 'released-button) :foreground fg2 :background bg))
     (custom-button-mouse
      (:box (:line-width 2 :style 'released-button) :foreground fg2 :background bg))
     (custom-button-pressed
      (:box (:line-width 2 :style 'pressed-button) :foreground fg2 :background bg))
     (custom-button-pressed-unraised (:foreground fg2 :inherit 'custom-button-unraised))
     (custom-button-unraised (:inherit 'underline))
     (custom-changed (:foreground fg2))
     (custom-comment (:inherit 'default))
     (custom-comment-tag (:foreground fg2))
     (custom-documentation nil)
     (custom-face-tag (:inherit 'custom-variable-tag))
     (custom-group-subtitle (:weight 'bold))
     (custom-group-tag (:weight 'bold :foreground fg2))
     (custom-group-tag-1 (:weight 'bold :foreground fg))
     (custom-invalid (:inherit 'underline ))
     (custom-link (:inherit 'link))
     (custom-modified (:foreground fg :background bg1))
     (custom-rogue (:foreground fg2 :background bg1))
     (custom-saved (:underline t))
     (custom-set (:foreground fg2 :background bg))
     (custom-state (:foreground fg0))
     (custom-themed (:foreground fg :background bg1))
     (custom-variable-button (:weight 'bold :underline t))
     (custom-variable-obsolete (:foreground fg2))
     (custom-variable-tag (:weight 'bold :foreground fg2))
     (custom-visibility (:inherit 'link))

     ;; isearch
     (isearch        (:background bg2))
     (lazy-highlight (:background bg1))
     (isearch-fail   (:underline (:color fg2 :style 'line)))

     ;; highlight indent guides
     (highlight-indent-guides-character-face (:foreground fg0))

     ;; flyspell
     (flyspell-duplicate (:underline (:color fg0 :style 'line)))
     (flyspell-incorrect (:underline (:color fg0 :style 'line)))

     ;; diff
     (diff-added (:foreground fg2))
     (diff-changed (:foreground fg1))
     (diff-changed-unspecified (:foreground fg1))
     (diff-context (:foreground fg))
     (diff-error (:inherit 'error))
     (diff-file-header (:foreground fg1))
     (diff-function (:foreground fg))
     (diff-header (:foreground fg1 :weight 'bold))
     (diff-hunk-header (:background bg2 :inherit 'diff-header))
     (diff-index (:foreground fg))
     (diff-indicator-added (:inherit 'diff-added))
     (diff-indicator-changed (:inherit 'diff-changed))
     (diff-indicator-removed (:inherit 'diff-removed))
     (diff-refine-added    (:background bg2 :foreground fg2))
     (diff-refine-changed  (:background bg2 :foreground fg))
     (diff-refine-removed  (:background bg2 :foreground fg0))
     (diff-removed (:foreground fg0))

     ;; diff-hl
     (diff-hl-change (:foreground fg :background fg))
     (diff-hl-delete (:foreground fg0 :background fg0))
     (diff-hl-insert (:foreground fg2 :background fg2))

     ;; dired
     (dired-broken-symlink (:underline (:color fg2 :style 'line)))

     ;; eshell
     (eshell-prompt        (:weight 'bold))
     (eshell-ls-archive    (:inherit 'default))
     (eshell-ls-backup     (:inherit 'default))
     (eshell-ls-clutter    (:underline (:color fg2 :style 'line)))
     (eshell-ls-directory  (:foreground fg1))
     (eshell-ls-executable (:underline t))
     (eshell-ls-missing    (:underline (:color fg2 :style 'line)))
     (eshell-ls-product    (:inherit 'default))
     (eshell-ls-readonly   (:underline (:color fg2 :style 'line)))
     (eshell-ls-special    (:inherit 'default))
     (eshell-ls-symlink    (:foreground fg0))
     (eshell-ls-unreadable (:foreground fg0))

     ;; Go
     (go-coverage-0 (:foreground fg0))
     (go-coverage-1 (:foreground fg0))
     (go-coverage-2 (:foreground fg0))
     (go-coverage-3 (:foreground fg))
     (go-coverage-4 (:foreground fg))
     (go-coverage-5 (:foreground fg))
     (go-coverage-6 (:foreground fg1))
     (go-coverage-7 (:foreground fg1))
     (go-coverage-8 (:foreground fg1))
     (go-coverage-9 (:foreground fg2))
     (go-coverage-10 (:foreground fg2))
     (go-coverage-covered (:foreground fg2))
     (go-coverage-untracked (:foreground fg))

     ;; help
     (help-for-help-header (:foreground fg2))
     (help-key-binding (:box (:color fg2) :foreground fg2 :background bg1))

     ;; icon
     (icon-button (:box (:color fg2) :foreground fg2 :background bg2))

     ;; info
     (info-header-node (:foreground fg2))
     (info-menu-star (:foreground fg2))
     (info-node (:foreground fg2))

     ;; isearch
     (isearch-group-1 (:background bg2 :foreground fg2))
     (isearch-group-2 (:background bg2 :foreground fg2))

     ;; tab bar
     (tab-bar                    (:background bg1 :foreground fg))
     (tab-bar-tab                (:weight 'bold :inherit 'tab-bar))
     (tab-bar-tab-inactive       (:inherit 'tab-bar))
     (tab-bar-tab-ungrouped      (:inherit 'tab-bar))
     (tab-bar-tab-group-current  (:inherit 'tab-bar-tab))
     (tab-bar-tab-group-inactive (:inherit 'tab-bar))

     ;; wgrep
     (wgrep-reject-face (:underline (:color fg2 :style 'line)))
     (wgrep-face        (:inherit 'default))
     (wgrep-done-face   (:inherit 'default))
     (wgrep-file-face   (:inherit 'default))
     (wgrep-delete-face (:underline (:color fg2 :style 'line)))

     ;; hydra
     (hydra-face-bright-red      (:foreground fg2))
     (hydra-face-bright-blue     (:foreground fg1))
     (hydra-face-amaranth (:foreground fg))
     (hydra-face-pink     (:foreground fg))
     (hydra-face-teal     (:foreground fg))

     ;; lui
     (lui-button-face               (:underline t))
     (lui-time-stamp-face           (:weight 'bold))
     (lui-irc-colors-monospace-face nil)

     ;; TODO: Fix magit diff section
     ;; magit
     (magit-blame-highlight
      (:extend t :foreground fg2 :background bg2))
     (magit-reflog-other                (:foreground fg1))
     (magit-reflog-remote               (:foreground fg1))
     (magit-reflog-cherry-pick          (:foreground fg1))
     (magit-reflog-rebase               (:foreground fg1))
     (magit-reflog-reset                (:foreground fg1))
     (magit-reflog-checkout             (:foreground fg1))
     (magit-reflog-merge                (:foreground fg1))
     (magit-reflog-amend                (:foreground fg1))
     (magit-reflog-commit               (:foreground fg1))
     (magit-bisect-bad                  (:foreground fg))
     (magit-bisect-skip                 (:foreground fg0))
     (magit-bisect-good                 (:foreground fg2))
     (magit-sequence-drop               (:foreground fg1))
     (magit-sequence-head               (:foreground fg1))
     (magit-sequence-part               (:foreground fg1))
     (magit-sequence-stop               (:foreground fg1))
     (magit-cherry-equivalent           (:foreground fg1))
     (magit-cherry-unmatched            (:foreground fg1))
     (magit-signature-error             (:foreground fg2))
     (magit-signature-revoked           (:foreground fg2))
     (magit-signature-expired           (:foreground fg2))
     (magit-signature-untrusted         (:foreground fg2))
     (magit-signature-bad               (:foreground fg2))
     (magit-signature-good              (:foreground fg1))
     (magit-refname-pullreq             (:foreground fg1))
     (magit-branch-upstream             (:slant 'italic))
     (magit-branch-local                (:foreground fg1))
     (magit-branch-remote               (:foreground fg2))
     (magit-tag                         (:foreground fg1))
     (magit-hash                        (:foreground fg1))
     (magit-refname                     (:foreground fg1))
     (magit-log-date                    (:foreground fg1))
     (magit-log-author                  (:foreground fg1))
     (magit-log-graph                   (:foreground fg1))
     (magit-diffstat-removed            (:foreground fg0))
     (magit-diffstat-added              (:foreground fg2))
     (magit-diff-context                (:foreground fg))
     (magit-diff-context-highlight      (:background bg1 :foreground fg))
     (magit-diff-base                   (:inherit 'default))
     (magit-diff-base-highlight         (:background bg1 :foreground fg0))
     (magit-diff-file-heading           (:inherit 'default))
     (magit-diff-removed                (:foreground fg0))
     (magit-diff-removed-highlight      (:background bg1 :foreground fg0))
     (magit-diff-added                  (:foreground fg2))
     (magit-diff-added-highlight        (:background bg1 :foreground fg2))
     (magit-diff-hunk-heading           (:background bg2 :foreground fg1))
     (magit-diff-hunk-heading-highlight
      (:background bg2 :foreground fg1 :weight 'bold))
     (magit-section-heading-selection   (:background bg :foreground fg1))
     (magit-section-heading             (:background bg :foreground fg1))
     (magit-section-highlight           (:weight 'bold))
     (magit-dimmed                      (:inherit 'shadow))

     ;; orderless
     (orderless-match-face-0 (:foreground fg2))
     (orderless-match-face-1 (:foreground fg2))
     (orderless-match-face-2 (:foreground fg2))
     (orderless-match-face-3 (:foreground fg2))

     ;; ansi
     (ansi-color-black          (:inherit 'default))
     (ansi-color-red            (:inherit 'default))
     (ansi-color-green          (:inherit 'default))
     (ansi-color-yellow         (:inherit 'default))
     (ansi-color-blue           (:inherit 'default))
     (ansi-color-magenta        (:inherit 'default))
     (ansi-color-cyan           (:inherit 'default))
     (ansi-color-white          (:inherit 'default))
     (ansi-color-bright-black   (:inherit 'default))
     (ansi-color-bright-red     (:inherit 'default))
     (ansi-color-bright-green   (:inherit 'default))
     (ansi-color-bright-yellow  (:inherit 'default))
     (ansi-color-bright-blue    (:inherit 'default))
     (ansi-color-bright-magenta (:inherit 'default))
     (ansi-color-bright-cyan    (:inherit 'default))
     (ansi-color-bright-white   (:inherit 'default))

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
     (org-agenda-dimmed-todo-face    (:foreground fg0))
     (org-agenda-done                (:foreground fg0))
     (org-agenda-filter-category     (:inherit 'mode-line))
     (org-agenda-filter-effort       (:inherit 'mode-line))
     (org-agenda-filter-regexp       (:inherit 'mode-line))
     (org-agenda-filter-tags         (:inherit 'mode-line))
     (org-agenda-restriction-lock    (:foreground fg1))
     (org-agenda-structure-filter
      (:inherit  ('org-warning 'org-agenda-structure)))
     (org-agenda-structure           (:foreground fg1))
     (org-agenda-structure-secondary (:inherit 'org-agenda-structure))
     (org-archived                   (:inherit 'shadow))
     (org-block-begin-line           (:extend t :inherit 'org-meta-line))
     (org-block-end-line             (:extend t :inherit 'org-block-begin-line))
     (org-block                      (:foreground fg1))
     (org-checkbox                   (:inherit 'bold))
     (org-checkbox-statistics-done   (:inherit 'org-done))
     (org-checkbox-statistics-todo   (:inherit 'org-todo))
     (org-cite                       (:inherit 'link))
     (org-cite-key                   (:inherit 'link))
     (org-clock-overlay              (:background bg1))
     (org-code                       (:foreground fg1))
     (org-column-title
      (:weight 'bold :underline t :background bg1))
     (org-column
      (:weight 'normal :slant 'normal :underline nil
               :strike-through nil :background bg1))
     (org-date-selected              (:inverse-video t :foreground fg2))
     (org-date                       (:underline t :foreground fg1))
     (org-default                    (:inherit 'default))
     (org-dispatcher-highlight       (:weight 'bold))
     (org-document-info              (:foreground fg2))
     (org-document-info-keyword      (:inherit 'shadow))
     (org-document-title             (:weight 'bold :foreground fg1))
     (org-done                       (:weight 'bold :foreground fg1))
     (org-drawer                     (:foreground fg1))
     (org-ellipsis                   (:underline t :foreground fg1))
     (org-footnote                   (:underline t :foreground fg1))
     (org-formula                    (:foreground fg1))
     (org-headline-done              (:foreground fg2))
     (org-headline-todo              (:foreground fg1))
     (org-hide                       (:foreground bg))
     (org-imminent-deadline          (:inherit 'org-warning))
     (org-inline-src-block           (:inherit 'org-block))
     (org-latex-and-related          (:foreground fg1))
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
      (:background bg1 :inherit 'mode-line))
     (org-priority                   (:inherit 'font-lock-keyword-face))
     (org-property-value             nil)
     (org-quote                      (:inherit 'org-block))
     (org-scheduled                  (:foreground fg1))
     (org-scheduled-previously       (:foreground fg1))
     (org-scheduled-today            (:foreground fg1))
     (org-sexp-date                  (:foreground fg1))
     (org-special-keyword            (:inherit 'font-lock-keyword-face))
     (org-table                      (:foreground fg1))
     (org-table-header               (:foreground fg1 :background bg1))
     (org-tag-group                  (:inherit 'org-tag))
     (org-tag                        (:weight 'bold))
     (org-target                     (:underline t))
     (org-time-grid                  (:foreground fg1))
     (org-todo                       (:weight 'bold :foreground fg1))
     (org-upcoming-deadline          (:foreground fg1))
     (org-upcoming-distant-deadline  (:inherit 'org-default))
     (org-verbatim                   (:inherit 'shadow))
     (org-verse                      (:inherit 'org-block))
     (org-warning                    (:inherit 'font-lock-warning-face))

     ;; eglot
     (eglot-inlay-hint-face       (:foreground fg0 :background bg1 :height 0.8))
     (eglot-highlight-symbol-face (:underline t))

     ;; sh-mode
     (sh-escaped-newline (:inherit 'font-lock-string-face))
     (sh-quoted-exec     (:foreground fg1))
     (sh-heredoc         (:foreground fg1))

     ;; Window dividers
     (window-divider             (:foreground bg3))
     (window-divider-last-pixel  (:foreground bg3))
     (window-divider-first-pixel (:foreground bg3))

     ;; Corfu
     (corfu-bar          (:background fg2))
     (corfu-border       (:background bg3))
     (corfu-current      (:inverse-video t))
     (corfu-default      (:background bg1))
     (corfu-deprecated   (:strike-through t :inherit 'shadow))
     (corfu-annotations  (:inherit 'completions-annotations))

     ;; completions
     (completions-common-part (:foreground fg2))

     ;; imenu-list
     (imenu-list-entry-face nil)
     (imenu-list-entry-face-0
      (:foreground fg2 :inherit 'imenu-list-entry-face))
     (imenu-list-entry-face-1
      (:foreground fg2 :inherit 'imenu-list-entry-face))
     (imenu-list-entry-face-2
      (:foreground fg2 :inherit 'imenu-list-entry-face))
     (imenu-list-entry-face-3
      (:foreground fg2 :inherit 'imenu-list-entry-face))
     (imenu-list-entry-subalist-face-0
      (:weight 'bold :underline t :inherit 'imenu-list-entry-face-0))
     (imenu-list-entry-subalist-face-1
      (:weight 'bold :underline t :inherit 'imenu-list-entry-face-1))
     (imenu-list-entry-subalist-face-2
      (:weight 'bold :underline t :inherit 'imenu-list-entry-face-2))
     (imenu-list-entry-subalist-face-3
      (:weight 'bold :underline t :inherit 'imenu-list-entry-face-3))

     ;; markdown
     (markdown-pre-face (:inherit 'shadow))
     (markdown-inline-code-face (:foreground fg1))

     ;; doom modeline
     (doom-modeline-panel (:foreground fg2))

     ;; evil
     (evil-ex-substitute-replacement (:underline t :foreground fg2))

     ;; transient
     (transient-key-exit (:foreground fg2 :inherit 'transient-key))
     (transient-key-return (:foreground fg2 :inherit 'transient-key))
     (transient-key-noop (:foreground bg2 :inherit 'transient-key))
     (transient-key-stay (:foreground fg2 :inherit 'transient-key))
     (transient-key (:inherit 'font-lock-builtin-face))

     ;; meow
     (meow-cheatsheet-highlight
      (:foreground fg0 :inherit 'meow-cheatsheet-command))
     (meow-cheatsheet-command (:inherit 'default))
     (meow-search-indicator (:foreground fg2))
     (meow-kmacro-cursor (:underline t))
     (meow-keypad-cannot-display (:height 0.7 :foreground fg0))
     (meow-position-highlight-number (:background fg2 :foreground bg1))
     (meow-position-highlight-number-3
      (:background fg2 :foreground bg1))
     (meow-position-highlight-number-2
      (:background fg2 :foreground bg1))
     (meow-position-highlight-number-1
      (:background fg2 :foreground bg1))
     (meow-position-highlight-reverse-number-3
      (:inherit 'meow-position-highlight-number-3))
     (meow-position-highlight-reverse-number-2
      (:inherit 'meow-position-highlight-number-2))
     (meow-position-highlight-reverse-number-1
      (:inherit 'meow-position-highlight-number-1))

     ;; neo-tree
     (neo-banner-face              (:foreground fg2))
     (neo-button-face              (:underline nil))
     (neo-dir-link-face            (:foreground fg2))
     (neo-expand-btn-face          (:foreground fg2))
     (neo-file-link-face           (:foreground fg0))
     (neo-header-face              (:foreground fg2))
     (neo-root-dir-face            (:foreground fg2))
     (neo-vc-added-face            (:foreground fg0))
     (neo-vc-conflict-face         (:foreground fg1))
     (neo-vc-default-face          (:foreground fg))
     (neo-vc-edited-face           (:foreground fg2))
     (neo-vc-ignored-face          (:foreground fg0))
     (neo-vc-missing-face          (:foreground fg2))
     (neo-vc-needs-merge-face      (:foreground fg2))
     (neo-vc-needs-update-face     (:underline t))
     (neo-vc-removed-face          (:strike-through t))
     (neo-vc-unlocked-changes-face (:foreground fg2))
     (neo-vc-unregistered-face     nil)
     (neo-vc-up-to-date-face       (:foreground fg2))
     (neo-vc-user-face             (:slant 'italic :foreground fg2))

     ;; vertical border
     (vertical-border (:foreground bg3))

     ;; compilation-mode
     (compilation-mode-line-exit (:foreground fg))
     (compilation-mode-line-fail (:foreground fg2))

     )))

(provide 'my-simple-theme-base)
;;; my-simple-theme-base.el ends here
