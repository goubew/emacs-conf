;; -*- lexical-binding: t; -*-
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

     (cursor              (:background fg :foreground bg))
     (fringe              (:background bg2))
     (highlight           (:background bg1))
     (hl-line             (:background bg1))
     (link                (:foreground fgblue :underline (:color fgblue)))
     (link-visited        (:foreground fgpurple :underline (:color fgpurple)))
     (margin              (:background bg1))
     (minibuffer-prompt   (:foreground fg0))
     (mode-line           (:inherit 'cursor))
     (mode-line-inactive  (:background bg1))
     (region              (:background bggreen))
     (secondary-selection (:background bg1))
     (shadow              (:foreground fg0))

     ;; Built in syntax bad
     (font-lock-builtin-face       (:foreground fg :weight 'bold))
     (font-lock-constant-face      (:foreground fg))
     (font-lock-comment-face       (:foreground fg0))
     (font-lock-function-name-face (:foreground fg))
     (font-lock-keyword-face       (:foreground fg :weight 'bold))
     (font-lock-string-face        (:foreground fg :slant 'oblique))
     (font-lock-number-face        (:foreground fg))
     (font-lock-variable-name-face (:foreground fg))
     (font-lock-type-face          (:foreground fg))
     (font-lock-warning-face       (:inherit 'warning))

     ;; Basic faces
     (blink-matching-paren-offscreen  (:foreground fg))
     (bookmark-face                   (:foreground fg))
     (confusingly-reordered           (:inherit 'warning))
     (edmacro-label                   (:foreground fg))
     (elisp-shorthand-font-lock-face  (:foreground fg))
     (error                           (:background bgred))
     (separator-line                  (:height 0.1 :background bg1))
     (success                         (:inherit 'default))
     (warning                         (:background bgyellow))
     (tool-bar                        (:inherit 'tab-bar))
     (tooltip                         (:inherit 'hl-line))
     (trailing-whitespace             (:inherit 'error))
     (escape-glyph                    (:weight 'bold))
     (header-line                     (:inherit 'mode-line))
     (homoglyph                       (:foreground fg))
     (match                           (:background bgaqua))
     (vertical-border                 (:foreground bg1))

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

     ;; line numbers
     (line-number              (:foreground fg :background bg1))
     (line-number-current-line (:inherit 'line-number :bold 't))

     ;; show-paren
     (show-paren-match    (:underline (:weight 'bold)))
     (show-paren-mismatch (:inherit 'error))

     ;; ace-window
     (aw-key-face                     (:inherit 'font-lock-builtin-face))
     (aw-mode-line-face               (:inherit 'mode-line-buffer-id))
     (aw-background-face              (:inherit 'default))
     (aw-minibuffer-leading-char-face (:inherit 'aw-leading-char-face))
     (aw-leading-char-face
      (:background bg1 :foreground fgred))

     ;; avy
     (avy-background-face (:foreground bg))
     (avy-lead-face       (:background fgaqua :foreground bg))
     (avy-lead-face-0     (:background fgblue :foreground bg))
     (avy-lead-face-1     (:background fgpurple :foreground bg))
     (avy-lead-face-2     (:background fggreen :foreground bg))

     ;; circe
     (circe-fool-face               (:foreground fg0))
     (circe-topic-diff-removed-face (:inherit 'diff-removed))
     (circe-topic-diff-new-face     (:inherit 'diff-added))
     (circe-originator-face         (:foreground fg0))
     (circe-my-message-face         (:background bgyellow))
     (circe-highlight-nick-face     (:underline t))
     (circe-server-face             (:background bgblue))
     (circe-prompt-face             (:weight 'bold))

     ;; consult
     (consult-async-running (:foreground fg0))
     (consult-narrow-indicator (:foreground fg1))

     ;; completion-preview
     (completion-preview (:foreground fgyellow))
     (completion-preview-common (:inherit 'completion-preview))
     (completion-preview-exact (:inherit 'completion-preview :underline (:color fgyellow)))

     ;; custom
     (custom-button
      (:box (:line-width 2 :style 'released-button)))
     (custom-button-mouse
      (:box (:line-width 2 :style 'released-button)))
     (custom-button-pressed
      (:box (:line-width 2 :style 'pressed-button) ))
     (custom-button-pressed-unraised (:inherit 'custom-button-unraised))
     (custom-button-unraised (:inherit 'underline))
     (custom-changed (:weight 'bold))
     (custom-comment (:inherit 'default))
     (custom-comment-tag (:foreground fg))
     (custom-documentation nil)
     (custom-face-tag (:inherit 'custom-variable-tag))
     (custom-group-subtitle (:weight 'bold))
     (custom-group-tag (:weight 'bold :foreground fg))
     (custom-group-tag-1 (:weight 'bold :foreground fg))
     (custom-invalid (:inherit 'underline))
     (custom-link (:inherit 'link))
     (custom-modified (:foreground fg :background bg1))
     (custom-rogue (:foreground fg :background bg1))
     (custom-saved (:underline t))
     (custom-set (:foreground fg :background bg))
     (custom-state (:foreground fg0))
     (custom-themed (:foreground fg :background bg1))
     (custom-variable-button (:weight 'bold :underline t))
     (custom-variable-obsolete (:foreground fg))
     (custom-variable-tag (:weight 'bold :foreground fg))
     (custom-visibility (:inherit 'link))

     ;; isearch
     (isearch        (:background bgpurple))
     (lazy-highlight (:background bg1))
     (isearch-fail   (:inherit 'error))
     (isearch-group-1 (:background bgblue))
     (isearch-group-2 (:background bgaqua))

     ;; highlight indent guides
     (highlight-indent-guides-character-face (:inherit 'shadow))

     ;; flymake
     (flymake-error (:inherit 'error))
     (flymake-warning (:inherit 'warning))
     (flymake-note (:background bgblue))

     ;; flyspell
     (flyspell-duplicate (:underline (:color fg0 :style 'line)))
     (flyspell-incorrect (:underline (:color fg0 :style 'line)))

     ;; diff
     (diff-added (:foreground fggreen))
     (diff-changed (:foreground fgblue))
     (diff-changed-unspecified (:foreground fgblue))
     (diff-context (:foreground fg))
     (diff-error (:inherit 'error))
     (diff-file-header (:foreground fg))
     (diff-function (:foreground fg))
     (diff-header (:foreground fg :weight 'bold))
     (diff-hunk-header (:background bg1 :inherit 'diff-header))
     (diff-index (:foreground fg))
     (diff-indicator-added (:inherit 'diff-added))
     (diff-indicator-changed (:inherit 'diff-changed))
     (diff-indicator-removed (:inherit 'diff-removed))
     (diff-refine-added    (:background bggreen :inherit 'diff-added))
     (diff-refine-changed  (:background bgblue :inherit 'diff-changed))
     (diff-refine-removed  (:background bgred :inherit 'diff-removed))
     (diff-removed (:foreground fgred))

     ;; diff-hl
     (diff-hl-change (:foreground fgblue :background fgblue))
     (diff-hl-delete (:foreground fgred :background fgred))
     (diff-hl-insert (:foreground fggreen :background fggreen))

     ;; dired
     (dired-broken-symlink (:inherit 'error))

     ;; eshell
     (eshell-prompt        (:weight 'bold))
     (eshell-ls-archive    (:inherit 'default))
     (eshell-ls-backup     (:inherit 'default))
     (eshell-ls-clutter    (:inherit 'warning))
     (eshell-ls-directory  (:weight 'bold))
     (eshell-ls-executable (:underline t))
     (eshell-ls-missing    (:inherit 'error))
     (eshell-ls-product    (:inherit 'default))
     (eshell-ls-readonly   (:inherit 'default))
     (eshell-ls-special    (:inherit 'default))
     (eshell-ls-symlink    (:inherit 'default))
     (eshell-ls-unreadable (:inherit 'warning))

     ;; Go
     (go-coverage-0 (:background bgred))
     (go-coverage-1 (:background bgred))
     (go-coverage-2 (:background bgorange))
     (go-coverage-3 (:background bgorange))
     (go-coverage-4 (:background bgyellow))
     (go-coverage-5 (:background bgyellow))
     (go-coverage-6 (:background bggreen))
     (go-coverage-7 (:background bggreen))
     (go-coverage-8 (:background bgblue))
     (go-coverage-9 (:background bgblue))
     (go-coverage-10 (:background bgpurple))
     (go-coverage-covered (:background bgpurple))
     (go-coverage-untracked (:background bgred))

     ;; help
     (help-for-help-header (:weight 'bold))
     (help-key-binding (:box (:color fg) :foreground fg :background bg1))

     ;; icon
     (icon-button (:box (:color fg) :foreground fg :background bg2))

     ;; info
     (info-header-node (:weight 'bold))
     (info-menu-star (:foreground fg))
     (info-node (:foreground fg))

     ;; line-number
     (line-number-major-tick (:background bg1))
     (line-number-minor-tick (:foreground bg2))

     ;; log-view
     (log-view-file (:weight 'bold))
     (log-view-message (:weight 'bold))

     ;; nobreak
     (nobreak-hyphen (:inherit 'error))

     ;; popup
     (popup-face (:inherit 'highlight))
     (popup-isearch-match (:inherit 'region))
     (popup-menu-face (:inherit 'highlight))
     (popup-menu-mouse-face (:inherit 'highlight))
     (popup-menu-selection-face (:inherit 'region))
     (popup-menu-summary-face (:inherit 'highlight))
     (popup-scroll-bar-background-face (:inherit 'region))
     (popup-scroll-bar-foreground-face (:inherit 'region))
     (popup-summary-face (:inherit 'highlight))
     (popup-tip-face (:inherit 'highlight))

     ;; tab bar
     (tab-bar                    (:background bg2 :foreground fg))
     (tab-bar-tab                (:weight 'bold :inherit 'tab-bar))
     (tab-bar-tab-inactive       (:inherit 'tab-bar))
     (tab-bar-tab-ungrouped      (:inherit 'tab-bar))
     (tab-bar-tab-group-current  (:inherit 'tab-bar-tab))
     (tab-bar-tab-group-inactive (:inherit 'tab-bar))

     ;; tab-line
     (tab-line (:inherit 'tab-bar))
     (tab-line-close-highlight (:inherit 'error))
     (tab-line-highlight (:inherit 'tab-bar-tab))
     (tab-line-tab-current (:foreground fg2 :inherit 'tab-line-tab))
     (tab-line-tab-inactive (:inherit 'tab-line-tab))
     (tab-line-tab-inactive-alternate (:inherit 'tab-line-tab))

     ;; transient
     (transient-disabled-suffix (:underline (:color fg :style 'line)))
     (transient-enabled-suffix (:underline (:color fg :style 'line)))
     (transient-higher-level (:box (:color fg)))
     (transient-key-recurse (:foreground fg))
     (transient-key-stack (:inherit 'default))
     (transient-mismatched-key (:box (:color fg)))
     (transient-nonstandard-key (:box (:color bg1)))

     ;; tty-menu
     (tty-menu-disabled-face (:inherit 'font-lock-comment-face))
     (tty-menu-enabled-face (:background bg1 :foreground fg))
     (tty-menu-selected-face (:background bg2 :foreground fg))

     ;; widget
     (widget-button-pressed (:inherit 'error))
     (widget-documentation (:foreground fg))
     (widget-field (:background bg1 :foreground fg))
     (widget-single-line-field (:background bg1 :foreground fg))

     ;; wgrep
     (wgrep-reject-face (:background bgyellow))
     (wgrep-face        (:background bgblue))
     (wgrep-done-face   (:background bggreen))
     (wgrep-file-face   (:weight 'bold))
     (wgrep-delete-face (:background bgred))

     ;; lui
     (lui-button-face               (:underline t))
     (lui-time-stamp-face           (:weight 'bold))
     (lui-irc-colors-monospace-face nil)

     ;; magit
     (magit-blame-highlight
      (:extend t :weight 'bold :background bg1))
     (magit-reflog-other                (:foreground fgblue))
     (magit-reflog-remote               (:foreground fgaqua))
     (magit-reflog-cherry-pick          (:foreground fggreen))
     (magit-reflog-rebase               (:foreground fgpurple))
     (magit-reflog-reset                (:foreground fgred))
     (magit-reflog-checkout             (:foreground fgblue))
     (magit-reflog-merge                (:foreground fggreen))
     (magit-reflog-amend                (:foreground fgpurple))
     (magit-reflog-commit               (:foreground fggreen))
     (magit-bisect-bad                  (:foreground fgred))
     (magit-bisect-skip                 (:foreground fgyellow))
     (magit-bisect-good                 (:foreground fggreen))
     (magit-sequence-done               (:foreground fg0))
     (magit-sequence-exec               (:foreground fg0))
     (magit-sequence-onto               (:foreground fg0))
     (magit-sequence-pick               (:foreground fg))
     (magit-sequence-drop               (:foreground fgred))
     (magit-sequence-head               (:foreground fgblue))
     (magit-sequence-part               (:foreground fgyellow))
     (magit-sequence-stop               (:foreground fggreen))
     (magit-cherry-equivalent           (:foreground fgpurple))
     (magit-cherry-unmatched            (:foreground fgaqua))
     (magit-signature-error             (:foreground fgred))
     (magit-signature-revoked           (:foreground fgpurple))
     (magit-signature-expired           (:foreground fgorange))
     (magit-signature-untrusted         (:foreground fgaqua))
     (magit-signature-bad               (:foreground fgred))
     (magit-signature-good              (:foreground fggreen))
     (magit-branch-current              (:foreground fgblue :box (:color fgblue)))
     (magit-branch-remote-head          (:foreground fggreen :box (:color fggreen)))
     (magit-branch-upstream             (:slant 'oblique))
     (magit-branch-local                (:foreground fgblue))
     (magit-branch-remote               (:foreground fggreen))
     (magit-branch-warning              (:background bgyellow))
     (magit-tag                         (:foreground fgyellow))
     (magit-hash                        (:foreground fg0))
     (magit-refname                     (:foreground fg0))
     (magit-log-date                    (:foreground fg0))
     (magit-log-author                  (:foreground fgred))
     (magit-log-graph                   (:foreground fg0))
     (magit-diffstat-removed            (:foreground fgred))
     (magit-diffstat-added              (:foreground fggreen))
     (magit-diff-context                (:foreground fg0))
     (magit-diff-context-highlight      (:background bg1 :foreground fg0))
     (magit-diff-base                   (:foreground fgyellow))
     (magit-diff-base-highlight         (:background bg1 :foreground fgyellow))
     (magit-diff-file-heading           (:inherit 'default))
     (magit-diff-file-heading-highlight (:background bg1 :weight 'bold))
     (magit-diff-removed                (:foreground fgred))
     (magit-diff-removed-highlight      (:background bg1 :foreground fgred))
     (magit-diff-added                  (:foreground fggreen))
     (magit-diff-added-highlight        (:background bg1 :foreground fggreen))
     (magit-diff-hunk-heading           (:background bg1))
     (magit-diff-hunk-heading-highlight
      (:background bg2 :weight 'bold))
     (magit-section-heading-selection   (:background bg :foreground fgyellow))
     (magit-section-heading             (:background bg :foreground fgorange))
     (magit-section-highlight           (:weight 'bold))
     (magit-dimmed                      (:inherit 'shadow))
     (magit-process-ng                  (:foreground fgred))
     (magit-process-ok                  (:foreground fggreen))

     ;; orderless
     (orderless-match-face-0 (:background bgpurple))
     (orderless-match-face-1 (:background bgaqua))
     (orderless-match-face-2 (:background bgorange))
     (orderless-match-face-3 (:background bggreen))

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
     (org-agenda-date-today          (:weight 'bold :inherit 'org-agenda-date))
     (org-agenda-date-weekend-today  (:inherit 'org-agenda-date-today))
     (org-agenda-date-weekend        (:weight 'bold :inherit 'org-agenda-date))
     (org-agenda-diary               (:inherit 'default))
     (org-agenda-dimmed-todo-face    (:foreground fg0))
     (org-agenda-done                (:foreground fggreen))
     (org-agenda-filter-category     (:inherit 'mode-line))
     (org-agenda-filter-effort       (:inherit 'mode-line))
     (org-agenda-filter-regexp       (:inherit 'mode-line))
     (org-agenda-filter-tags         (:inherit 'mode-line))
     (org-agenda-restriction-lock    (:background bg1))
     (org-agenda-structure-filter
      (:inherit  ('org-warning 'org-agenda-structure)))
     (org-agenda-structure           (:foreground fgblue))
     (org-agenda-structure-secondary
      (:background bgyellow :inherit 'org-agenda-structure))
     (org-archived                   (:inherit 'shadow))
     (org-block-begin-line           (:extend t :inherit 'org-meta-line))
     (org-block-end-line             (:extend t :inherit 'org-block-begin-line))
     (org-block                      (:foreground fg0))
     (org-checkbox                   (:inherit 'bold))
     (org-checkbox-statistics-done   (:inherit 'org-done))
     (org-checkbox-statistics-todo   (:inherit 'org-todo))
     (org-cite                       (:inherit 'link))
     (org-cite-key                   (:inherit 'link))
     (org-clock-overlay              (:background bg1))
     (org-code                       (:foreground fg0))
     (org-column-title
      (:weight 'bold :underline t :background bg1))
     (org-column
      (:weight 'normal :slant 'normal :underline nil
               :strike-through nil :background bg1))
     (org-date-selected              (:background bgred))
     (org-date                       (:underline t :foreground fgpurple))
     (org-default                    (:inherit 'default))
     (org-dispatcher-highlight       (:weight 'bold))
     (org-document-info              (:foreground fgblue))
     (org-document-info-keyword      (:inherit 'shadow))
     (org-document-title             (:weight 'bold :foreground fgblue))
     (org-done                       (:weight 'bold :foreground fggreen))
     (org-drawer                     (:foreground fgblue))
     (org-ellipsis                   (:underline t :foreground fgyellow))
     (org-footnote                   (:underline t :foreground fgpurple))
     (org-formula                    (:foreground fgred))
     (org-headline-done              (:foreground fggreen))
     (org-headline-todo              (:foreground fgred))
     (org-hide                       (:foreground bg))
     (org-imminent-deadline          (:inherit 'org-warning))
     (org-inline-src-block           (:inherit 'org-block))
     (org-latex-and-related          (:foreground fgyellow))
     (org-level-1                    (:weight 'bold))
     (org-level-2                    (:weight 'bold))
     (org-level-3                    (:weight 'bold))
     (org-level-4                    (:weight 'bold))
     (org-level-5                    (:weight 'bold))
     (org-level-6                    (:weight 'bold))
     (org-level-7                    (:weight 'bold))
     (org-level-8                    (:weight 'bold))
     (org-link                       (:inherit 'link))
     (org-list-dt                    (:weight 'bold))
     (org-macro                      (:inherit 'org-latex-and-related))
     (org-meta-line                  (:inherit 'font-lock-comment-face))
     (org-mode-line-clock            (:inherit 'mode-line))
     (org-mode-line-clock-overrun    (:background bgred :foreground fg))
     (org-priority                   (:inherit 'font-lock-keyword-face))
     (org-property-value             nil)
     (org-quote                      (:inherit 'org-block))
     (org-scheduled                  (:foreground fggreen))
     (org-scheduled-previously       (:foreground fgred))
     (org-scheduled-today            (:foreground fggreen))
     (org-sexp-date                  (:foreground fgpurple))
     (org-special-keyword            (:inherit 'font-lock-keyword-face))
     (org-table                      (:foreground fgblue))
     (org-table-header               (:foreground fg :background bg1))
     (org-tag-group                  (:inherit 'org-tag))
     (org-tag                        (:weight 'bold))
     (org-target                     (:underline t))
     (org-time-grid                  (:foreground fgyellow))
     (org-todo                       (:weight 'bold :foreground fgred))
     (org-upcoming-deadline          (:foreground fgred))
     (org-upcoming-distant-deadline  (:inherit 'org-default))
     (org-verbatim                   (:inherit 'shadow))
     (org-verse                      (:inherit 'org-block))
     (org-warning                    (:inherit 'font-lock-warning-face))

     ;; eglot
     (eglot-inlay-hint-face       (:foreground fg0 :background bg1 :height 0.8))
     (eglot-highlight-symbol-face (:underline t))
     (eglot-mode-line nil)

     ;; sh-mode
     (sh-escaped-newline (:inherit 'font-lock-string-face))
     (sh-quoted-exec     (:foreground fg))
     (sh-heredoc         (:foreground fg0))

     ;; Window dividers
     (window-divider             (:foreground bg3))
     (window-divider-last-pixel  (:foreground bg3))
     (window-divider-first-pixel (:foreground bg3))

     ;; completions
     (completions-common-part (:foreground fgblue))

     ;; markdown
     (markdown-pre-face (:inherit 'shadow))
     (markdown-inline-code-face (:foreground fg0))
     (markdown-highlighting-face (:background bgyellow))

     ;; evil
     (evil-ex-substitute-replacement (:underline t :foreground fgred))

     ;; transient
     (transient-key-exit (:foreground fgred))
     (transient-key-return (:foreground fgyellow))
     (transient-key-noop (:foreground fg0))
     (transient-key-stay (:foreground fggreen))
     (transient-key (:inherit 'font-lock-builtin-face))

     ;; neo-tree
     (neo-banner-face              (:foreground fgpurple))
     (neo-button-face              (:underline nil))
     (neo-dir-link-face            (:foreground fgblue))
     (neo-expand-btn-face          (:foreground fgaqua))
     (neo-file-link-face           (:foreground fg))
     (neo-header-face              (:foreground fgpurple))
     (neo-root-dir-face            (:foreground fgpurple))
     (neo-vc-added-face            (:foreground fggreen))
     (neo-vc-conflict-face         (:foreground fgred))
     (neo-vc-default-face          (:foreground fg))
     (neo-vc-edited-face           (:foreground fgpurple))
     (neo-vc-ignored-face          (:foreground fg0))
     (neo-vc-missing-face          (:foreground fgred))
     (neo-vc-needs-merge-face      (:foreground fgred))
     (neo-vc-needs-update-face     (:underline t))
     (neo-vc-removed-face          (:strike-through t))
     (neo-vc-unlocked-changes-face (:background bgblue))
     (neo-vc-unregistered-face     (:foreground fg))
     (neo-vc-up-to-date-face       (:foreground fg0))
     (neo-vc-user-face             (:slant 'italic :foreground fg2))

     ;; compilation-mode
     (compilation-mode-line-exit (:foreground fggreen))
     (compilation-mode-line-fail (:background bgred))
     (compilation-mode-line-run (:background bgyellow))

     )))

(provide 'my-simple-theme-base)
;;; my-simple-theme-base.el ends here
