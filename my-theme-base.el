(require 'autothemer)

(defmacro my-theme-deftheme (name description palette &rest body)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette

    (;; basics
     (default (:background bg :foreground fg))

     (cursor              (:background fg0))
     (fringe              (:background bg0_s))
     (highlight           (:background bg1))
     (hl-line             (:background bg0_s))
     (link                (:foreground blue :underline t))
     (link-visited        (:foreground purple :underline t))
     (minibuffer-prompt   (:foreground blue))
     (mode-line           (:background bg3))
     (mode-line-inactive  (:background bg1))
     (region              (:background bg1))
     (secondary-selection (:background bg1))
     (shadow              (:foreground fg2))

     ;; Built in syntax
     (font-lock-builtin-face       (:foreground red))
     (font-lock-constant-face      (:foreground purple))
     (font-lock-comment-face       (:foreground fg2))
     (font-lock-function-name-face (:foreground yellow))
     (font-lock-keyword-face       (:foreground red))
     (font-lock-string-face        (:foreground green))
     (font-lock-number-face        (:foreground blue))
     (font-lock-variable-name-face (:foreground blue))
     (font-lock-type-face          (:foreground purple))
     (font-lock-warning-face       (:foreground red))

     ;; Basic faces
     (error               (:foreground red))
     (success             (:foreground green))
     (warning             (:foreground yellow))
     (trailing-whitespace (:background bright-red :foreground bg0))
     (escape-glyph        (:foreground aqua))
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
     (whitespace-space-after-tab  (:background red :foreground bg0))
     (whitespace-space-before-tab (:background red :foreground bg0))
     (whitespace-tab              (:inherit 'default))
     (whitespace-trailing         (:background red :foreground bg0))

     ;; RainbowDelimiters
     (rainbow-delimiters-depth-1-face   (:foreground orange))
     (rainbow-delimiters-depth-2-face   (:foreground yellow))
     (rainbow-delimiters-depth-3-face   (:foreground green))
     (rainbow-delimiters-depth-4-face   (:foreground blue))
     (rainbow-delimiters-depth-5-face   (:foreground purple))
     (rainbow-delimiters-depth-6-face   (:foreground orange))
     (rainbow-delimiters-depth-7-face   (:foreground yellow))
     (rainbow-delimiters-depth-8-face   (:foreground green))
     (rainbow-delimiters-depth-9-face   (:foreground blue))
     (rainbow-delimiters-depth-10-face  (:foreground purple))
     (rainbow-delimiters-depth-11-face  (:foreground orange))
     (rainbow-delimiters-depth-12-face  (:foreground yellow))
     (rainbow-delimiters-unmatched-face (:background red :foreground bg0))

     ;; line numbers
     (line-number              (:background bg0_s :foreground fg2))
     (line-number-current-line (:background bg0_s :foreground fg:bold 't))

     ;; show-paren
     (show-paren-match    (:underline t))
     (show-paren-mismatch (:background red :foreground bg0))

     ;; ace-window
     (aw-key-face                     (:inherit 'font-lock-builtin-face))
     (aw-mode-line-face               (:inherit 'mode-line-buffer-id))
     (aw-background-face              (:foreground bg))
     (aw-minibuffer-leading-char-face (:inherit 'aw-leading-char-face))
     (aw-leading-char-face            (:background bg4 :foreground orange))

     ;; avy
     (avy-background-face (:foreground bg))
     (avy-lead-face       (:background orange :foreground bg))
     (avy-lead-face-0     (:background green :foreground bg))
     (avy-lead-face-1     (:background yellow :foreground bg))
     (avy-lead-face-2     (:background cyan :foreground bg))

     ;; circe
     (circe-fool-face               (:foreground fg2))
     (circe-topic-diff-removed-face (:inherit 'diff-removed))
     (circe-topic-diff-new-face     (:inherit 'diff-added))
     (circe-originator-face         nil)
     (circe-my-message-face         nil)
     (circe-highlight-nick-face     (:weight 'bold :foreground purple))
     (circe-server-face             (:foreground blue))
     (circe-prompt-face             (:weight 'bold :foreground aqua :background bg1))

     ;; isearch
     (isearch        (:background bg3))
     (lazy-highlight (:background bg2))
     (isearch-fail   (:foreground red))

     ;; highlight indent guides
     (highlight-indent-guides-character-face (:foreground yellow))

     ;; flyspell
     (flyspell-duplicate (:underline (:color aqua :style 'wave)))
     (flyspell-incorrect (:underline (:color red :style 'wave)))

     ;; diff-hl
     (diff-hl-change (:foreground blue :background blue))
     (diff-hl-delete (:foreground red :background red))
     (diff-hl-insert (:foreground green :background green))

     ;; dired
     ;; TODO Start here down
     (dired-broken-symlink (:background red :foreground bg0))

     ;; eshell
     (eshell-prompt        (:foreground green))
     (eshell-ls-backup     (:foreground fg))
     (eshell-ls-archive    (:foreground fg))
     (eshell-ls-clutter    (:foreground fg))
     (eshell-ls-missing    (:foreground fg))
     (eshell-ls-product    (:foreground fg))
     (eshell-ls-special    (:foreground fg))
     (eshell-ls-symlink    (:foreground green))
     (eshell-ls-readonly   (:foreground fg))
     (eshell-ls-directory  (:foreground orange))
     (eshell-ls-executable (:foreground violet))
     (eshell-ls-unreadable (:foreground fg))

     ;; tab bar
     (tab-bar                    (:background bg2 :foreground fg))
     (tab-bar-tab                (:foreground cyan :weight 'bold))
     (tab-bar-tab-inactive       (:foreground fg))
     (tab-bar-tab-ungrouped      (:foreground fg))
     (tab-bar-tab-group-current  (:foreground bg2 :weight 'bold))
     (tab-bar-tab-group-inactive (:foreground fg))

     ;; wgrep
     (wgrep-reject-face (:foreground red))
     (wgrep-face        (:background green :foreground bg0))
     (wgrep-done-face   (:foreground blue))
     (wgrep-file-face   (:background green :foreground bg0))
     (wgrep-delete-face (:background red :foreground bg0))

     ;; hydra
     (hydra-face-red      (:foreground red))
     (hydra-face-blue     (:foreground blue))
     (hydra-face-amaranth (:foreground violet))
     (hydra-face-pink     (:foreground magenta))
     (hydra-face-teal     (:foreground cyan))

     ;; lui
     (lui-button-face               (:underline t :foreground green))
     (lui-time-stamp-face           (:weight 'bold :foreground violet))
     (lui-irc-colors-monospace-face nil)

     ;; magit
     (magit-blame-highlight             (:extend t :foreground bg1 :background grey2))
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
     (magit-sequence-head               (:foreground grey2))
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
     (magit-refname-pullreq             (:foreground grey2))
     (magit-branch-upstream             (:slant 'italic))
     (magit-branch-local                (:foreground grey2))
     (magit-branch-remote               (:foreground green))
     (magit-tag                         (:foreground yellow))
     (magit-hash                        (:foreground grey1))
     (magit-refname                     (:foreground yellow))
     (magit-log-date                    (:foreground yellow))
     (magit-log-author                  (:foreground red))
     (magit-log-graph                   (:foreground grey2))
     (magit-diffstat-removed            (:foreground orange))
     (magit-diffstat-added              (:foreground green))
     (magit-diff-context                (:foreground grey2))
     (magit-diff-context-highlight      (:background bg1 :foreground fg))
     (magit-diff-base                   (:background yellow :foreground bg0))
     (magit-diff-base-highlight         (:background yellow :foreground bg0))
     (magit-diff-removed                (:background bg0 :foreground red))
     (magit-diff-removed-highlight      (:background bg1 :foreground red))
     (magit-diff-added                  (:background bg0 :foreground green))
     (magit-diff-added-highlight        (:background bg1 :foreground green))
     (magit-diff-hunk-heading           (:background bg3 :foreground grey2 :weight 'bold))
     (magit-diff-hunk-heading-highlight (:background bg3 :foreground fg :weight 'bold))
     (magit-section-heading-selection   (:foreground red))
     (magit-section-heading             (:weight 'bold :foreground red))
     (magit-section-highlight           (:background bg1))
     (magit-dimmed                      (:foreground grey1))

     ;; orderless
     (orderless-match-face-0 (:foreground blue))
     (orderless-match-face-1 (:foreground violet))
     (orderless-match-face-2 (:foreground green))
     (orderless-match-face-3 (:foreground yellow))

     ;; ansi
     (ansi-color-black          (:foreground grey2 :background grey2))
     (ansi-color-red            (:foreground red :background red))
     (ansi-color-green          (:foreground green :background green))
     (ansi-color-yellow         (:foreground yellow :background yellow))
     (ansi-color-blue           (:foreground blue :background blue))
     (ansi-color-magenta        (:foreground magenta :background magenta))
     (ansi-color-cyan           (:foreground cyan :background cyan))
     (ansi-color-white          (:foreground grey1 :background grey1))
     (ansi-color-bright-black   (:foreground grey2 :background grey2))
     (ansi-color-bright-red     (:foreground red :background red))
     (ansi-color-bright-green   (:foreground green :background green))
     (ansi-color-bright-yellow  (:foreground yellow :background yellow))
     (ansi-color-bright-blue    (:foreground blue :background blue))
     (ansi-color-bright-magenta (:foreground magenta :background magenta))
     (ansi-color-bright-cyan    (:foreground cyan :background cyan))
     (ansi-color-bright-white   (:foreground bg1 :background bg1))

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
     (org-agenda-dimmed-todo-face    (:foreground fg))
     (org-agenda-done                (:foreground green))
     (org-agenda-filter-category     (:inherit 'mode-line))
     (org-agenda-filter-effort       (:inherit 'mode-line))
     (org-agenda-filter-regexp       (:inherit 'mode-line))
     (org-agenda-filter-tags         (:inherit 'mode-line))
     (org-agenda-restriction-lock    (:background cyan :foreground bg0))
     (org-agenda-structure-filter    (:inherit  ('org-warning 'org-agenda-structure)))
     (org-agenda-structure           (:foreground blue))
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
     (org-code                       (:foreground green))
     (org-column-title               (:weight 'bold :underline t :background bg1))
     (org-column                     (:weight 'normal :slant 'normal :underline nil :strike-through nil :background bg1))
     (org-date-selected              (:inverse-video t :foreground red))
     (org-date                       (:underline t :foreground violet))
     (org-default                    (:inherit 'default))
     (org-dispatcher-highlight       (:weight 'bold :foreground bg0 :background blue))
     (org-document-info              (:foreground grey2))
     (org-document-info-keyword      (:inherit 'shadow))
     (org-document-title             (:weight 'bold :foreground blue))
     (org-done                       (:weight 'bold :foreground green))
     (org-drawer                     (:foreground blue))
     (org-ellipsis                   (:underline t :foreground yellow))
     (org-footnote                   (:underline t :foreground violet))
     (org-formula                    (:foreground red))
     (org-headline-done              (:foreground grey1))
     (org-headline-todo              (:foreground red))
     (org-hide                       (:foreground bg0))
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
     (org-table-header               (:foreground blue :background bg1 :inherit 'org-table))
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
     (eglot-inlay-hint-face       (:foreground grey1 :background bg1 :height 0.8))
     (eglot-highlight-symbol-face (:underline t))

     ;; sh-mode
     (sh-escaped-newline (:inherit 'font-lock-string-face))
     (sh-quoted-exec     (:foreground violet))
     (sh-heredoc         (:foreground yellow))

     ;; Window dividers
     (window-divider             (:foreground bg4))
     (window-divider-last-pixel  (:foreground bg4))
     (window-divider-first-pixel (:foreground bg4))

     ;; Corfu
     (corfu-bar          (:background grey2))
     (corfu-border       (:background bg5))
     (corfu-current      (:inverse-video t))
     (corfu-default      (:background bg1))
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
     (diff-refine-added    (:background bg_green :foreground green))
     (diff-refine-changed  (:background bg_blue :foreground blue))
     (diff-refine-removed  (:background bg_red :foreground red))

     ;; markdown
     (markdown-pre-face (:foreground fg))
     (markdown-inline-code-face (:foreground magenta))

     ;; doom modeline
     (doom-modeline-panel (:foreground blue))

     ;; evil
     (evil-ex-substitute-replacement (:underline t :foreground red))
     ))

  (provide-theme 'my-theme-base)
