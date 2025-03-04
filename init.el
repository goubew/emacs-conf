(use-package ansible
  :ensure t
  :init
  (defun my-should-ansible ()
    "Check if ansible should be enabled"
    (when (string-match "/playbooks/.*\.ya?ml\\'" (buffer-file-name))
      (ansible-mode)))
  :hook (yaml-mode . my-should-ansible))

(use-package ansible-doc
  :ensure t
  :hook (ansible-mode . ansible-doc-mode))

(use-package anzu
  :ensure t
  :after evil
  :config (global-anzu-mode))

(use-package app-launcher
  :if (daemonp)
  :init
  ;; Sourced from https://gitlab.com/dwt1/configuring-emacs/-/blob/main/07-the-final-touches/scripts/app-launchers.el
  (defun my-emacs-run-launcher ()
    "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
    (interactive)
    (with-selected-frame
        (make-frame '((name . "emacs-run-launcher")
                      (minibuffer . only)
                      (fullscreen . 0) ; no fullscreen
                      (undecorated . t) ; remove title bar
                      (internal-border-width . 10)
                      (width . 80)
                      (height . 11)))
      (unwind-protect
          (app-launcher-run-app)
        (delete-frame)))))

(use-package autothemer
  :ensure t
  :config
  (add-hook 'after-init-hook (lambda () (load-theme 'my-gruvbox-light t))))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("C-c w" . ace-window)))

(use-package avy
  :ensure t
  :bind ("C-c j" . avy-goto-char-timer))

(use-package cape
  :ensure t
  :after consult
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package c-ts-mode
  :defer t
  :config
  (c-set-offset 'case-label '+)
  (require 'project)
  (defun my-swap-h-c-file ()
    "Finds the associated .h or .c file in the current project"
    (interactive)
    (let ((file-name (file-name-nondirectory (buffer-file-name))))
      (let ((related-file
             (cond
              ((string-match "\\.h\\'" file-name)
               (concat "^" (regexp-quote (replace-regexp-in-string "\\.h\\'" ".c" file-name)) "$"))
              ((string-match "\\.c\\'" file-name)
               (concat "^" (regexp-quote (replace-regexp-in-string "\\.c\\'" ".h" file-name)) "$"))
              )))
        (if related-file
            (let ((found-file-list (directory-files-recursively (project-root (project-current)) related-file nil)))
              (if (> (length found-file-list) 0)
                  (find-file (nth 0 found-file-list))
                (message "Associated file not found in project")))
          (message "File does not end in .c or .h")))))
  :bind (:map c-ts-mode-map
              ("C-c m o" . my-swap-h-c-file)))

(use-package circe
  :ensure t
  :init
  (setq circe-network-options
        '(("Libera Chat"
           :nick "wag"
           :nickserv-password
           (lambda (server) (read-passwd "Enter the SAML password=>")))))
  :commands 'circe)

(use-package completion-preview
  :hook ((prog-mode text-mode) . completion-preview-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c b" . consult-buffer)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c y" . consult-yank-pop)
         ("C-c B" . consult-bookmark)
         ("C-c o e" . consult-compile-error)
         ("C-c o f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("C-c o g" . consult-goto-line)             ;; orig. goto-line
         ("C-c o k" . consult-kmacro)
         ("C-c o o" . consult-outline)               ;; Alternative: consult-org-heading
         ("C-c o m" . consult-mark)
         ("C-c o k" . consult-global-mark)
         ("C-c o i" . consult-imenu)
         ("C-c o I" . consult-imenu-multi)
         ("C-c i m" . consult-man)
         ("C-c i r" . consult-recent-file)
         ("C-c i i" . consult-info)
         ("C-c R" . consult-register-store)
         ("C-c M-r" . consult-register)
         ("C-c r" . consult-register-load)
         ("C-c s d" . consult-find)                  ;; Alternative: consult-fd
         ("C-c s e" . consult-isearch-history)
         ("C-c s c" . consult-locate)
         ("C-c s g" . consult-grep)
         ("C-c s G" . consult-git-grep)
         ("C-c s r" . consult-ripgrep)
         ("C-c s l" . consult-line)
         ("C-c s L" . consult-line-multi)
         ("C-c s k" . consult-keep-lines)
         ("C-c s u" . consult-focus-lines)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package corfu
  :ensure t
  :hook (prog-mode . global-corfu-mode)
  :custom
  (corfu-on-exact-match 'insert)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-quit-at-boundary 'separator)
  (corfu-left-margin-width 0)
  :bind (:map evil-insert-state-map
        ("C-n" . 'completion-at-point)
        ("C-p" . 'completion-at-point)
        :map corfu-map
        ("C-l" . 'corfu-complete)
        ("C-e" . 'corfu-quit)
        ("<return>" . 'corfu-insert)))

(use-package css-mode
  :defer t
  :config
  (defun my-css-mode ()
    "My css mode settings"
    (setq css-indent-offset 2))
  (add-hook 'css-mode-hook 'my-css-mode))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode vc-dir-mode ledger-mode) . diff-hl-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package dockerfile-mode
  :ensure t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (column-number-mode 1)
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-minor-modes nil))

(use-package dumb-jump
  :ensure t
  :after evil
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eat
  :ensure t
  :init
  (setq eat-enable-shell-prompt-annotation nil)
  (setq eat-kill-buffer-on-exit t)
  (defun my-eat-new ()
    (interactive)
    (let ((current-prefix-arg
           (gethash "id"
                    (json-parse-string
                     (shell-command-to-string "hyprctl activeworkspace -j")))))
      (call-interactively 'eat)))
  :bind (("C-c a t" . eat)
         :map eat-semi-char-mode-map
         ("C-u" . eat-self-input)))

;; Make sure models are pre-pulled in ollama
(use-package ellama
  :ensure t
  :bind ("C-c ?" . ellama-transient-main-menu)
  :init
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "mistral-nemo:12B"
           :embedding-model "nomic-embed-text"))
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:14b"
           :embedding-model "nomic-embed-text"))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-words)
  :config
  (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))

(use-package elec-pair
  :defer t
  :hook (prog-mode . electric-pair-local-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :defer t
  :custom
  (scroll-margin 2)
  (use-short-answers t) ; Faster prompts
  (visible-cursor nil)
  (ring-bell-function 'ignore) ; SILENCE
  (help-window-select t) ; Autofocus help windows
  (inhibit-startup-screen t) ; No splash screen
  (backup-directory-alist '(("." . "~/.saves"))) ; Backups in different dir
  (auto-save-file-name-transforms '((".*" "~/.saves/" t))) ; auto saves in different dir
  (ispell-program-name "aspell") ; Use aspell for flyspell
  (vc-follow-symlinks t) ; Follow symlinks
  (enable-recursive-minibuffers t) ; Recommended by vertico
  (recentf-max-menu-items 25) ; Set recent file limit
  (recentf-max-saved-items 25) ; Set recent file limit
  (scroll-conservatively 33); Adjust recent sensitivity
  (split-width-threshold 200); Only split horizontally for 200 cols
  (frame-title-format "Emacs")
  (tab-always-indent 'complete)
  (x-underline-at-descent-line t)
  :init
  ;; Set default buffer local vars
  (setq-default indent-tabs-mode nil); indent with spaces
  (setq-default tab-width 4) ; Display width of tab char

  (add-to-list 'same-window-buffer-names "*compilation*") ; Run compile commands in current window

  (add-hook 'after-init-hook (lambda()
                               (global-auto-revert-mode nil)
                               (save-place-mode 1)
                               (recentf-mode 1)))
  (add-hook 'prog-mode-hook (lambda()
                              (display-line-numbers-mode 1)
                              (flyspell-prog-mode)))
  (add-hook 'text-mode-hook 'flyspell-mode)

  ;; Terminal mode has a menu-bar too
  (menu-bar-mode -1)
  (when (or (display-graphic-p) (daemonp))
    (blink-cursor-mode 0)
    (tool-bar-mode -1)
    (scroll-bar-mode 0)

    ;; Enable window dividers
    (setq window-divider-default-places t); Make vertical and horizontal window dividers)
    (setq window-divider-default-right-width 1)
    (setq window-divider-default-bottom-width 1)
    (window-divider-mode t)

    (defun my-set-font ()
      "Sets the font"
      (interactive)
      (if (not (null (x-list-fonts "Iosevka")))
          (set-frame-font "Iosevka 14" nil t)
        (if (eq system-type 'darwin)
            (set-frame-font "SF Mono Light 19" nil t)
          (progn
            (when (not (null (x-list-fonts "Droid Sans Mono")))
              (set-frame-font "Droid Sans Mono 14" nil t))
            (when (not (null (x-list-fonts "SF Mono")))
              (set-frame-font "SF Mono Light 13" nil t))))))

    (if (daemonp)
        (add-hook 'server-after-make-frame-hook #'my-set-font)
      (my-set-font)))

  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'control))

  (defun count-paragraphs (start end)
    "Return number of paragraphs between START and END."
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (- (buffer-size) (forward-paragraph (buffer-size))))))

  (defun count-paragraphs-region-or-buffer ()
    "Report number of paragraphs in the region (if it's active) or the entire buffer."
    (declare (interactive-only count-paragraphs))
    (interactive)
    (let ((paragraphs (if (use-region-p)
                          (count-paragraphs (region-beginning) (region-end))
                        (count-paragraphs (point-min) (point-max)))))
      (message "%s has %d paragraph%s"
               (if (use-region-p) "Region" "Buffer")
               paragraphs
               (if (> paragraphs 1) "s" ""))))

  (defun my-current-filename ()
    "Copy the full path of the current file and write it to the minibuffer"
    (interactive)
    (let ((bufname (buffer-file-name (window-buffer (minibuffer-selected-window)))))
      (kill-new bufname)
      (message bufname)))

  (defun my-eval-and-run-all-tests-in-buffer ()
    "Delete all loaded tests from the runtime, evaluate the current buffer and run all loaded tests with ert."
    (interactive)
    (ert-delete-all-tests)
    (eval-buffer)
    (ert 't))

  (defun my-switch-to-previous-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer)))

  :bind (("C-c =" . indent-region)
         :map minibuffer-local-map
         ("C-w" . backward-kill-word)))

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :custom
  (evil-insert-state-message nil)
  (evil-normal-state-message nil)
  (evil-motion-state-message nil)
  (evil-visual-state-message nil)
  (evil-replace-state-message nil)
  (evil-operator-state-message nil)
  (evil-respect-visual-line-mode t)
  (evil-search-module 'evil-search)
  (evil-ex-search-persistent-highlight nil)
  (evil-symbol-word-search t)
  (evil-shift-width 2)
  (evil-undo-system 'undo-fu)
  (evil-want-C-d-scroll t)
  (evil-want-C-i-jump t)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-delete t)
  (evil-want-keybinding nil)
  :init
  (defun my-emulate-ctrl-c ()
    (interactive)
    (setq  unread-command-events (nconc (listify-key-sequence (kbd "C-c")) unread-command-events)))
  :bind (("C-c s c" . 'evil-ex-nohighlight)
         :map evil-normal-state-map
         ("SPC" . 'my-emulate-ctrl-c)))

(use-package evil-anzu
  :ensure t
  :after (anzu evil))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'circe-mode 'emacs)
  (evil-set-initial-state 'eat-mode 'emacs))

(use-package evil-numbers
  :ensure t
  :after evil
  :config
  ;; Avoid using C-a C-x to keep the default C-x bindings
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package eglot
  :init
  (setq eldoc-echo-area-use-multiline-p nil)
  :commands 'eglot
  :bind (("C-c l e" . eglot)
         ("C-c l a" . eglot-code-actions)
         ("C-c l d" . xref-find-definitions)
         ("C-c l h" . eldoc)
         ("C-c l o" . eglot-code-action-organize-imports)
         ("C-c l r" . eglot-rename)
         ("C-c l u" . xref-find-references)))

(use-package expand-region
  :ensure t
  :after evil
  :bind (:map evil-visual-state-map
         ("," . er/contract-region)
         ("." . er/expand-region)))

(use-package flymake
  :bind (("C-c e n" . flymake-goto-next-error)
         ("C-c e p" . flymake-goto-prev-error)))

(use-package flyspell-correct
  :ensure t
  :bind ("C-C e s" . flyspell-correct-wrapper))

(use-package git-timemachine
  :ensure t
  :commands 'git-timemachine)

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode))

(use-package groovy-mode
  :ensure t
  :mode ("\\.\\(groovy\\|gradle\\)\\'" . groovy-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook (yaml-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-method 'bitmap))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package java-ts-mode
  :defer t
  :custom
  (java-ts-mode-indent-offset 2))

(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package js
  :defer t
  :init
  (setq js-indent-level 2))

(use-package key-chord
  :ensure t
  :after evil
  :config
  (defun my-evil-insert-exit ()
    (interactive)
    "Exit evil's insert mode and save the buffer if it has a buffer-file-name"
    (evil-normal-state)
    (when (buffer-file-name) (save-buffer)))
  (key-chord-define evil-insert-state-map "fd" 'my-evil-insert-exit)
  (key-chord-mode 1))

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" . ledger-mode)
  :init
  (setq ledger-binary-path "/usr/local/bin/ledger")
  :config
  (defun my--find-expense-matches ()
    "finds want or need Expenses: in the buffer to complete"
    (delete-dups (save-match-data
                   (let ((pos 0) (string (buffer-string)) matches)
                     (while (string-match "Expenses:[WN][^ ]*" string pos)
                       (push (match-string-no-properties 0 string) matches)
                       (setq pos (match-end 0)))
                     matches)))
    )
  (defun my-new-ledger-entry ()
    "Enter a new ledger entry based on the Expenses: keyword of a previous entry"
    (interactive)
    (goto-char (point-max))
    (insert "\n")
    (let ((cost (read-string "Enter the price =>")))
      (insert (replace-regexp-in-string "-" "/" (org-read-date)))
      (insert " ")
      (insert (read-string "Enter the expense title =>"))
      (insert "\n    ")
      (insert (completing-read "Enter the expense: " (my--find-expense-matches)))
      (insert "  $")
      (insert cost)
      (insert "\n    Assets:Checking\n")))
  (defun my-ledger-mode ()
    "My ledger mode settings"
    (setq tab-width 4
          indent-line-function 'insert-tab
          electric-indent-local-mode -1))
  (add-hook 'ledger-mode-hook 'my-ledger-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c v" . magit-status))
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-diff-refine-hunk t))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia readme recommends immediate loading
  (marginalia-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :defer t
  :custom
  (org-adapt-indentation nil)
  (org-agenda-files '("~/org/"))
  (org-edit-src-content-indentation 0)
  (org-goto-interface 'outline-path-completion)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-outline-path-complete-in-steps nil)
  (org-src-window-setup t)
  (visual-fill-column-width 90)
  (visual-fill-column-center-text t)
  :config
  (defun my-org-header-link ()
    "Uses org-goto to prompt for a heading and creates a link"
    (interactive)
    (let ((org-header
           (save-excursion
             (org-goto)
             (replace-regexp-in-string "\*+" "*"
                                       (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position))))))
    (insert "[[")
    (insert org-header)
    (insert "][")
    (insert
     (read-string "Enter link description =>" (replace-regexp-in-string "^\* *" "" org-header)))
    (insert "]]"))))

(use-package ox-ascii
  :defer t
  :custom
  (org-ascii-global-margin 0)
  (org-ascii-inner-margin 0)
  (org-ascii-text-width 65))

(use-package project
  :defer t
  :init
  (global-set-key (kbd "C-c p") project-prefix-map)
  (setq project-vc-extra-root-markers '(".projectile"))
  :config
  (define-key project-prefix-map "s" #'consult-ripgrep)
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep") t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :commands 'rainbow-mode)

(use-package sh-script
  :defer t
  :custom
  (sh-basic-offset 2))

(use-package tab-bar
  :defer t
  :custom
  (tab-bar-separator "|")
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :hook (after-init . global-treesit-auto-mode)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-u" . kill-whole-line))
  :hook (after-init . vertico-mode))

(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package wgrep
  :ensure t
  :after project
  :custom
  (wgrep-auto-save-buffer t))

(use-package xclip
  :unless (display-graphic-p)
  :ensure t
  :hook (init-mode . xclip-mode))

(use-package yaml-mode
  :ensure t
  :init
  (add-hook 'yaml-mode-hook (lambda() (display-line-numbers-mode 1)))
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package zone-nyan
  :ensure t
  :commands 'zone-nyan-preview)

;; Load customizations if they exist
(when (file-exists-p custom-file) (load-file custom-file))

;; Open the init file
(find-file (concat user-emacs-directory "init.el"))
