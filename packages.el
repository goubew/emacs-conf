(use-package ansible
  :ensure t
  :init
  (load-file (concat user-emacs-directory "funs/ansible-funs.el"))
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
  :config
  (load-file (concat user-emacs-directory "funs/app-launcher-funs.el"))
)

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
  :init
  (setq avy-single-candidate-jump nil)
  :bind (("C-c j" . avy-goto-char-timer)
         ("M-n" . avy-goto-char-timer)))

(use-package cape
  :ensure t
  :after consult
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package circe
  :ensure t
  :init
  (setq circe-network-options
        '(("Libera Chat"
           :nick "wag"
           :nickserv-password
           (lambda (server) (read-passwd "Enter the SAML password=>")))))
  :commands 'circe)

(use-package consult
  :ensure t
  :bind (([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
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
        xref-show-definitions-function #'consult-xref)
  :config
  (setq completion-in-region-function #'consult-completion-in-region))

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
  (setq inhibit-compacting-font-caches t
        doom-modeline-icon nil
        doom-modeline-minor-modes nil))

(use-package dumb-jump
  :ensure t
  :after evil
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eat
  :ensure t
  :init
  (setq eat-enable-shell-prompt-annotation nil
        eat-kill-buffer-on-exit t)
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

(use-package evil
  :ensure t
  :commands evil-mode
  :init
  (setq
   evil-default-state 'emacs
   evil-insert-state-message nil
   evil-normal-state-message nil
   evil-motion-state-message nil
   evil-visual-state-message nil
   evil-replace-state-message nil
   evil-operator-state-message nil
   evil-respect-visual-line-mode t
   evil-search-module 'evil-search
   evil-ex-search-persistent-highlight nil
   evil-symbol-word-search t
   evil-shift-width 2
   evil-undo-system 'undo-redo
   evil-want-C-d-scroll t
   evil-want-C-i-jump t
   evil-want-C-u-delete t
   evil-want-C-u-scroll t
   evil-want-C-w-delete t
   evil-want-keybinding nil)
  :config
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (load-file (concat user-emacs-directory "funs/evil-funs.el"))
  :bind (("C-c s c" . 'evil-ex-nohighlight)
         :map evil-normal-state-map
         ("SPC" . 'my-emulate-ctrl-c)))

(use-package evil-anzu
  :ensure t
  :after (anzu evil))

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
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :after evil
  :bind (:map evil-visual-state-map
         ("," . er/contract-region)
         ("." . er/expand-region)))

(use-package flyspell-correct
  :ensure t
  :bind ("C-C e s" . flyspell-correct-wrapper))

(use-package git-timemachine
  :ensure t
  :commands 'git-timemachine)

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
  (key-chord-define evil-insert-state-map "fd" 'evil-normal-state)
  (key-chord-mode 1))

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" . ledger-mode)
  :init
  (setq ledger-binary-path "/usr/local/bin/ledger")
  :config
  (load-file (concat user-emacs-directory "funs/ledger-funs.el"))
  (add-hook 'ledger-mode-hook 'my-ledger-mode-settings))

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

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :commands 'rainbow-mode)

(use-package treesit-auto
  :ensure t
  :init
  (setq treesit-auto-install 'prompt)
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
