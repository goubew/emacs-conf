;; -------------------
;; use-package macro packages
;; -------------------

(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-c")
  (general-def
    "M-u" 'universal-argument)
  (general-def
   :states 'normal
   "]c" 'next-error
   "[c" 'previous-error)
  (defun my-buffer-delete ()
    "Prompts to delete the window attached to a buffer when more than one window exists"
    (interactive)
    (progn
      (kill-this-buffer)
      (when (> (count-windows) 1)
        (when (y-or-n-p "Close window?") (delete-window)))))
  (my-leader-def
    "a" '(:ignore t :which-key "applications")
    "b" '(:ignore t :which-key "buffers")
    "c" '(:ignore t :which-key "conflicts")
    "e" '(:ignore t :which-key "errors/elisp")
    "f" '(:ignore t :which-key "file")
    "g" '(:ignore t :which-key "git")
    "h" '(:ignore t :which-key "hydra")
    "i" '(:ignore t :which-key "info")
    "j" '(:ignore t :which-key "jump")
    "l" '(:ignore t :which-key "lsp")
    "L" '(:ignore t :which-key "lisp")
    "m" '(:ignore t :which-key "mode")
    "p" '(:ignore t :which-key "project")
    "s" '(:ignore t :which-key "search")
    "t" '(:ignore t :which-key "toggle/tab")
    "q" '(:ignore t :which-key "query ai")
    "ab" 'bookmark-set
    "ac" 'calc
    "bB" 'switch-to-buffer
    "bd" 'my-buffer-delete
    "bs" 'scratch-buffer
    "bt" 'tab-bar-switch-to-tab
    "cn" 'smerge-next
    "cp" 'smerge-prev
    "cm" 'smerge-keep-mine
    "cb" 'smerge-keep-base
    "cu" 'smerge-keep-upper
    "cl" 'smerge-keep-lower
    "ee"  'eval-expression
    "ff" 'find-file
    "fs" 'save-buffer
    "jb" 'bookmark-jump
    "Le" 'eval-expression
    "Ls" 'eval-last-sexp
    "Lr" 'eval-region
    "M"  'describe-mode
    "t1" (lambda() (interactive) (tab-select 1))
    "t2" (lambda() (interactive) (tab-select 2))
    "t3" (lambda() (interactive) (tab-select 3))
    "t4" (lambda() (interactive) (tab-select 4))
    "t5" (lambda() (interactive) (tab-select 5))
    "t6" (lambda() (interactive) (tab-select 6))
    "t7" (lambda() (interactive) (tab-select 7))
    "t8" (lambda() (interactive) (tab-select 8))
    "t9" (lambda() (interactive) (tab-select 9))
    "tl" 'display-line-numbers-mode
    "tm" 'toggle-frame-maximized
    "tw" 'toggle-truncate-lines
    "ts" 'flyspell-mode
    "u"  'universal-argument
    "x"  'execute-extended-command))

;; --------
;; Packages
;; --------

(use-package ansible
  :ensure t
  :after yaml-mode
  :init
  (defun my-should-ansible ()
    "Check if ansible should be enabled"
    (when (string-match "/playbooks/.*\.ya?ml\\'" (buffer-file-name))
      (ansible)))
  (add-hook 'yaml-mode-hook 'my-should-ansible))

(use-package ansible-doc
  :ensure t
  :after ansible)

(use-package anzu
  :ensure t
  :after evil
  :config
  (global-anzu-mode))

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
        (delete-frame))))
  (my-leader-def "aa" 'my-emacs-run-launcher))

(use-package autothemer
  :ensure t
  :config
  (defun my-reload-theme ()
    "Reloads the theme to test changes"
    (interactive)
    (disable-theme 'desert-light)
    (load-theme 'desert-light t))
  (my-leader-def "T" 'my-reload-theme)
  (add-hook 'after-init-hook (lambda () (load-theme 'desert-light t))))

(use-package ace-window
  :general
  ("C-x o" 'ace-window))

(use-package avy
  :ensure t
  :general
  (my-leader-def
    "SPC" 'avy-goto-char
    "jj"  'avy-goto-char
    "jl"  'avy-goto-line
    "jw"  'avy-goto-word-0))

(use-package cape
  :ensure t
  :after consult
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package cc-mode
  :defer t
  :config
  (c-set-offset 'case-label '+)
  (with-eval-after-load 'project
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
    (my-leader-def
      :keymaps 'c-mode-map
      "mo" 'my-swap-h-c-file)))

(use-package circe
  :ensure t
  :init
  (setq circe-network-options
        '(("Libera Chat"
           :nick "wag"
           :nickserv-password
           (lambda (server) (read-passwd "Enter the SAML password=>")))))
  :general
  (my-leader-def
    "aC" 'circe))

(use-package consult
  :ensure t
  :after vertico
  :config
  (my-leader-def
    "bb" 'consult-buffer
    "ec" 'consult-flymake
    "fr" 'consult-recent-file
    "pb" 'consult-project-buffer
    "ps" 'consult-ripgrep
    "sl" 'consult-line
    "sL" 'consult-line-multi
    "sm" 'consult-mark
    "si" 'consult-imenu
    "sI" 'consult-imenu-multi
    "so" 'consult-outline
    "y"  'consult-yank-from-kill-ring))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :init
  (setq corfu-on-exact-match 'insert)
  (setq corfu-cycle t)
  (setq corfu-preview-current 'insert)
  (setq corfu-preselect 'prompt)
  (setq corfu-quit-at-boundary 'separator)
  (setq corfu-left-margin-width 0)
  :config
  (general-def
   :states 'insert
   "C-n" 'completion-at-point
   "C-p" 'completion-at-point)
  (general-def
    :keymaps 'completion-in-region-mode
    :definer 'minor-mode
    :states 'insert
    :predicate 'corfu-mode
    "C-n" 'corfu-next
    "C-p" 'corfu-previous
    "C-l" 'corfu-complete
    "C-e" 'corfu-quit
    "<return>" 'corfu-insert))

(use-package css-mode
  :defer t
  :config
  (defun my-css-mode ()
    "My css mode settings"
    (setq css-indent-offset 2))
  (add-hook 'css-mode-hook 'my-css-mode))

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :hook ((prog-mode vc-dir-mode ledger-mode) . diff-hl-mode))

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
  (my-leader-def
    "at" 'eat
    "pt" 'eat-project)
  (defun my-eat-new ()
    (interactive)
    (let ((current-prefix-arg
           (gethash "id"
                    (json-parse-string
                     (shell-command-to-string "hyprctl activeworkspace -j")))))
      (call-interactively 'eat)))
  (general-def
    :keymaps 'eat-semi-char-mode-map
    "C-u" 'eat-self-input
    "C-6" 'evil-switch-to-windows-last-buffer))

(use-package elec-pair
  :defer t
  :hook (prog-mode . electric-pair-local-mode))

(use-package embark
  :ensure t
  :general
  ("C-." 'embark-act)
  ("C-;" 'embark-dwim)
  ("C-h B" 'embark-bindings)
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
  :init
  ;; Set default buffer local vars
  (setq-default indent-tabs-mode nil); indent with spaces
  (setq-default tab-width 4) ; Display width of tab char

  (setq scroll-margin 2)
  (setq use-short-answers t) ; Faster prompts
  (setq visible-cursor nil)
  (setq ring-bell-function 'ignore) ; SILENCE
  (setq help-window-select t) ; Autofocus help windows
  (setq inhibit-startup-screen t) ; No splash screen
  (setq backup-directory-alist '(("." . "~/.saves"))) ; Backups in different dir
  (setq auto-save-file-name-transforms '((".*" "~/.saves/" t))) ; auto saves in different dir
  (setq ispell-program-name "aspell") ; Use aspell for flyspell
  (setq vc-follow-symlinks t) ; Follow symlinks
  (setq enable-recursive-minibuffers t) ; Recommended by vertico
  (setq recentf-max-menu-items 25) ; Set recent file limit
  (setq recentf-max-saved-items 25) ; Set recent file limit
  (setq scroll-conservatively 33); Adjust recent sensitivity
  (setq split-width-threshold 200); Only split horizontally for 200 cols
  (setq frame-title-format "Emacs")

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

  (my-leader-def "if" 'my-current-filename))

(use-package eshell
  :general
  (my-leader-def "as" 'eshell)
  :config
  (general-def
   :keymaps 'eshell-mode-map
   "C-6" 'evil-switch-to-windows-last-buffer))

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :init
  (setq evil-insert-state-message nil)
  (setq evil-normal-state-message nil)
  (setq evil-motion-state-message nil)
  (setq evil-visual-state-message nil)
  (setq evil-replace-state-message nil)
  (setq evil-operator-state-message nil)

  (setq evil-respect-visual-line-mode t)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-persistent-highlight nil)
  (setq evil-symbol-word-search t)
  (setq evil-shift-width 2)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-delete t)
  (setq evil-want-keybinding nil)
  :config
  ; Set initial emacs states in the evil-collection config
  (my-leader-def "sc" 'evil-ex-nohighlight))

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
  (evil-set-initial-state 'mingus-playlist-mode 'emacs)
  (evil-set-initial-state 'mingus-help-mode 'emacs)
  (evil-set-initial-state 'mingus-browse-mode 'emacs)
  (evil-set-initial-state 'eat-mode 'emacs))

(use-package evil-escape
  :ensure t
  :after evil
  :init
  (setq evil-escape-inhibit-functions '(evil-emacs-state-p))
  (setq evil-escape-key-sequence "fd")
  (setq evil-escape-delay 0.5)
  :config
  (add-hook 'evil-mode-hook (lambda () (evil-escape-mode 1))))

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
  (add-hook 'evil-mode-hook (lambda () (global-evil-surround-mode 1))))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package eglot
  :init
  (setq eldoc-echo-area-use-multiline-p nil)
  :general
  (my-leader-def "le" 'eglot)
  :config
  (my-leader-def
    "la" 'eglot-code-actions
    "ld" 'xref-find-definitions
    "lh" 'eldoc
    "lo" 'eglot-code-action-organize-imports
    "lr" 'eglot-rename
    "lu" 'xref-find-references))

(use-package expand-region
  :ensure t
  :general
  (general-def
   :states '(visual)
   "," 'er/contract-region
   "." 'er/expand-region))

(use-package flymake
  :defer t
  :init
  (setq flymake-no-changes-timeout 5)
  (setq flymake-start-on-save-buffer t)
  (my-leader-def
    "ee" 'flymake-mode)
  :config
  (my-leader-def
    "el" 'flymake-show-buffer-diagnostics
    "eL" 'flymake-show-project-diagnostics
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error))

(use-package git-timemachine
  :ensure t
  :general
  (my-leader-def
    "gt" 'git-timemachine))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode))

(use-package gptel
  :ensure t
  :general
  (my-leader-def
    "qq" 'gptel
    "qs" 'gptel-send
    "qm" 'gptel-menu)
  :config
  (general-def
    :keymaps 'gtpel-mode-map
    "C-<return>" 'gtpel-send
    "C-RET" 'gtpel-send)
  (gptel-make-openai ;Not a typo, same API as OpenAI
   "llama-cpp"
   :stream t
   :protocol "http"
   :host "localhost:8080"
   :models '("dummy")))

(use-package groovy-mode
  :ensure t
  :mode ("\\.\\(groovy\\|gradle\\)\\'" . groovy-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook (yaml-mode . highlight-indent-guides-mode)
  :general
  (my-leader-def "ti" 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-method 'bitmap))

(use-package helpful
  :ensure t
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-c C-d" 'helpful-at-point)
  ("C-h F" 'helpful-function)
  ("C-h C" 'helpful-command))

(use-package hl-line
  :defer t
  :init
  (my-leader-def
    "th" 'hl-line-mode)
  ; https://stackoverflow.com/posts/40572675/revisions
  (defvar-local was-hl-line-mode-on nil)
  (defun hl-line-on-maybe ()  (if was-hl-line-mode-on (hl-line-mode +1)))
  (defun hl-line-off-maybe () (if was-hl-line-mode-on (hl-line-mode -1)))
  (add-hook 'hl-line-mode-hook
            (lambda () (if hl-line-mode (setq was-hl-line-mode-on t))))
  (add-hook 'evil-visual-state-entry-hook 'hl-line-off-maybe)
  (add-hook 'evil-visual-state-exit-hook 'hl-line-on-maybe))

(use-package hydra
  :ensure t
  :general
  (my-leader-def
    "hd" 'hydra-diff-hl/body
    "hs" 'hydra-smerge/body)
  :config
  (defhydra hydra-smerge
    (:color red :hint nil
            :pre (smerge-mode 1))
    "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase _R_efine _<_: base-mine
_p_rev _m_ine _E_diff _=_: mine-other
^ ^ _o_ther _C_ombine _>_: base-other
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
"
    ("RET" smerge-keep-current)
    ("C" smerge-combine-with-next)
    ("E" smerge-ediff)
    ("R" smerge-refine)
    ("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("m" smerge-keep-mine)
    ("n" smerge-next)
    ("o" smerge-keep-other)
    ("p" smerge-prev)
    ("r" smerge-resolve)
    ("<" smerge-diff-base-mine)
    ("=" smerge-diff-mine-other)
    (">" smerge-diff-base-other)
    ("q" nil :color blue))
  (defhydra hydra-diff-hl
    (:color orange :hint nil)
    "
^Hunk Nav^
_j_: next
_k_: prev
"
    ("j" diff-hl-next-hunk)
    ("k" diff-hl-previous-hunk)))

(use-package imenu-list
  :ensure t
  :general
  (my-leader-def "tI" 'imenu-list-smart-toggle))

(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package js
  :defer t
  :init
  (setq js-indent-level 2))

(use-package kubernetes
  :ensure t
  :general
  (my-leader-def "ak" 'kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600)
  (setq kubernetes-redraw-frequency 3600))

(use-package kubernetes-evil
  :ensure t
  :after (evil kubernetes))

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" . ledger-mode)
  :init
  (setq ledger-binary-path "/usr/local/bin/ledger")
  :config
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
      (insert (completing-read "Enter the expense: "
                               (delete-dups (save-match-data
                                              (let ((pos 0) (string (buffer-string)) matches)
                                                (while (string-match "Expenses:[WN][^ ]*" string pos)
                                                  (push (match-string-no-properties 0 string) matches)
                                                  (setq pos (match-end 0)))
                                                matches)))))
      (insert "  $")
      (insert cost)
      (insert "\n    Assets:Checking\n")))
  (my-leader-def
    :keymaps 'ledger-mode-map
    "mc" 'ledger-mode-clean-buffer
    "mn" 'my-new-ledger-entry
    "mk" 'ledger-copy-transaction-at-point)
  (defun my-ledger-mode ()
    "My ledger mode settings"
    (setq tab-width 4)
    (setq indent-line-function 'insert-tab)
    (setq evil-auto-indent nil)
    (electric-indent-local-mode -1))
  (add-hook 'ledger-mode-hook 'my-ledger-mode))

(use-package magit
  :ensure t
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk t)
  :general
  (my-leader-def
    "gs" 'magit-status
    "gb" 'magit-blame))

(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package mingus
  :ensure t
  :general
  (my-leader-def "am" 'mingus))

(use-package orderless
  :ensure t
  :after vertico
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :defer t
  :init
  (setq org-adapt-indentation nil)
  (setq org-agenda-files '("~/org/"))
  (setq org-edit-src-content-indentation 0)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-src-window-setup t)
  (setq visual-fill-column-width 90)
  (setq visual-fill-column-center-text t)
  :config
  (defun my-org-meta-return ()
    (interactive)
    (move-end-of-line nil)
    (org-meta-return)
    (evil-append))
  (general-def
   :states '(normal)
   :keymaps 'org-mode-map
   "M-<return>" 'my-org-meta-return
   "M-RET" 'my-org-meta-return
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)
  (my-leader-def
    :keymaps 'org-mode-map
    "mj" 'org-goto
    "mo" 'org-open-at-point
    "mle" 'org-insert-link
    "mln" 'my-org-header-link
    "mtl" 'org-toggle-link-display
    "mtv" 'visual-line-mode)
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
    (insert (read-string "Enter link description =>" (replace-regexp-in-string "^\* *" "" org-header)))
    (insert "]]")))
  (defun my-org-mode ()
    (electric-indent-local-mode -1)
    (setq-local evil-cross-lines t) ; Make horizontal movement cross lines
    )
  (add-hook 'org-mode-hook 'my-org-mode))

(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode))

(use-package ox-ascii
  :defer t
  :init
  (setq org-ascii-global-margin 0)
  (setq org-ascii-inner-margin 0)
  (setq org-ascii-text-width 65))

(use-package project
  :defer t
  :init
  (setq project-vc-extra-root-markers '(".projectile"))
  ;; buffer switch handled by consult
  (my-leader-def
    "pc" 'project-compile
    "pf" 'project-find-file
    "pp" 'project-switch-project)
  :config
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  (define-key project-prefix-map "s" #'consult-ripgrep)
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep") t))

(use-package powershell
  :ensure t
  :mode
  ("\\.pwsh\\'" . powershell-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ranger
  :ensure t
  :init
  (setq ranger-show-hidden t)
  :general
  (my-leader-def "ar" 'ranger)
  :config
  (ranger-override-dired-mode t))

(use-package sh-script
  :defer t
  :init
  (setq sh-basic-offset 2))

(use-package tab-bar
  :init
  (setq tab-bar-separator "|")
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-button-show nil)
  :general
  (my-leader-def "tT" 'tab-bar-mode))

(use-package treemacs
  :ensure t
  :init
  (setq treemacs-no-png-images t)
  :general
  (my-leader-def "tt" 'treemacs))

(use-package treemacs-evil
  :ensure t
  :after (evil treemacs))

(use-package undo-fu
  :ensure t
  :after evil)

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (general-def
   :keymaps 'vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous
   :keymaps 'minibuffer-local-map
   "C-h" 'backward-kill-word
   "C-w" 'backward-kill-word
   "C-u" 'kill-whole-line))

(use-package visual-fill-column
  :ensure t
  :hook
  (visual-line-mode . visual-fill-column-mode))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (push '((nil . "lambda") . t) which-key-replacement-alist))

(use-package wgrep
  :ensure t
  :after project
  :init
  (setq wgrep-auto-save-buffer t))

(use-package xclip
  :unless (display-graphic-p)
  :ensure t
  :hook (init-mode . xclip-mode))

(use-package yaml-mode
  :ensure t
  :init
  (add-hook 'yaml-mode-hook (lambda()
                              (display-line-numbers-mode 1)))
  :mode
  ("\\.ya?ml\\'" . yaml-mode))

(use-package yasnippet
  :ensure t
  :general
  (my-leader-def "ty" 'yas-minor-mode)
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))

(use-package zone-nyan
  :ensure t
  :general
  (my-leader-def "tn" 'zone-nyan-preview))

;; Load customizations if they exist
(when (file-exists-p custom-file) (load-file custom-file))

;; Open the init file
(find-file "~/.emacs.d/init.el")
