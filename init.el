;; -------------------
;; use-package macro packages
;; -------------------

(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (general-def
   :states 'insert
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
    "e" '(:ignore t :which-key "errors")
    "f" '(:ignore t :which-key "file")
    "g" '(:ignore t :which-key "git")
    "h" '(:ignore t :which-key "hydra")
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
    "bb" 'switch-to-buffer
    "bd" 'my-buffer-delete
    "bs" 'scratch-buffer
    "cn" 'smerge-next
    "cp" 'smerge-prev
    "cm" 'smerge-keep-mine
    "cb" 'smerge-keep-base
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

(use-package ag
  :ensure t
  :general
  (my-leader-def "as" 'ag)
  :init
  (setq ag-highlight-search t))

(use-package anzu
  :ensure t
  :after evil
  :config
  (global-anzu-mode))

(use-package autothemer
  :ensure t
  :config
  (defun my-reload-theme ()
      "Reloads the theme to test changes"
    (interactive)
    (disable-theme 'my-solarized-light)
    (load-theme 'my-solarized-light t))
  (my-leader-def "T" 'my-reload-theme)
  (add-hook 'after-init-hook (lambda () (load-theme 'my-solarized-light t))))

(use-package avy
  :ensure t
  :general
  (my-leader-def
    "SPC" 'avy-goto-char
    "jj"  'avy-goto-char
    "jl"  'avy-goto-line
    "jw"  'avy-goto-word-0))

(use-package cc-mode
  :defer t
  :config
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
              ;(project-find-file-in related-file (list (project-root (project-current))) (project-current nil))
              (let ((found-file-list (directory-files-recursively (project-root (project-current)) related-file nil)))
                (if (> (length found-file-list) 0)
                    (find-file (nth 0 found-file-list))
                  (message "Associated file not found in project")))
            (message "File does not end in .c or .h")))))
    (my-leader-def
      :keymaps 'c-mode-map
      "mo" 'my-swap-h-c-file)))

(use-package cape
  :ensure t
  :after consult
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package consult
  :ensure t
  :after vertico
  :config
  (my-leader-def
    "bB" 'consult-buffer
    "ec" 'consult-flymake
    "fr" 'consult-recent-file
    "pb" 'consult-project-buffer
    "ps" 'consult-git-grep
    "sl" 'consult-line
    "sL" 'consult-line-multi
    "sm" 'consult-mark
    "si" 'consult-imenu
    "so" 'consult-outline))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :init
  (setq corfu-on-exact-match 'show)
  (setq corfu-cycle t)
  (setq corfu-preview-current 'insert)
  (setq corfu-preselect 'prompt)
  (setq corfu-quit-at-boundary 'separator)
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

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-minor-modes nil))

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
  (setq scroll-conservatively 101); Do not recenter after scrolling offscreen

  (add-to-list 'same-window-buffer-names "*compilation*") ; Run compile commands in current window

  (add-hook 'after-init-hook (lambda()
                               (global-auto-revert-mode nil)
                               (save-place-mode 1)
                               (recentf-mode 1)))
  (add-hook 'prog-mode-hook (lambda()
                              (display-line-numbers-mode 1)
                              (hl-line-mode)
                              (flyspell-prog-mode)))
  (add-hook 'text-mode-hook 'hl-line-mode)

  ;; Terminal mode has a menu-bar too
  (menu-bar-mode -1)
  (when (display-graphic-p)
    (blink-cursor-mode 0)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1)

    ;; Enable window dividers
    (setq window-divider-default-places t); Make vertical and horizontal window dividers)
    (setq window-divider-default-right-width 1)
    (setq window-divider-default-bottom-width 1)
    (window-divider-mode t)

    ;; Set font
    (if (not (null (x-list-fonts "QuadLemon")))
        (set-frame-font "QuadLemon" nil t)
      (if (eq system-type 'darwin)
          (set-frame-font "SF Mono Light 18" nil t)
        (progn
          (when (not (null (x-list-fonts "Droid Sans Mono")))
            (set-frame-font "Droid Sans Mono 14" nil t))
          (when (not (null (x-list-fonts "SF Mono")))
            (set-frame-font "SF Mono Light 13" nil t))))))

  ;; Fringe icons do not scale at high DPI
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=31203
  (when (>= (display-pixel-width) 3840)
    (fringe-mode 0))

  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'control)))

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
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (my-leader-def "sc" 'evil-ex-nohighlight))

(use-package evil-anzu
  :ensure t
  :after (anzu evil))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (add-hook 'evil-mode-hook (lambda () (evil-collection-init))))

(use-package evil-escape
  :ensure t
  :after evil
  :init
  (setq evil-escape-key-sequence "fd")
  (setq evil-escape-delay 0.5)
  (setq evil-escape-excluded-major-modes '(vterm-mode))
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
  (my-leader-def
    "ee" 'flymake-mode)
  :config
  (my-leader-def
    "el" 'flymake-show-buffer-diagnostics
    "eL" 'flymake-show-project-diagnostics
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error))

(use-package git-gutter-fringe
  :if (display-graphic-p)
  :ensure t
  :config
  ;; Stolen from doom emacs
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  ;; The fringe bitmaps only seem to work when package loading isn't defered
  (add-hook 'after-init-hook 'global-git-gutter-mode))

(use-package git-timemachine
  :ensure t
  :general
  (my-leader-def
    "gt" 'git-timemachine))

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

(use-package hydra
  :ensure t
  :general
  (my-leader-def "hs" 'hydra-smerge/body)
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
    ("q" nil :color blue)))

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
  (general-def
   :keymaps 'ledger-mode-map
   "DEL" 'my-backward-delete-whitespace-to-column)
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

(use-package orderless
  :ensure t
  :after vertico
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

;; TODO: Make org mode use cape-dabbrev completions instead of pcomplete
(use-package org
  :defer t
  :init
  (setq org-agenda-files '("~/org/"))
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-adapt-indentation nil)
  (setq evil-cross-lines t) ; Make horizontal movement cross lines
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq visual-fill-column-width 90)
  (setq visual-fill-column-center-text t)
  :config
  (defun my-org-meta-return ()
    "Moves the curor to the end of the line and calls org-meta-return"
    (interactive)
    (move-end-of-line nil)
    (org-meta-return))
  (general-def
   :states '(normal insert)
   :keymaps 'org-mode-map
   "M-<return>" 'my-org-meta-return
   "M-RET" 'my-org-meta-return)
  (my-leader-def
    :keymaps 'org-mode-map
    "mj" 'org-goto
    "mo" 'org-open-at-point
    "mle" 'org-insert-link
    "mln" 'my-org-header-link
    "mtl" 'org-toggle-link-display)
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
    (visual-line-mode 1))
  (add-hook 'org-mode-hook 'my-org-mode))

(use-package origami
  :ensure t
  :hook (prog-mode . origami-mode))

(use-package ox-ascii
  :defer t
  :init
  (setq org-ascii-global-margin 0)
  (setq org-ascii-text-width 70))

(use-package project
  :defer t
  :init
  ;; buffer switch handled by consult
  (my-leader-def
    "pc" 'project-compile
    "pf" 'project-find-file
    "pp" 'project-switch-project))

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

(use-package simple-mpc
  :ensure t
  :general
  (my-leader-def "am" 'simple-mpc))

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
  (with-eval-after-load 'winum
    (my-leader-def "0" 'treemacs-select-window))
  :general
  (my-leader-def "tt" 'treemacs))

(use-package treemacs-evil
  :ensure t
  :after (evil treemacs))

(use-package undo-fu
  :ensure t
  :after evil)

; TODO Readd vertico-repeat loading
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

(use-package vterm
  :ensure t
  :init
  ;; https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-811850431
  (defun my-evil-normal-in-vterm-copy-mode ()
    (if (bound-and-true-p vterm-copy-mode)
        (evil-normal-state)
      (evil-emacs-state)))
  (add-hook 'vterm-mode-hook 'evil-emacs-state)
  (add-hook 'vterm-copy-mode-hook 'my-evil-normal-in-vterm-copy-mode)
  (setq vterm-disable-bold-font t)
  :general
  (my-leader-def "at" 'vterm)
  :config
  (general-def
   :keymaps 'vterm-mode-map
   "C-u" 'vterm-send-C-u
   "C-6" 'evil-switch-to-windows-last-buffer))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package wgrep
  :ensure t
  :after project
  :init
  (setq wgrep-auto-save-buffer t))

(use-package wgrep-ag
  :ensure t
  :after project)

(use-package winum
  :ensure t
  :hook (after-init . winum-mode)
  :config
  (my-leader-def
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9)
  ;Remove winum from which-key
  (with-eval-after-load 'which-key
    (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)))

(use-package xclip
  :unless (display-graphic-p)
  :ensure t
  :hook (init-mode . xclip-mode))

(use-package yaml-mode
  :ensure t
  :mode
  ("\\.ya?ml\\'" . yaml-mode))

(use-package yasnippet
  :ensure t
  :general
  (my-leader-def "ty" 'yas-minor-mode)
  :hook
  (prog-mode-hook . yas-minor-mode))

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
