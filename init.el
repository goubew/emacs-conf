;; ---------------
;; Global Settings
;; ---------------

;; Disable UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(setq-default indent-tabs-mode nil); indent with spaces
(setq-default tab-width 4) ; Display width of tab char
(setq ring-bell-function 'ignore) ; SILENCE
(setq help-window-select t) ; Autofocus help windows
(setq inhibit-startup-screen t) ; No splash screen
(setq mac-command-modifier 'control) ; Command key remap
(setq backup-directory-alist '(("." . "~/.saves"))) ; Backups in different dir
(setq auto-save-file-name-transforms '((".*" "~/.saves/" t))) ; auto saves in different dir
(setq ispell-program-name "aspell") ; Use aspell for flyspell
(setq vc-follow-symlinks t) ; Follow symlinks
(setq enable-recursive-minibuffers t) ; Recommended by vertico
(setq recentf-max-menu-items 25) ; Set recent file limit
(setq recentf-max-saved-items 25) ; Set recent file limit
(setq scroll-conservatively 100); Do not recenter after scrolling offscreen
(add-to-list 'same-window-buffer-names "*compilation*") ; Run compile commands in current window

;; Fonts
(defun my-check-if-font-exists (font)
  "Check if a font is installed"
  (if (null (x-list-fonts font)) nil t))

(defun my-set-font ()
  "Sets the font to SF Mono, Droid Sans Mono, or Terminus as appropriate."
  (interactive)
  (when (display-graphic-p)
    (progn
      (when (my-check-if-font-exists "Droid Sans Mono 13")
        (set-frame-font "Droid Sans Mono 14" nil t))
      (when (my-check-if-font-exists "SF Mono")
        (set-frame-font "SF Mono Light 18" nil t))
      (when (<= (display-pixel-width) 1280)
        (when (my-check-if-font-exists "Terminus")
          (set-frame-font "Terminus 12" nil t))))))

;; Enable modes
(defun my-init ()
  (global-auto-revert-mode nil) ; Keep buffers when files change on disk
  (blink-cursor-mode 0) ; Don't blink
  (save-place-mode 1) ; Save place in files
  (my-set-font) ; Set the right font
  (recentf-mode 1) ; Enable recent file capturing
  (if (eq system-type 'darwin) (toggle-frame-maximized)) ; Maximixe the window on MacOS
  )

; https://stackoverflow.com/a/40572675
(defvar-local was-hl-line-mode-on nil)
(defun hl-line-on-maybe ()  (if was-hl-line-mode-on (hl-line-mode +1)))
(defun hl-line-off-maybe () (if was-hl-line-mode-on (hl-line-mode -1)))
(add-hook 'hl-line-mode-hook
          (lambda () (if hl-line-mode (setq was-hl-line-mode-on t))))
  
(add-hook 'after-init-hook 'my-init)
(add-hook 'prog-mode-hook (lambda ()
                            (electric-pair-mode 1) ; Autoclose brackets
                            (display-line-numbers-mode 1);
                            (hl-line-mode)))
(add-hook 'text-mode-hook 'hl-line-mode)

;; vim softtab-stop equivalent
;; https://stackoverflow.com/a/1450454
(defun my-backward-delete-whitespace-to-column ()
  "Delete back to the previous column of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char)
    (let ((movement (% (current-column) tab-width))
          (p (point)))
      (when (= movement 0) (setq movement tab-width))
      (save-match-data
        (if (string-match "\\w*\\(\\s-+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))
(define-key prog-mode-map (kbd "DEL") 'my-backward-delete-whitespace-to-column)
(define-key emacs-lisp-mode-map (kbd "DEL") 'my-backward-delete-whitespace-to-column)

;; Faster prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; -------------------
;; Early Package Setup
;; -------------------

(setq straight-use-package-by-default t) ; Adds :straight to use-package transparently
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
      (error "Straight.el not installed. Clone https://github.com/radian-software/straight.el.git into ~/.emacs.d/straight/repos/"))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ; Install use-package

(use-package general
  :config
  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
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
    "t" '(:ignore t :which-key "toggle")
    "x" 'execute-extended-command
    "ab" 'bookmark-set
    "ac" 'calc
    "bb" 'switch-to-buffer
    "bd" 'my-buffer-delete
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
    "tm" 'toggle-frame-maximized
    "tw" 'toggle-truncate-lines
    "ts" 'flyspell-mode
    "tT" 'tab-bar-mode))

;; --------
;; Packages
;; --------

(use-package ag
  :general
  (my-leader-def "as" 'ag)
  :init
  (setq ag-highlight-search t))

(use-package autothemer
  :config
  (add-hook 'after-init-hook (lambda () (load-theme 'my-solarized-light t)))
  )

(use-package avy
  :general
  (my-leader-def
    "jj" 'evil-avy-goto-char
    "jl" 'evil-avy-goto-line
    "jw" 'evil-avy-goto-word-1))

(use-package cc-mode
  :straight nil
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
  :after consult
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package consult
  :after vertico
  :config
  (my-leader-def
    "bB" 'consult-buffer
    "ec" 'consult-flymake
    "fr" 'consult-recent-file
    "ps" 'consult-git-grep
    "sl" 'consult-line
    "sL" 'consult-line-multi
    "sm" 'consult-mark
    "si" 'consult-imenu
    "so" 'consult-outline)
  (setq completion-in-region-function 'consult-completion-in-region)
  (general-define-key
   :states 'insert
   "C-n" 'completion-at-point
   "C-p" 'completion-at-point))

(use-package css-mode
  :straight nil
  :defer t
  :config
  (defun my-css-mode ()
    "My css mode settings"
    (setq css-indent-offset 2))
  (add-hook 'css-mode-hook 'my-css-mode))

(use-package doom-modeline
  :init
  (setq inhibit-compacting-font-caches t)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-minor-modes nil)
  :config
  ;; skipped modeline segments
  ;;
  ;; workspace-name - no workspace packages currently
  ;; modals - editing state not needed
  ;; matches - requires anzu, evil-ex-substitute, and symbol-overlay
  ;; word count - don't need to count words
  ;; parrot - cute, but not needed
  ;; objed-state  - seems interesting, but not using atm
  ;; persp-mode - not using
  ;; battery - I use battery in system bar
  ;; grip - seems cool, but not using
  ;; irc mu4e gnus - maybe someday
  ;; github - not modeline worthy
  ;; input-method - (using) not entirely sure what this does
  ;; indent-info - it will annoy me instantly if it's wrong already
  ;; process - (using) not sure what this does
  ;; vcs - info in magit
  (doom-modeline-def-modeline 'my-doom-modeline
    '(bar window-number buffer-info remote-host buffer-position selection-info)
    '(misc-info debug repl lsp minor-modes input-method buffer-encoding major-mode process checker))
  (defun my-setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-doom-modeline 'default))
  (add-hook 'doom-modeline-mode-hook 'my-setup-custom-doom-modeline)
  (doom-modeline-mode t))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold nil)
  (setq doom-themes-enable-italic nil)
  ;(add-hook 'after-init-hook (lambda () (load-theme 'doom-solarized-light-custom t))))
)

(use-package embark
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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package eshell
  :straight nil
  :general
  (my-leader-def "as" 'eshell)
  :config
  (general-define-key
   :keymaps 'eshell-mode-map
   "C-6" 'evil-switch-to-windows-last-buffer))

(use-package evil
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
  (my-leader-def "sc" 'evil-ex-nohighlight)
  (add-hook 'evil-visual-state-entry-hook 'hl-line-off-maybe)
  (add-hook 'evil-visual-state-exit-hook 'hl-line-on-maybe))

(use-package evil-collection
  :after evil
  :config
  (add-hook 'evil-mode-hook (lambda () (evil-collection-init))))

(use-package evil-escape
  :after evil
  :init
  (setq evil-escape-key-sequence "fd")
  (setq evil-escape-delay 0.5)
  (setq evil-escape-excluded-major-modes '(vterm-mode))
  :config
  (add-hook 'evil-mode-hook (lambda () (evil-escape-mode 1))))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :after evil
  :config
  (add-hook 'evil-mode-hook (lambda () (global-evil-surround-mode 1))))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package eglot
  :straight nil
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
  :general
  (general-define-key
   :states '(visual)
   "," 'er/contract-region
   "." 'er/expand-region))

(use-package flymake
  :straight nil
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
  :config
  ; Stolen from doom emacs
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  ; The fringe bitmaps only seem to work when package loading isn't defered
  (add-hook 'after-init-hook 'global-git-gutter-mode))

(use-package git-timemachine
  :general
  (my-leader-def
    "gt" 'git-timemachine))

(use-package groovy-mode
  :mode ("\\.groovy\\'" . groovy-mode))

(use-package highlight-indent-guides
  :hook (yaml-mode . highlight-indent-guides-mode)
  :general
  (my-leader-def "ti" 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-method 'bitmap))

(use-package helpful
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-c C-d" 'helpful-at-point)
  ("C-h F" 'helpful-function)
  ("C-h C" 'helpful-command))

(use-package hydra
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

(use-package jinja2-mode
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package js
  :straight nil
  :defer t
  :config
  (defun my-js-mode ()
    "My js mode settings"
    (setq js-indent-level 2))
  (add-hook 'js-mode-hook 'my-js-mode))

(use-package kubernetes
  :general

  (my-leader-def "ak" 'kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600)
  (setq kubernetes-redraw-frequency 3600))

(use-package kubernetes-evil
  :after (evil kubernetes))

(use-package ledger-mode
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
  (general-define-key
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
  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :general
  (my-leader-def
    "gs" 'magit-status
    "gb" 'magit-blame))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

;; TODO: Make org mode use cape-dabbrev completions instead of pcomplete
(use-package org
  :straight nil
  :defer t
  :config
  (my-leader-def
    :keymaps 'org-mode-map
    "mo" 'org-open-at-point
    "mle" 'org-insert-link
    "mln" 'my-org-header-link
    "mj" 'org-goto
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
    (setq org-adapt-indentation nil)
    (setq evil-cross-lines t) ; Make horizontal movement cross lines
    (setq org-goto-interface 'outline-path-completion)
    (setq org-outline-path-complete-in-steps nil)
    (setq visual-fill-column-width 90)
    (setq visual-fill-column-center-text t)
    (electric-indent-local-mode -1)
    (visual-line-mode 1))
  (add-hook 'org-mode-hook 'my-org-mode))

(use-package project
  :straight nil
  :defer t
  :init
  (my-leader-def
    "pb" 'project-switch-to-buffer
    "pc" 'project-compile
    "pf" 'project-find-file
    "pp" 'project-switch-project
  ))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package sh-script
  :straight nil
  :defer t
  :config
  (defun my-shell-mode ()
    "My shell mode settings"
    (setq sh-indentation 2))
  (add-hook 'shell-mode-hook 'my-shell-mode))

(use-package treemacs
  :init
  (setq treemacs-no-png-images t)
  (with-eval-after-load 'winum
    (my-leader-def "0" 'treemacs-select-window))
  :general
  (my-leader-def "tt" 'treemacs))

(use-package treemacs-evil
  :after (evil treemacs))

(use-package undo-fu
  :after evil)

(use-package vertico
  :hook (after-init . vertico-mode)
  :straight (:files (:defaults "extensions/*")
                   :includes (vertico-buffer
                              vertico-directory
                              vertico-flat
                              vertico-indexed
                              vertico-mouse
                              vertico-quick
                              vertico-repeat
                              vertico-reverse))
  :config
  (general-define-key
   :keymaps 'vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous
   :keymaps 'minibuffer-local-map
   "C-h" 'backward-kill-word
   "C-w" 'backward-kill-word
   "C-u" 'kill-whole-line)
  (require 'vertico-repeat)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (my-leader-def
    "SPC" 'vertico-repeat-last
    "C-SPC" 'vertico-repeat-select))

(use-package visual-fill-column
  :hook
  (visual-line-mode . visual-fill-column-mode))

(use-package vterm
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
  (general-define-key
   :keymaps 'vterm-mode-map
   "C-u" 'vterm-send-C-u
   "C-6" 'evil-switch-to-windows-last-buffer))

(use-package which-key
  :config (which-key-mode))

(use-package wgrep
  :after project
  :init
  (setq wgrep-auto-save-buffer t))

(use-package wgrep-ag
  :after project)

(use-package winum
  :config
  (winum-mode 1)
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
  (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package yasnippet
  :init
  (with-eval-after-load "eglot" (require 'yasnippet)) ; Enable in case snippet completions are provided
  :general
  (my-leader-def "ty" 'yas-minor-mode))

(use-package zone-nyan
  :general
  (my-leader-def "tn" 'zone-nyan-preview))

;; -----
;; Final
;; -----

(find-file "~/.emacs.d/init.el")

;; Return gc-cons-threshold from early-init value
(setq gc-cons-threshold (* 2 1000 1000))
