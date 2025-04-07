(use-package c-ts-mode
  :defer t
  :config
  (load-file (concat user-emacs-directory "funs/c-funs.el"))
  (c-set-offset 'case-label '+)
  :bind (:map c-ts-mode-map
              ("C-c m o" . my-swap-h-c-file)))

(use-package completion-preview
  :hook ((prog-mode text-mode) . completion-preview-mode))

(use-package css-mode
  :defer t
  :init
  (setq css-indent-offset 2))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

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

(use-package elec-pair
  :defer t
  :hook (prog-mode . electric-pair-local-mode))

(use-package emacs
  :init
  (setq
   auto-save-file-name-transforms '((".*" "~/.saves/" t))
   backup-directory-alist '(("." . "~/.saves"))
   enable-recursive-minibuffers t
   frame-title-format "Emacs"
   help-window-select t
   inhibit-startup-screen t
   ispell-program-name "aspell"
   mac-command-modifier 'control
   recentf-max-menu-items 25
   recentf-max-saved-items 25
   ring-bell-function 'ignore
   scroll-conservatively 33
   scroll-margin 2
   show-trailing-whitespace t
   split-width-threshold 200
   tab-always-indent 'complete
   use-short-answers t
   vc-follow-symlinks t
   visible-cursor nil
   window-divider-default-places t
   window-divider-default-right-width 1
   window-divider-default-bottom-width 1
   visual-fill-column-width 90
   visual-fill-column-center-text t
   x-underline-at-descent-line t)

  (setq-default
   indent-tabs-mode nil
   tab-width 4)

  (menu-bar-mode -1)
  (recentf-mode 1)
  (save-place-mode 1)
  (window-divider-mode t)

  (add-to-list 'same-window-buffer-names "*compilation*")
  (load-file (concat user-emacs-directory "funs/emacs-funs.el"))
  (when enable-third-party-packages (load-file (concat user-emacs-directory "packages.el")))

  (when (or (display-graphic-p) (daemonp))
    (blink-cursor-mode 0)
    (tool-bar-mode -1)
    (scroll-bar-mode 0)

    (if (daemonp)
        (add-hook 'server-after-make-frame-hook #'my-set-font)
      (my-set-font)))

  :bind (("C-c =" . indent-region)
         :map minibuffer-local-map
         ("C-w" . backward-kill-word)))

(use-package flymake
  :bind (("C-c e n" . flymake-goto-next-error)
         ("C-c e p" . flymake-goto-prev-error)))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(use-package go-ts-mode
  :defer t
  :config
  (setq go-ts-mode-indent-offset 4))

(use-package java-ts-mode
  :defer t
  :config
  (setq java-ts-mode-indent-offset 2))

(use-package org
  :defer t
  :init
  (setq
   org-adapt-indentation nil
   org-agenda-files '("~/org/")
   org-edit-src-content-indentation 0
   org-goto-interface 'outline-path-completion
   org-indent-mode-turns-on-hiding-stars nil
   org-outline-path-complete-in-steps nil
   org-src-window-setup t)
  :config
  (load-file (concat user-emacs-directory "funs/org-funs.el")))

(use-package ox-ascii
  :defer t
  :init
  (setq
   org-ascii-global-margin 0
   org-ascii-inner-margin 0
   org-ascii-text-width 65))

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

(use-package repeat
  :config
  (repeat-mode))

(use-package sh-script
  :defer t
  :init
  (setq sh-basic-offset 2))

(use-package tab-bar
  :defer t
  :init
  (setq
   tab-bar-separator "|"
   tab-bar-new-button-show nil
   tab-bar-close-button-show nil))
