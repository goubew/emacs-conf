;; Load .el files over .elc if they are newer
(setq load-prefer-newer t)

;; Add non-packages files to the load path
(add-to-list 'load-path (concat user-emacs-directory "packages/"))

;; Add MELPA archive
(with-eval-after-load 'package (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Disable native comp warnings
(setq native-comp-async-report-warnings-errors 'silent); Do not pop up comp warnings

;; Set load path
(add-to-list 'load-path (expand-file-name "extra-packages" user-emacs-directory))
