;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Load .el files over .elc if they are newer
(setq load-prefer-newer t)

;; Add non-packages files to the load path
(add-to-list 'load-path (concat user-emacs-directory "packages/"))

;; Add MELPA archive
(with-eval-after-load 'package (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
