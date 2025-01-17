(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp (bytecomp)))
      native-comp-async-report-warnings-errors 'silent
      load-prefer-newer t
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      default-frame-alist '((fullscreen . maximized)))

;; Add non-packages files to the load path
(add-to-list 'load-path (concat user-emacs-directory "packages/"))

;; Add MELPA archive
(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (setq package-archive-priorities
        '(("gnu" . 10)
          ("nongnu" . 5)
          ("melpa" . 1))))
