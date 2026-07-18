(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp (bytecomp)))
      native-comp-async-report-warnings-errors 'silent
      load-prefer-newer t
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      default-frame-alist '((fullscreen . maximized)))

(setq enable-third-party-packages t)

(when enable-third-party-packages
  ;; Add non-packages files to the load path
  (add-to-list 'load-path (concat user-emacs-directory "packages/"))
  (add-to-list 'load-path (concat user-emacs-directory "lilypond/"))

  ;; Add MELPA archive
  (with-eval-after-load 'package
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (setq package-archive-priorities
          '(("gnu" . 10)
            ("nongnu" . 5)
            ("melpa" . 1)))))

;; Adds repeat support to a whole keymap at once
;; From Karthinks "It Bears Repeating"
(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))
