(defun my-visual-fill-column-mode ()
  (interactive)
  (setq
   visual-fill-column-extra-text-width
   (cons (if display-line-numbers (+ 3 display-line-numbers-width) 0) 0))
  (visual-fill-column-mode))
