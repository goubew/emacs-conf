(defun my-count-paragraphs (start end)
  "Return number of paragraphs between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (- (buffer-size) (forward-paragraph (buffer-size))))))

(defun my-count-paragraphs-region-or-buffer ()
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

(defun my-eval-and-run-all-tests-in-buffer ()
  "Delete all loaded tests from the runtime, evaluate the current buffer and run all loaded tests with ert."
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert 't))

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
