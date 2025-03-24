(require 'project)
(defun my-swap-h-c-file ()
  "Finds the associated .h or .c file in the current project"
  (interactive)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (let ((related-file
           (cond
            ((string-match "\\.h\\'" file-name)
             (concat "^" (regexp-quote (replace-regexp-in-string "\\.h\\'" ".c" file-name)) "$"))
            ((string-match "\\.c\\'" file-name)
             (concat "^" (regexp-quote (replace-regexp-in-string "\\.c\\'" ".h" file-name)) "$")))))
      (if related-file
          (let ((found-file-list
                 (directory-files-recursively (project-root (project-current)) related-file nil)))
            (if (> (length found-file-list) 0)
                (find-file (nth 0 found-file-list))
              (message "Associated file not found in project")))
        (message "File does not end in .c or .h")))))
