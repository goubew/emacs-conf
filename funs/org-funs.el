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
    (insert
     (read-string "Enter link description =>"
                  (replace-regexp-in-string "^\* *" "" org-header)))
    (insert "]]")))
