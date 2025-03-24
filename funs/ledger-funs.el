(defun my--find-expense-matches ()
  "finds want or need Expenses: in the buffer to complete"
  (delete-dups (save-match-data
                 (let ((pos 0) (string (buffer-string)) matches)
                   (while (string-match "Expenses:[WN][^ ]*" string pos)
                     (push (match-string-no-properties 0 string) matches)
                     (setq pos (match-end 0)))
                   matches))))

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
    (insert (completing-read "Enter the expense: " (my--find-expense-matches)))
    (insert "  $")
    (insert cost)
    (insert "\n    Assets:Checking\n")))

(defun my-ledger-mode-settings ()
  "My ledger mode settings"
  (setq tab-width 4
        indent-line-function 'insert-tab
        electric-indent-local-mode -1))
