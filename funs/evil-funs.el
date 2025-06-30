(defun my-emulate-ctrl-c ()
  (interactive)
  (setq unread-command-events
        (nconc (listify-key-sequence (kbd "C-c")) unread-command-events)))
