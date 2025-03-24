(defun my-should-ansible ()
  "Check if ansible should be enabled"
  (when (string-match "/playbooks/.*\.ya?ml\\'" (buffer-file-name))
    (ansible-mode)))
