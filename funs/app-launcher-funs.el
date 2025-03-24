;; Sourced from https://gitlab.com/dwt1/configuring-emacs/-/blob/main/07-the-final-touches/scripts/app-launchers.el
(defun my-emacs-run-launcher ()
  "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                    (minibuffer . only)
                    (fullscreen . 0) ; no fullscreen
                    (undecorated . t) ; remove title bar
                    (internal-border-width . 10)
                    (width . 80)
                    (height . 11)))
    (unwind-protect
        (app-launcher-run-app)
      (delete-frame))))
