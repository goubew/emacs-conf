;;; snw-mode.el --- snw output syntax highlighting

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a major mode with highlighting for .snw files

;;; Code:

(require 'transient)
(require 'org)

;; create the list for font-lock.
;; each category of keyword is given a particular face
(defvar snw-font-lock-keywords)

;; Regular expression sections to be combined into the full lines
(setq
 snw--date-re "[[:blank:]]*[[:digit:]]\\{4\\}/[[:digit:]]\\{2\\}/[[:digit:]]\\\{2\\}[[:blank:]]*"
 snw--amount-re "$\\(?:[[:digit:]]+\\.[[:digit:]]\\{2\\}\\|\\*\\)[[:blank:]]*"
 snw--txn-path-re "[[:alpha:]]+\\(?::[[:alpha:]]+\\)*[[:blank:]]*"
 snw--bucket-re "[[:upper:]][[:alpha:]]+[[:blank:]]*"
 snw--inc-split-re "\\([[:digit:]]\\{2\\}/[[:digit:]]\\{2\\}/[[:digit:]]\\{2\\}\\)?[[:blank:]]*"
 snw--comment-re "\\(?:([^)]*)\\)?[[:blank:]]*"
 snw--arrow-re "[[:blank:]]*->[[:blank:]*")

(setq snw--inc-txn-re
      (concat snw--date-re
              "\\+"
              snw--amount-re
              snw--txn-path-re
              snw--inc-split-re
              snw--comment-re)
      snw--exp-txn-re
      (concat snw--date-re
              "-"
              snw--amount-re
              snw--bucket-re
              snw--arrow-re
              snw--txn-path-re
              snw--comment-re)
      snw--qual-ice-re
      (concat snw--date-re
              "\\*"
              snw--amount-re
              snw--bucket-re
              snw--arrow-re
              snw--bucket-re
              snw--comment-re)
      snw--unqual-ice-re
      (concat snw--date-re
              "\\*"
              snw--amount-re
              snw--bucket-re
              snw--comment-re))

(setq snw-font-lock-keywords
      (list (cons snw--inc-txn-re 'font-lock-doc-face)
            (cons snw--exp-txn-re 'font-lock-keyword-face)
            (cons snw--qual-ice-re 'font-lock-type-face)
            (cons snw--unqual-ice-re 'font-lock-type-face)))

(defvar-keymap snw-mode-map
  :doc "Keymap for snw mode"
    "C-c C-e" #'snw-transient-expense)

;;;###autoload
(define-derived-mode snw-mode text-mode "snw"
  "Major mode for snw budget files."
  (setq font-lock-defaults '(snw-font-lock-keywords))
  (use-local-map snw-mode-map))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.snw\\'" . snw-mode))

(defun snw--find-expense-paths ()
  "Finds expense paths to complete"
  (delete-dups (save-match-data
                 (let ((pos 0) (string (buffer-string)) matches)
                   (while (string-match
                           "-> \\([:[:alpha:]]+\\)"
                           string
                           pos)
                     (push (match-string-no-properties 1 string) matches)
                     (setq pos (match-end 0)))
                   matches))))

(transient-define-infix snw--transient-date-infix ()
  :class 'transient-option
  :argument "--date="
  :shortarg "d"
  :description "date"
  :always-read t
  :init-value (lambda (obj) (oset obj value (format-time-string "%Y/%m/%d")))
  :reader (lambda (_ _ _)
            (let ((org-read-date-prefer-future nil))
              (replace-regexp-in-string "-" "/" (org-read-date)))))

(transient-define-infix snw--transient-path-infix ()
  :class 'transient-option
  :argument "--path="
  :shortarg "p"
  :description "path"
  :always-read t
  :reader (lambda (_ _ _)
            (completing-read "Enter path: " (snw--find-expense-paths))))

(transient-define-suffix snw--transient-expense-suffix (&optional args)
  "Adds a new expense to the end of the file"
  :key "n"
  :description "Add new expense"
  (interactive (list (transient-args transient-current-command)))
  (let* ((type (transient-arg-value "--type=" args))
         (date (transient-arg-value "--date=" args))
         (amount (transient-arg-value "--amount=" args))
         (fmtamount (format "%-9s" amount))
         (txnpath (transient-arg-value "--path=" args)))
    (goto-char (point-max))
    (insert date " -$" fmtamount type " -> " txnpath)))

(transient-define-prefix snw-transient-expense ()
  "Prompt for information to create a new snw expense"
  ["Transaction components"
   (snw--transient-date-infix)
   ("a" "amount" "--amount=" :always-read t)
   ("t" "type" "--type=" :choices ("Want" "Need") :always-read t)
   (snw--transient-path-infix)]
  ["Execute"
   (snw--transient-expense-suffix)])

;; add the mode to the `features' list
(provide 'snw-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; snw-mode.el ends here
