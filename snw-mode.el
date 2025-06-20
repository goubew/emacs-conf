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

;;;###autoload
(define-derived-mode snw-mode text-mode "snw"
  "Major mode for snw budget files."
  (setq font-lock-defaults '(snw-font-lock-keywords))
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.snw\\'" . snw-mode))

;; add the mode to the `features' list
(provide 'snw-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; snw-mode.el ends here
