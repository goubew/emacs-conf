emacs-lisp-mode

(defkey "(define-key map (kbd \"" p "\") '" q ")")

go-mode

(errf "fmt.Errorf(\"" r> ": %w\", err)")
(iferr "if err != nil {" n> "return err" n> "}" >)
(iferrf "if err != nil {" n> "return fmt.Errorf(\"" r> ": %w\", err)" n "}" >)

lilypond-mode

(tup "\\tuplet 3/2 { " r " }")

js-mode

(byid "document.getElementById(\"" r "\");")
