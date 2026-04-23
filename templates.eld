go-mode

(errf "fmt.Errorf(\"" r> ": %w\", err)")
(iferr "if err != nil {" n> "return err" n> "}" >)
(iferrf "if err != nil {" n> "return fmt.Errorf(\"" r> ": %w\", err)" n "}" >)
