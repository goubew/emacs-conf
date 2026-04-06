go-mode

(iferr "if err != nil {" n> "return err" n> "}" >)
(iferrf "if err != nil {" n> "return fmt.Errorf(\"" r> ": %w\", err)" n "}" >)
