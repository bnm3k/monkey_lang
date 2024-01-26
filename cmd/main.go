package main

import (
	"fmt"
	"os"

	"github.com/bnm3k/monkey_lang/repl"
)

func main() {
	fmt.Printf("This is the Monkey programming language!\n")
	repl.Start(os.Stdin, os.Stdout)
}
