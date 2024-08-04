// Modification to echo to print the index of each command-line argument.
package main

import (
	"fmt"
	"os"
)

func main() {
	for idx, arg := range os.Args {
		fmt.Printf("[%d]: %s\n", idx, arg)
	}
}
