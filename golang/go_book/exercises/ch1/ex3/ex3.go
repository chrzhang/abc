// Modification of the echo program to compare performance.
package main

import (
	"fmt"
	"os"
	"strings"
	"time"
)

func main() {
	iterations := 10000000
	start := time.Now()
	for i := 0; i < iterations; i++ {
		go echoWithLoop()
	}
	secs := time.Since(start).Seconds()
	fmt.Printf("echoWithLoop took %f seconds.\n", secs)

	start = time.Now()
	for i := 0; i < iterations; i++ {
		go echoWithJoin()
	}
	go echoWithJoin()
	secs = time.Since(start).Seconds()
	fmt.Printf("echoWithJoin took %f seconds.\n", secs)
}

func echoWithLoop() {
	s, sep := "", ""
	for _, arg := range os.Args {
		s += sep + arg
		s = " "
	}
}

func echoWithJoin() {
	strings.Join(os.Args, " ")
}
