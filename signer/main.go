package main

import (
	"encoding/json"
	"errors"
	"log"
	"net"
	"os"
	"strings"
)

func main() {
	log.SetOutput(os.Stdout)

	listener, err := net.Listen("unix", os.Args[1])
	checkErr(err)

	for {
		conn, err := listener.Accept()
		if err != nil {
			if errors.Is(err, net.ErrClosed) {
				return
			}
		}
		checkErr(err)

		go serveConn(conn)
	}
}

func serveConn(conn net.Conn) {
	env := make(map[string]string)
	for _, e := range os.Environ() {
		name, value, _ := strings.Cut(e, "=")
		env[name] = value
	}

	payload, _ := json.MarshalIndent(env, "", "  ")
	conn.Write(payload)
	// time.Sleep(time.Second)
	// conn.Write(payload[len(payload)/2:])
	conn.Close()
}

func checkErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
