package main

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"net"
	"os"
	"strings"

	"github.com/aws/aws-sdk-go-v2/aws"
	"github.com/aws/aws-sdk-go-v2/config"
	"github.com/aws/aws-sdk-go-v2/credentials/stscreds"
)

func main() {
	log.SetOutput(os.Stdout)
	log.SetFlags(log.LstdFlags | log.Lmsgprefix)

	listener, err := net.Listen("unix", os.Args[1])
	checkErr(err)

	server := connServer{make(map[string]aws.Credentials)}

	for {
		conn, err := listener.Accept()
		if err != nil {
			if errors.Is(err, net.ErrClosed) {
				return
			}
		}
		checkErr(err)

		go server.serveConn(conn)
	}
}

type connServer struct {
	profileCreds map[string]aws.Credentials
}

func (s connServer) serveConn(conn net.Conn) {
	if err := s.sendCreds(conn); err != nil {
		fmt.Fprint(conn, err)
		log.Println(err)
	}

	if err := conn.Close(); err != nil {
		log.Println(err)
	}
}

func (s connServer) sendCreds(conn net.Conn) error {
	var req request
	if err := json.NewDecoder(conn).Decode(&req); err != nil {
		return err
	}

	creds, err := s.getCreds(req.Profile, conn)
	if err != nil {
		return err
	}

	return sendResponse(credsResponse{
		KeyId:        creds.AccessKeyID,
		SecretKey:    creds.SecretAccessKey,
		SessionToken: creds.SessionToken,
	}, conn)
}

func (s connServer) getCreds(profile string, conn net.Conn) (aws.Credentials, error) {
	creds, ok := s.profileCreds[profile]
	if ok && !creds.Expired() {
		return creds, nil
	}

	ctx := context.Background()

	cfg, err := config.LoadDefaultConfig(ctx,
		config.WithSharedConfigProfile(profile),
		config.WithAssumeRoleCredentialOptions(func(o *stscreds.AssumeRoleOptions) {
			o.TokenProvider = createMFAProvider(o, profile, conn)
		}),
	)
	if err != nil {
		return aws.Credentials{}, err
	}

	creds, err = cfg.Credentials.Retrieve(ctx)
	if err != nil {
		return aws.Credentials{}, err
	}

	s.profileCreds[profile] = creds

	return creds, nil
}

type request struct {
	Profile string `json:"profile"`
}

type credsResponse struct {
	KeyId        string `json:"key-id"`
	SecretKey    string `json:"secret-key"`
	SessionToken string `json:"session-token,omitempty"`
}

func (r credsResponse) name() string {
	return "creds"
}

func createMFAProvider(o *stscreds.AssumeRoleOptions, profile string, conn net.Conn) func() (string, error) {
	return func() (string, error) {
		err := sendResponse(mfaPrompt{
			Profile:      profile,
			RoleARN:      o.RoleARN,
			SerialNumber: o.SerialNumber,
		}, conn)
		if err != nil {
			return "", err
		}

		reader := bufio.NewReader(conn)
		line, err := reader.ReadString('\n')
		if err != nil {
			return "", err
		}

		return strings.TrimSpace(line), nil
	}
}

type mfaPrompt struct {
	Profile      string  `json:"profile"`
	RoleARN      string  `json:"role-arn,omitempty"`
	SerialNumber *string `json:"serial-number,omitempty"`
}

func (r mfaPrompt) name() string {
	return "mfa-prompt"
}

type response interface {
	name() string
}

func sendResponse(resp response, conn net.Conn) error {
	data, err := json.Marshal(resp)
	if err != nil {
		return err
	}

	fields := make(map[string]any)
	if err := json.Unmarshal(data, &fields); err != nil {
		return err
	}

	fields["type"] = resp.name()
	return json.NewEncoder(conn).Encode(fields)
}

func checkErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
