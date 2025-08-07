package main

import (
	"bufio"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"io"
	"log"
	"net"
	"net/http"
	"net/url"
	"os"
	"strings"
	"time"

	"github.com/aws/aws-sdk-go-v2/aws"
	v4 "github.com/aws/aws-sdk-go-v2/aws/signer/v4"
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
	r, err := s.signRequest(conn)
	if err != nil {
		sendResponse(errorResp{ErrMsg: err.Error()}, conn)
	} else {
		resp := signedHeaders{
			Headers: make(map[string]string),
		}
		for key, value := range r.Header {
			if len(value) > 0 {
				resp.Headers[key] = value[0]
			}
		}

		if err = sendResponse(resp, conn); err != nil {
			log.Println(err)
		}
	}

	if err = conn.Close(); err != nil {
		log.Println(err)
	}
}

type request struct {
	Profile string            `json:"profile"`
	Region  string            `json:"region"`
	Service *string           `json:"service"`
	Method  string            `json:"method"`
	URL     string            `json:"url"`
	Headers map[string]string `json:"headers"`
	Body    string            `json:"body"`
}

func (s connServer) signRequest(conn net.Conn) (*http.Request, error) {
	var req request
	if err := json.NewDecoder(conn).Decode(&req); err != nil {
		return nil, err
	}

	u, err := url.Parse(req.URL)
	if err != nil {
		return nil, err
	}

	httpRequest := &http.Request{
		Method: req.Method,
		URL:    u,
		Header: make(http.Header),
		Body:   io.NopCloser(strings.NewReader(req.Body)),
	}

	for key, value := range req.Headers {
		httpRequest.Header.Add(key, value)
	}

	creds, err := s.getCreds(req.Profile, conn)
	if err != nil {
		return nil, err
	}

	service := "execute-api"
	if req.Service != nil {
		service = *req.Service
	}

	h := sha256.New()
	h.Write([]byte(req.Body))
	payloadHash := hex.EncodeToString(h.Sum(nil))

	ctx := context.Background()
	now := time.Now()
	signer := v4.NewSigner()
	err = signer.SignHTTP(ctx, creds, httpRequest, payloadHash, service, req.Region, now)
	if err != nil {
		return nil, err
	}

	return httpRequest, nil
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

type signedHeaders struct {
	Headers map[string]string `json:"headers"`
}

func (r signedHeaders) name() string {
	return "signed"
}

type mfaPrompt struct {
	Profile      string  `json:"profile"`
	RoleARN      string  `json:"role-arn,omitempty"`
	SerialNumber *string `json:"serial-number,omitempty"`
}

func (r mfaPrompt) name() string {
	return "mfa-prompt"
}

type errorResp struct {
	ErrMsg string `json:"err-msg"`
}

func (r errorResp) name() string {
	return "error"
}

func checkErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}
