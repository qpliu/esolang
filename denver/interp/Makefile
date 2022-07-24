denver: denver.go interp.go parse.go system.go thread.go tokenize.go
	go build $^

clean:
	go clean

test:
	go fmt
	go test

cover:
	go test -coverprofile=/tmp/cover.out
	go tool cover -html=/tmp/cover.out
