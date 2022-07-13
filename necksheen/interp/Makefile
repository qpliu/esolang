neck:
	go build neck.go interp.go parse.go queue.go tokenize.go

clean:
	go clean

test:
	go test

cover:
	go test -coverprofile=/tmp/cover.out
	go tool cover -html=/tmp/cover.out
