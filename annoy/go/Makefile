all: annoy

annoy: annoy.go ast.go interp2.go lib.go parser.go tokenizer.go value.go
	go build -o $@ $^

clean:
	go clean
	$(RM) coverage.out coverage2.out

test:
	go fmt
	go vet
	go test

cover:
	go test -coverprofile=coverage.out
	sed -e 's/^_.*[/]go/./' < coverage.out > coverage2.out
	go tool cover -html=coverage2.out
	$(RM) coverage.out coverage2.out

.PHONY: all clean test cover
