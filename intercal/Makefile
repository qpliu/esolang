COMPILAC: COMPILAC.go errors.go expr.go input.go interp.go ops.go output.go statement.go tokenizer.go
	go build $^

clean:
	go clean

test:
	go fmt && go vet && go test -cover

.PHONY: clean test
