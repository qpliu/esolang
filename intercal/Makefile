COMPILAC: COMPILAC.go errors.go expr.go input.go interp.go ops.go output.go statement.go tokenizer.go
	go build $^

clean:
	go clean
	$(RM) coverage.out coverage2.out src _

test:
	go fmt && go vet && go test -cover

cover:
	ln -s . _
	ln -s . src
	go test -coverprofile=coverage.out
	sed -e 's/^\(_[/]\).*[/]/\1/g' < coverage.out > coverage2.out
	GOPATH=$(PWD) go tool cover -html=coverage2.out
	$(RM) _ src coverage.out coverage2.out

.PHONY: clean test cover
