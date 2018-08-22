all: COMPILAC INTERLAC

COMPILAC: runtime
	go build COMPILAC.go

INTERLAC: runtime
	go build INTERLAC.go

intercal/runtime.go: intercal/runtime.ll
	(echo 'package intercal;const RUNTIME=`'; cat $^; echo '`') > $@

runtime: intercal/runtime.go

clean:
	go clean
	$(RM) coverage.out coverage2.out intercal/runtime.go src _

test:
	go fmt && go vet && go test -cover ./intercal

cover:
	$(RM) _ src
	ln -s . _
	ln -s . src
	go test ./intercal -coverprofile=coverage.out
	sed -e 's/^\(_[/]\).*[/]intercal/\1intercal/g' < coverage.out > coverage2.out
	GOPATH=$(PWD) go tool cover -html=coverage2.out
	$(RM) _ src coverage.out coverage2.out

.PHONY: clean test cover runtime
