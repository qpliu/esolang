all: COMPILAC INTERLAC

COMPILAC:
	go build COMPILAC.go

INTERLAC: runtime
	go build INTERLAC.go runtime.go

runtime.go: intercal/runtime-common.ll intercal/runtime-6.ll intercal/runtime-7.ll
	(echo 'package main;const RUNTIME=`'; cat intercal/runtime-common.ll; echo '`;const RUNTIME6=`'; cat intercal/runtime-6.ll; echo '`;const RUNTIME7=`'; cat intercal/runtime-7.ll; echo '`') > $@

runtime: runtime.go

clean:
	go clean
	$(RM) coverage.out coverage2.out runtime.go src _
	$(RM) -r a.out.dSYM

test:
	go fmt ./... && go vet ./... && go test -cover ./intercal

quicktest:
	go fmt ./... && go vet ./... && go test -short -cover ./intercal

cover:
	$(RM) _ src
	ln -s . _
	ln -s . src
	go test -short ./intercal -coverprofile=coverage.out
	sed -e 's/^\(_[/]\).*[/]intercal/\1intercal/g' < coverage.out > coverage2.out
	GOPATH=$(PWD) go tool cover -html=coverage2.out
	$(RM) _ src coverage.out coverage2.out

.PHONY: clean test cover runtime
