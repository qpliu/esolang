all: interp compile

test:
	go fmt && go vet && go test

validate: test interp compile
	go clean

interp:
	go build interp.go compile.go annotate.go ast.go blo.go eval.go llvm_codegen.go llvm_runtime.go parse.go runtime.go token.go value.go

compile:
	go build compile.go interp.go annotate.go ast.go blo.go eval.go llvm_codegen.go llvm_runtime.go parse.go runtime.go token.go value.go
