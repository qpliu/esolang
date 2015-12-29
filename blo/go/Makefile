blo:
	go build blo.go interp.go compile.go annotate.go ast.go eval.go llvm_codegen.go llvm_runtime.go parse.go runtime.go token.go value.go

test:
	go fmt && go vet && go test

validate: test blo
	go clean
