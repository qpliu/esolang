SRC=blo.go interp.go compile.go annotate.go ast.go eval.go llvm_codegen.go llvm_runtime.go parse.go runtime.go token.go value.go

blo: $(SRC)
	go build $(SRC)

test:
	go fmt && go vet && go test

clean:
	go clean

validate: test blo
	go clean
