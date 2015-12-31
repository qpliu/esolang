SRC=annotate.go ast.go blo.go compile.go eval.go interp.go llvm_codegen.go llvm_runtime.go parse.go runtime.go token.go value.go

blo: $(SRC)
	go build -o $@ $(SRC)

test:
	go fmt && go vet && go test

clean:
	go clean

validate: test blo
	go clean
