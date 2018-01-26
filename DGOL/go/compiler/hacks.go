package main

// #cgo LDFLAGS: -lLLVMCore
// #include <llvm-c/Core.h>
import "C"
import "unsafe"

import "llvm.org/llvm/bindings/go/llvm"

func InsertDebugAddr(b llvm.Builder, mod llvm.Module, addr llvm.Value, diVar llvm.Metadata, diExpr llvm.Metadata) {
	contextRef := C.LLVMContextRef(unsafe.Pointer(mod.Context().C))

	var valueRef *C.LLVMValueRef

	var mdAddrVal llvm.Value
	valueRef = (*C.LLVMValueRef)(unsafe.Pointer(&mdAddrVal.C))
	*valueRef = C.LLVMMetadataAsValue(contextRef, C.LLVMValueAsMetadata(C.LLVMValueRef(unsafe.Pointer(addr.C))))

	var diVarVal llvm.Value
	valueRef = (*C.LLVMValueRef)(unsafe.Pointer(&diVarVal.C))
	*valueRef = C.LLVMMetadataAsValue(contextRef, C.LLVMMetadataRef(unsafe.Pointer(diVar.C)))

	var diExprVal llvm.Value
	valueRef = (*C.LLVMValueRef)(unsafe.Pointer(&diExprVal.C))
	*valueRef = C.LLVMMetadataAsValue(contextRef, C.LLVMMetadataRef(unsafe.Pointer(diExpr.C)))

	dbgAddr := mod.NamedFunction("llvm.dbg.addr")
	if dbgAddr.IsNil() {
		dbgAddr = llvm.AddFunction(mod, "llvm.dbg.addr", llvm.FunctionType(llvm.VoidType(), []llvm.Type{mdAddrVal.Type(), diVarVal.Type(), diExprVal.Type()}, false))
	}
	// b.CreateCall(dbgAddr, []llvm.Value{mdAddrVal, diVarVal, diExprVal}, "")
}
