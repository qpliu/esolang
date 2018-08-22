; INTERLAC runtime library

declare i8* @malloc(i32)
declare void @free(i8*)
declare i32 @read(i32,*i8,i32)
declare i32 @write(i32,*i8,i32)
declare void @exit(i32) noreturn
declare void @srandom(i32)
declare i32 @random()
declare i32 @time(*i8)
declare void @llvm.memcpy.p0i8.p0i8.i32(i8*, i8*, i32, i1)
declare i32 @llvm.ctpop.i32(i32)

; tag, value
; tag=0 16-bit value
; tag=1 32-bit value
; tag=2 error code
; tag=3 error code with special handling for the error message
; tag=3 used for error 000 and 579
%val = type { i2, i32 }

@stack = global [79 x *i8] zeroinitializer
@stackptr = global i8 0

;...
