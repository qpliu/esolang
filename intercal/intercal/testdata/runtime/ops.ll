declare void @printf(i8*,...)
@test_output_fmt = private constant [12 x i8] c"%u:{%u,%u}\0A\00"

define void @pval(i32 %index, %val %arg) {
    %tag2 = extractvalue %val %arg, 0
    %tag = zext i2 %tag2 to i32
    %v = extractvalue %val %arg, 1
    call void (i8*,...) @printf(i8* getelementptr([12 x i8], [12 x i8]* @test_output_fmt, i32 0, i32 0), i32 %index, i32 %tag, i32 %v)
    ret void
}

define void @test_mingle(i32 %index, i2 %ltag, i32 %lval, i2 %rtag, i32 %rval) {
    %l1 = insertvalue %val zeroinitializer, i2 %ltag, 0
    %l = insertvalue %val %l1, i32 %lval, 1
    %r1 = insertvalue %val zeroinitializer, i2 %rtag, 0
    %r = insertvalue %val %r1, i32 %rval, 1
    %res = call %val @op_mingle(%val %l, %val %r)
    call void @pval(i32 %index, %val %res)
    ret void
}

define void @test_select(i32 %index, i2 %ltag, i32 %lval, i2 %rtag, i32 %rval) {
    %l1 = insertvalue %val zeroinitializer, i2 %ltag, 0
    %l = insertvalue %val %l1, i32 %lval, 1
    %r1 = insertvalue %val zeroinitializer, i2 %rtag, 0
    %r = insertvalue %val %r1, i32 %rval, 1
    %res = call %val @op_select(%val %l, %val %r)
    call void @pval(i32 %index, %val %res)
    ret void
}

define void @test_and(i32 %index, i2 %ltag, i32 %lval) {
    %l1 = insertvalue %val zeroinitializer, i2 %ltag, 0
    %l = insertvalue %val %l1, i32 %lval, 1
    %res = call %val @op_and(%val %l)
    call void @pval(i32 %index, %val %res)
    ret void
}

define void @test_or(i32 %index, i2 %ltag, i32 %lval) {
    %l1 = insertvalue %val zeroinitializer, i2 %ltag, 0
    %l = insertvalue %val %l1, i32 %lval, 1
    %res = call %val @op_or(%val %l)
    call void @pval(i32 %index, %val %res)
    ret void
}

define void @test_xor(i32 %index, i2 %ltag, i32 %lval) {
    %l1 = insertvalue %val zeroinitializer, i2 %ltag, 0
    %l = insertvalue %val %l1, i32 %lval, 1
    %res = call %val @op_xor(%val %l)
    call void @pval(i32 %index, %val %res)
    ret void
}

define void @main() {
    call void @test_mingle(i32  0, i2 0, i32 17, i2 0, i32 123)
    call void @test_mingle(i32  1, i2 1, i32 17, i2 0, i32 123)
    call void @test_mingle(i32  2, i2 2, i32 17, i2 0, i32 123)
    call void @test_mingle(i32  3, i2 3, i32 17, i2 0, i32 123)
    call void @test_mingle(i32  4, i2 0, i32 17, i2 1, i32 123)
    call void @test_mingle(i32  5, i2 1, i32 17, i2 1, i32 123)
    call void @test_mingle(i32  6, i2 2, i32 17, i2 1, i32 123)
    call void @test_mingle(i32  7, i2 3, i32 17, i2 1, i32 123)
    call void @test_mingle(i32  8, i2 0, i32 17, i2 2, i32 123)
    call void @test_mingle(i32  9, i2 1, i32 17, i2 2, i32 123)
    call void @test_mingle(i32 10, i2 2, i32 17, i2 2, i32 123)
    call void @test_mingle(i32 11, i2 3, i32 17, i2 2, i32 123)
    call void @test_mingle(i32 12, i2 0, i32 17, i2 3, i32 123)
    call void @test_mingle(i32 13, i2 1, i32 17, i2 3, i32 123)
    call void @test_mingle(i32 14, i2 2, i32 17, i2 3, i32 123)
    call void @test_mingle(i32 15, i2 3, i32 17, i2 3, i32 123)

    call void @test_select(i32 20, i2 0, i32 17, i2 0, i32 123)
    call void @test_select(i32 21, i2 1, i32 17, i2 0, i32 123)
    call void @test_select(i32 22, i2 2, i32 17, i2 0, i32 123)
    call void @test_select(i32 23, i2 3, i32 17, i2 0, i32 123)
    call void @test_select(i32 24, i2 0, i32 17, i2 1, i32 123)
    call void @test_select(i32 25, i2 1, i32 17, i2 1, i32 123)
    call void @test_select(i32 26, i2 2, i32 17, i2 1, i32 123)
    call void @test_select(i32 27, i2 3, i32 17, i2 1, i32 123)
    call void @test_select(i32 28, i2 0, i32 17, i2 2, i32 123)
    call void @test_select(i32 29, i2 1, i32 17, i2 2, i32 123)
    call void @test_select(i32 30, i2 2, i32 17, i2 2, i32 123)
    call void @test_select(i32 31, i2 3, i32 17, i2 2, i32 123)
    call void @test_select(i32 32, i2 0, i32 17, i2 3, i32 123)
    call void @test_select(i32 33, i2 1, i32 17, i2 3, i32 123)
    call void @test_select(i32 34, i2 2, i32 17, i2 3, i32 123)
    call void @test_select(i32 35, i2 3, i32 17, i2 3, i32 123)

    call void @test_and(i32 40, i2 0, i32 17)
    call void @test_and(i32 41, i2 1, i32 17)
    call void @test_and(i32 42, i2 2, i32 17)
    call void @test_and(i32 43, i2 3, i32 17)

    call void @test_or(i32 50, i2 0, i32 17)
    call void @test_or(i32 51, i2 1, i32 17)
    call void @test_or(i32 52, i2 2, i32 17)
    call void @test_or(i32 53, i2 3, i32 17)

    call void @test_xor(i32 60, i2 0, i32 17)
    call void @test_xor(i32 61, i2 1, i32 17)
    call void @test_xor(i32 62, i2 2, i32 17)
    call void @test_xor(i32 63, i2 3, i32 17)

    call void @test_mingle(i32 2863311530, i2 0, i32 65535, i2 0, i32 0)
    call void @test_mingle(i32 1431655765, i2 0, i32 0, i2 0, i32 65535)
    call void @test_mingle(i32 65535, i2 0, i32 255, i2 0, i32 255)

    call void @test_select(i32 9, i2 0, i32 179, i2 0, i32 201)
    call void @test_select(i32 17, i2 0, i32 201, i2 0, i32 179)
    call void @test_select(i32 31, i2 0, i32 179, i2 0, i32 179)
    call void @test_select(i32 15, i2 0, i32 201, i2 0, i32 201)

    call void @test_and(i32 4, i2 0, i32 77)
    call void @test_and(i32 4, i2 1, i32 77)

    call void @test_or(i32 32879, i2 0, i32 77)
    call void @test_or(i32 2147483759, i2 1, i32 77)

    call void @test_xor(i32 32875, i2 0, i32 77)
    call void @test_xor(i32 2147483755, i2 1, i32 77)

    ret void
}
