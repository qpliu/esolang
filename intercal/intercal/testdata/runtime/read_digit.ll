declare void @printf(i8*,...)
@test_output_fmt = private constant [9 x i8] c"{%d,%d}\0A\00"

define void @main() {
       %r = call {i4,i32} @read_digit()
       %tag4 = extractvalue {i4,i32} %r, 0
       %tag = zext i4 %tag4 to i32
       %val = extractvalue {i4,i32} %r, 1
       call void (i8*,...) @printf(i8* getelementptr([9 x i8], [9 x i8]* @test_output_fmt, i32 0, i32 0), i32 %tag, i32 %val)
       ret void
}
