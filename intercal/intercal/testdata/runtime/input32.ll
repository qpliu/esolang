declare void @printf(i8*,...)
@test_output_fmt = private constant [9 x i8] c"{%d,%u}\0A\00"

define void @main() {
    br label %loop

  loop:
    %r = call %val @input32()
    %tag2 = extractvalue %val %r, 0
    %tag = zext i2 %tag2 to i32
    %val = extractvalue %val %r, 1
    call void (i8*,...) @printf(i8* getelementptr([9 x i8], [9 x i8]* @test_output_fmt, i32 0, i32 0), i32 %tag, i32 %val)
    %got_value = icmp eq i2 %tag2, 1
    br i1 %got_value, label %loop, label %done

  done:
    ret void
}
