define void @main() {
    %buf = alloca i8
    br label %loop

  loop:
    %read = call {i1,i1} @input_binary()
    %eof = extractvalue {i1,i1} %read,1
    br i1 %eof,label %done,label %continue

  continue:
    %bit = extractvalue {i1,i1} %read,0
    %char = select i1 %bit,i8 49,i8 48
    store i8 %char,i8* %buf
    call i32 @write(i32 1,i8* %buf,i32 1)
    br label %loop

  done:
    store i8 10,i8* %buf
    call i32 @write(i32 1,i8* %buf,i32 1)
    ret void
}