; INTERLAC runtime library

declare i8* @malloc(i32)
declare void @free(i8*)
declare i32 @read(i32,i8*,i32)
declare i32 @write(i32,i8*,i32)
declare void @exit(i32) noreturn
declare void @srandom(i32)
declare i32 @random()
declare i32 @time(i8*)
declare void @llvm.memcpy.p0i8.p0i8.i32(i8*, i8*, i32, i1)
declare i32 @llvm.ctpop.i32(i32)

; tag, value
; tag=0 16-bit value
; tag=1 32-bit value
; tag=2 error code
; tag=3 error code with special handling for the error message
; tag=3 used for error 000 and 579
%val = type {i2,i32}

@stack = global [79 x i8*] zeroinitializer
@stackptr = global i8 0

@read_digit_error = private constant [43 x i8] c"ICL579I WHAT BASE AND/OR LANGUAGE INCLUDES "

; read digit from fd 0
; tag
; tag=0 digit followed by space
; tag=1 digit followed by newline/eof/error
; tag=2 newline
; tag=3 eof/error
; tag=4 invalid digit (and beginning of error message already printed out)
define {i4,i32} @read_digit() {
    %buf = alloca i8 
    br label %read__

  read__:
    %count__ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1__ = icmp eq i32 %count__, 1
    br i1 %countis1__, label %check__, label %ret_eof

  check__:
    %check__char = load i8, i8* %buf
    %check__charisspace = icmp eq i8 %check__char, 32
    br i1 %check__charisspace, label %read__, label %check__newline

  check__newline:
    %check__charisnewline = icmp eq i8 %check__char, 10
    br i1 %check__charisnewline, label %ret_newline, label %check__O

  check__O:
    %check__charisO = icmp eq i8 %check__char, 79
    br i1 %check__charisO, label %read_O_, label %check__Z

  check__Z:
    %check__charisZ = icmp eq i8 %check__char, 90
    br i1 %check__charisZ, label %read_Z_, label %check__T

  check__T:
    %check__charisT = icmp eq i8 %check__char, 84
    br i1 %check__charisT, label %read_T_, label %check__F

  check__F:
    %check__charisF = icmp eq i8 %check__char, 70
    br i1 %check__charisF, label %read_F_, label %check__S

  check__S:
    %check__charisS = icmp eq i8 %check__char, 83
    br i1 %check__charisS, label %read_S_, label %check__E

  check__E:
    %check__charisE = icmp eq i8 %check__char, 69
    br i1 %check__charisE, label %read_E_, label %check__N

  check__N:
    %check__charisN = icmp eq i8 %check__char, 78
    br i1 %check__charisN, label %read_N_, label %error__char

  error__char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    ; store i8 %check__char, i8* %buf ; redundant
    br label %error_invalid_char


  read_O_:
    %count_O_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_O_ = icmp eq i32 %count_O_, 1
    br i1 %countis1_O_, label %check_O_, label %error_O_

  check_O_:
    %check_O_char = load i8, i8* %buf
    %check_O_charisspace = icmp eq i8 %check_O_char, 32
    br i1 %check_O_charisspace, label %error_O_, label %check_O_newline

  check_O_newline:
    %check_O_charisnewline = icmp eq i8 %check_O_char, 10
    br i1 %check_O_charisnewline, label %error_O_, label %check_O_H

  check_O_H:
    %check_O_charisH = icmp eq i8 %check_O_char, 72
    br i1 %check_O_charisH, label %read_OH_, label %check_O_N

  check_O_N:
    %check_O_charisN = icmp eq i8 %check_O_char, 78
    br i1 %check_O_charisN, label %read_ON_, label %error_O_char

  error_O_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_O_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_O_char, i8* %buf
    br label %error_invalid_char


  read_OH_:
    %count_OH_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_OH_ = icmp eq i32 %count_OH_, 1
    br i1 %countis1_OH_, label %check_OH_, label %ret_digit_newline

  check_OH_:
    %check_OH_char = load i8, i8* %buf
    %check_OH_charisspace = icmp eq i8 %check_OH_char, 32
    br i1 %check_OH_charisspace, label %ret_digit_space, label %check_OH_newline

  check_OH_newline:
    %check_OH_charisnewline = icmp eq i8 %check_OH_char, 10
    br i1 %check_OH_charisnewline, label %ret_digit_newline, label %error_OH_char

  error_OH_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_OH_char, i8* %buf
    br label %error_invalid_char


  read_ON_:
    %count_ON_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ON_ = icmp eq i32 %count_ON_, 1
    br i1 %countis1_ON_, label %check_ON_, label %error_ON_

  check_ON_:
    %check_ON_char = load i8, i8* %buf
    %check_ON_charisspace = icmp eq i8 %check_ON_char, 32
    br i1 %check_ON_charisspace, label %error_ON_, label %check_ON_newline

  check_ON_newline:
    %check_ON_charisnewline = icmp eq i8 %check_ON_char, 10
    br i1 %check_ON_charisnewline, label %error_ON_, label %check_ON_E

  check_ON_E:
    %check_ON_charisE = icmp eq i8 %check_ON_char, 69
    br i1 %check_ON_charisE, label %read_ONE_, label %error_ON_char

  error_ON_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_ON_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_ON_char, i8* %buf
    br label %error_invalid_char


  read_ONE_:
    %count_ONE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ONE_ = icmp eq i32 %count_ONE_, 1
    br i1 %countis1_ONE_, label %check_ONE_, label %ret_digit_newline

  check_ONE_:
    %check_ONE_char = load i8, i8* %buf
    %check_ONE_charisspace = icmp eq i8 %check_ONE_char, 32
    br i1 %check_ONE_charisspace, label %ret_digit_space, label %check_ONE_newline

  check_ONE_newline:
    %check_ONE_charisnewline = icmp eq i8 %check_ONE_char, 10
    br i1 %check_ONE_charisnewline, label %ret_digit_newline, label %error_ONE_char

  error_ONE_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_ONE_char, i8* %buf
    br label %error_invalid_char


  read_Z_:
    %count_Z_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_Z_ = icmp eq i32 %count_Z_, 1
    br i1 %countis1_Z_, label %check_Z_, label %error_Z_

  check_Z_:
    %check_Z_char = load i8, i8* %buf
    %check_Z_charisspace = icmp eq i8 %check_Z_char, 32
    br i1 %check_Z_charisspace, label %error_Z_, label %check_Z_newline

  check_Z_newline:
    %check_Z_charisnewline = icmp eq i8 %check_Z_char, 10
    br i1 %check_Z_charisnewline, label %error_Z_, label %check_Z_E

  check_Z_E:
    %check_Z_charisE = icmp eq i8 %check_Z_char, 69
    br i1 %check_Z_charisE, label %read_ZE_, label %error_Z_char

  error_Z_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_Z_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_Z_char, i8* %buf
    br label %error_invalid_char


  read_ZE_:
    %count_ZE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ZE_ = icmp eq i32 %count_ZE_, 1
    br i1 %countis1_ZE_, label %check_ZE_, label %error_ZE_

  check_ZE_:
    %check_ZE_char = load i8, i8* %buf
    %check_ZE_charisspace = icmp eq i8 %check_ZE_char, 32
    br i1 %check_ZE_charisspace, label %error_ZE_, label %check_ZE_newline

  check_ZE_newline:
    %check_ZE_charisnewline = icmp eq i8 %check_ZE_char, 10
    br i1 %check_ZE_charisnewline, label %error_ZE_, label %check_ZE_R

  check_ZE_R:
    %check_ZE_charisR = icmp eq i8 %check_ZE_char, 82
    br i1 %check_ZE_charisR, label %read_ZER_, label %error_ZE_char

  error_ZE_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_ZE_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_ZE_char, i8* %buf
    br label %error_invalid_char


  read_ZER_:
    %count_ZER_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ZER_ = icmp eq i32 %count_ZER_, 1
    br i1 %countis1_ZER_, label %check_ZER_, label %error_ZER_

  check_ZER_:
    %check_ZER_char = load i8, i8* %buf
    %check_ZER_charisspace = icmp eq i8 %check_ZER_char, 32
    br i1 %check_ZER_charisspace, label %error_ZER_, label %check_ZER_newline

  check_ZER_newline:
    %check_ZER_charisnewline = icmp eq i8 %check_ZER_char, 10
    br i1 %check_ZER_charisnewline, label %error_ZER_, label %check_ZER_O

  check_ZER_O:
    %check_ZER_charisO = icmp eq i8 %check_ZER_char, 79
    br i1 %check_ZER_charisO, label %read_ZERO_, label %error_ZER_char

  error_ZER_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_ZER_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_ZER_char, i8* %buf
    br label %error_invalid_char


  read_ZERO_:
    %count_ZERO_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ZERO_ = icmp eq i32 %count_ZERO_, 1
    br i1 %countis1_ZERO_, label %check_ZERO_, label %ret_digit_newline

  check_ZERO_:
    %check_ZERO_char = load i8, i8* %buf
    %check_ZERO_charisspace = icmp eq i8 %check_ZERO_char, 32
    br i1 %check_ZERO_charisspace, label %ret_digit_space, label %check_ZERO_newline

  check_ZERO_newline:
    %check_ZERO_charisnewline = icmp eq i8 %check_ZERO_char, 10
    br i1 %check_ZERO_charisnewline, label %ret_digit_newline, label %error_ZERO_char

  error_ZERO_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_ZERO_char, i8* %buf
    br label %error_invalid_char


  read_T_:
    %count_T_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_T_ = icmp eq i32 %count_T_, 1
    br i1 %countis1_T_, label %check_T_, label %error_T_

  check_T_:
    %check_T_char = load i8, i8* %buf
    %check_T_charisspace = icmp eq i8 %check_T_char, 32
    br i1 %check_T_charisspace, label %error_T_, label %check_T_newline

  check_T_newline:
    %check_T_charisnewline = icmp eq i8 %check_T_char, 10
    br i1 %check_T_charisnewline, label %error_T_, label %check_T_W

  check_T_W:
    %check_T_charisW = icmp eq i8 %check_T_char, 87
    br i1 %check_T_charisW, label %read_TW_, label %check_T_H

  check_T_H:
    %check_T_charisH = icmp eq i8 %check_T_char, 72
    br i1 %check_T_charisH, label %read_TH_, label %error_T_char

  error_T_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_T_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_T_char, i8* %buf
    br label %error_invalid_char


  read_TW_:
    %count_TW_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_TW_ = icmp eq i32 %count_TW_, 1
    br i1 %countis1_TW_, label %check_TW_, label %error_TW_

  check_TW_:
    %check_TW_char = load i8, i8* %buf
    %check_TW_charisspace = icmp eq i8 %check_TW_char, 32
    br i1 %check_TW_charisspace, label %error_TW_, label %check_TW_newline

  check_TW_newline:
    %check_TW_charisnewline = icmp eq i8 %check_TW_char, 10
    br i1 %check_TW_charisnewline, label %error_TW_, label %check_TW_O

  check_TW_O:
    %check_TW_charisO = icmp eq i8 %check_TW_char, 79
    br i1 %check_TW_charisO, label %read_TWO_, label %error_TW_char

  error_TW_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 87, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_TW_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 87, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_TW_char, i8* %buf
    br label %error_invalid_char


  read_TWO_:
    %count_TWO_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_TWO_ = icmp eq i32 %count_TWO_, 1
    br i1 %countis1_TWO_, label %check_TWO_, label %ret_digit_newline

  check_TWO_:
    %check_TWO_char = load i8, i8* %buf
    %check_TWO_charisspace = icmp eq i8 %check_TWO_char, 32
    br i1 %check_TWO_charisspace, label %ret_digit_space, label %check_TWO_newline

  check_TWO_newline:
    %check_TWO_charisnewline = icmp eq i8 %check_TWO_char, 10
    br i1 %check_TWO_charisnewline, label %ret_digit_newline, label %error_TWO_char

  error_TWO_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 87, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_TWO_char, i8* %buf
    br label %error_invalid_char


  read_TH_:
    %count_TH_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_TH_ = icmp eq i32 %count_TH_, 1
    br i1 %countis1_TH_, label %check_TH_, label %error_TH_

  check_TH_:
    %check_TH_char = load i8, i8* %buf
    %check_TH_charisspace = icmp eq i8 %check_TH_char, 32
    br i1 %check_TH_charisspace, label %error_TH_, label %check_TH_newline

  check_TH_newline:
    %check_TH_charisnewline = icmp eq i8 %check_TH_char, 10
    br i1 %check_TH_charisnewline, label %error_TH_, label %check_TH_R

  check_TH_R:
    %check_TH_charisR = icmp eq i8 %check_TH_char, 82
    br i1 %check_TH_charisR, label %read_THR_, label %error_TH_char

  error_TH_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_TH_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_TH_char, i8* %buf
    br label %error_invalid_char


  read_THR_:
    %count_THR_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_THR_ = icmp eq i32 %count_THR_, 1
    br i1 %countis1_THR_, label %check_THR_, label %error_THR_

  check_THR_:
    %check_THR_char = load i8, i8* %buf
    %check_THR_charisspace = icmp eq i8 %check_THR_char, 32
    br i1 %check_THR_charisspace, label %error_THR_, label %check_THR_newline

  check_THR_newline:
    %check_THR_charisnewline = icmp eq i8 %check_THR_char, 10
    br i1 %check_THR_charisnewline, label %error_THR_, label %check_THR_E

  check_THR_E:
    %check_THR_charisE = icmp eq i8 %check_THR_char, 69
    br i1 %check_THR_charisE, label %read_THRE_, label %error_THR_char

  error_THR_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_THR_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_THR_char, i8* %buf
    br label %error_invalid_char


  read_THRE_:
    %count_THRE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_THRE_ = icmp eq i32 %count_THRE_, 1
    br i1 %countis1_THRE_, label %check_THRE_, label %error_THRE_

  check_THRE_:
    %check_THRE_char = load i8, i8* %buf
    %check_THRE_charisspace = icmp eq i8 %check_THRE_char, 32
    br i1 %check_THRE_charisspace, label %error_THRE_, label %check_THRE_newline

  check_THRE_newline:
    %check_THRE_charisnewline = icmp eq i8 %check_THRE_char, 10
    br i1 %check_THRE_charisnewline, label %error_THRE_, label %check_THRE_E

  check_THRE_E:
    %check_THRE_charisE = icmp eq i8 %check_THRE_char, 69
    br i1 %check_THRE_charisE, label %read_THREE_, label %error_THRE_char

  error_THRE_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_THRE_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_THRE_char, i8* %buf
    br label %error_invalid_char


  read_THREE_:
    %count_THREE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_THREE_ = icmp eq i32 %count_THREE_, 1
    br i1 %countis1_THREE_, label %check_THREE_, label %ret_digit_newline

  check_THREE_:
    %check_THREE_char = load i8, i8* %buf
    %check_THREE_charisspace = icmp eq i8 %check_THREE_char, 32
    br i1 %check_THREE_charisspace, label %ret_digit_space, label %check_THREE_newline

  check_THREE_newline:
    %check_THREE_charisnewline = icmp eq i8 %check_THREE_char, 10
    br i1 %check_THREE_charisnewline, label %ret_digit_newline, label %error_THREE_char

  error_THREE_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    ; store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_THREE_char, i8* %buf
    br label %error_invalid_char


  read_F_:
    %count_F_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_F_ = icmp eq i32 %count_F_, 1
    br i1 %countis1_F_, label %check_F_, label %error_F_

  check_F_:
    %check_F_char = load i8, i8* %buf
    %check_F_charisspace = icmp eq i8 %check_F_char, 32
    br i1 %check_F_charisspace, label %error_F_, label %check_F_newline

  check_F_newline:
    %check_F_charisnewline = icmp eq i8 %check_F_char, 10
    br i1 %check_F_charisnewline, label %error_F_, label %check_F_O

  check_F_O:
    %check_F_charisO = icmp eq i8 %check_F_char, 79
    br i1 %check_F_charisO, label %read_FO_, label %check_F_I

  check_F_I:
    %check_F_charisI = icmp eq i8 %check_F_char, 73
    br i1 %check_F_charisI, label %read_FI_, label %error_F_char

  error_F_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_F_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_F_char, i8* %buf
    br label %error_invalid_char


  read_FO_:
    %count_FO_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FO_ = icmp eq i32 %count_FO_, 1
    br i1 %countis1_FO_, label %check_FO_, label %error_FO_

  check_FO_:
    %check_FO_char = load i8, i8* %buf
    %check_FO_charisspace = icmp eq i8 %check_FO_char, 32
    br i1 %check_FO_charisspace, label %error_FO_, label %check_FO_newline

  check_FO_newline:
    %check_FO_charisnewline = icmp eq i8 %check_FO_char, 10
    br i1 %check_FO_charisnewline, label %error_FO_, label %check_FO_U

  check_FO_U:
    %check_FO_charisU = icmp eq i8 %check_FO_char, 85
    br i1 %check_FO_charisU, label %read_FOU_, label %error_FO_char

  error_FO_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_FO_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_FO_char, i8* %buf
    br label %error_invalid_char


  read_FOU_:
    %count_FOU_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FOU_ = icmp eq i32 %count_FOU_, 1
    br i1 %countis1_FOU_, label %check_FOU_, label %error_FOU_

  check_FOU_:
    %check_FOU_char = load i8, i8* %buf
    %check_FOU_charisspace = icmp eq i8 %check_FOU_char, 32
    br i1 %check_FOU_charisspace, label %error_FOU_, label %check_FOU_newline

  check_FOU_newline:
    %check_FOU_charisnewline = icmp eq i8 %check_FOU_char, 10
    br i1 %check_FOU_charisnewline, label %error_FOU_, label %check_FOU_R

  check_FOU_R:
    %check_FOU_charisR = icmp eq i8 %check_FOU_char, 82
    br i1 %check_FOU_charisR, label %read_FOUR_, label %error_FOU_char

  error_FOU_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 85, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_FOU_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 85, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_FOU_char, i8* %buf
    br label %error_invalid_char


  read_FOUR_:
    %count_FOUR_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FOUR_ = icmp eq i32 %count_FOUR_, 1
    br i1 %countis1_FOUR_, label %check_FOUR_, label %ret_digit_newline

  check_FOUR_:
    %check_FOUR_char = load i8, i8* %buf
    %check_FOUR_charisspace = icmp eq i8 %check_FOUR_char, 32
    br i1 %check_FOUR_charisspace, label %ret_digit_space, label %check_FOUR_newline

  check_FOUR_newline:
    %check_FOUR_charisnewline = icmp eq i8 %check_FOUR_char, 10
    br i1 %check_FOUR_charisnewline, label %ret_digit_newline, label %error_FOUR_char

  error_FOUR_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 85, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_FOUR_char, i8* %buf
    br label %error_invalid_char


  read_FI_:
    %count_FI_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FI_ = icmp eq i32 %count_FI_, 1
    br i1 %countis1_FI_, label %check_FI_, label %error_FI_

  check_FI_:
    %check_FI_char = load i8, i8* %buf
    %check_FI_charisspace = icmp eq i8 %check_FI_char, 32
    br i1 %check_FI_charisspace, label %error_FI_, label %check_FI_newline

  check_FI_newline:
    %check_FI_charisnewline = icmp eq i8 %check_FI_char, 10
    br i1 %check_FI_charisnewline, label %error_FI_, label %check_FI_V

  check_FI_V:
    %check_FI_charisV = icmp eq i8 %check_FI_char, 86
    br i1 %check_FI_charisV, label %read_FIV_, label %error_FI_char

  error_FI_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_FI_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_FI_char, i8* %buf
    br label %error_invalid_char


  read_FIV_:
    %count_FIV_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FIV_ = icmp eq i32 %count_FIV_, 1
    br i1 %countis1_FIV_, label %check_FIV_, label %error_FIV_

  check_FIV_:
    %check_FIV_char = load i8, i8* %buf
    %check_FIV_charisspace = icmp eq i8 %check_FIV_char, 32
    br i1 %check_FIV_charisspace, label %error_FIV_, label %check_FIV_newline

  check_FIV_newline:
    %check_FIV_charisnewline = icmp eq i8 %check_FIV_char, 10
    br i1 %check_FIV_charisnewline, label %error_FIV_, label %check_FIV_E

  check_FIV_E:
    %check_FIV_charisE = icmp eq i8 %check_FIV_char, 69
    br i1 %check_FIV_charisE, label %read_FIVE_, label %error_FIV_char

  error_FIV_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_FIV_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_FIV_char, i8* %buf
    br label %error_invalid_char


  read_FIVE_:
    %count_FIVE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FIVE_ = icmp eq i32 %count_FIVE_, 1
    br i1 %countis1_FIVE_, label %check_FIVE_, label %ret_digit_newline

  check_FIVE_:
    %check_FIVE_char = load i8, i8* %buf
    %check_FIVE_charisspace = icmp eq i8 %check_FIVE_char, 32
    br i1 %check_FIVE_charisspace, label %ret_digit_space, label %check_FIVE_newline

  check_FIVE_newline:
    %check_FIVE_charisnewline = icmp eq i8 %check_FIVE_char, 10
    br i1 %check_FIVE_charisnewline, label %ret_digit_newline, label %error_FIVE_char

  error_FIVE_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_FIVE_char, i8* %buf
    br label %error_invalid_char


  read_S_:
    %count_S_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_S_ = icmp eq i32 %count_S_, 1
    br i1 %countis1_S_, label %check_S_, label %error_S_

  check_S_:
    %check_S_char = load i8, i8* %buf
    %check_S_charisspace = icmp eq i8 %check_S_char, 32
    br i1 %check_S_charisspace, label %error_S_, label %check_S_newline

  check_S_newline:
    %check_S_charisnewline = icmp eq i8 %check_S_char, 10
    br i1 %check_S_charisnewline, label %error_S_, label %check_S_I

  check_S_I:
    %check_S_charisI = icmp eq i8 %check_S_char, 73
    br i1 %check_S_charisI, label %read_SI_, label %check_S_E

  check_S_E:
    %check_S_charisE = icmp eq i8 %check_S_char, 69
    br i1 %check_S_charisE, label %read_SE_, label %error_S_char

  error_S_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_S_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_S_char, i8* %buf
    br label %error_invalid_char


  read_SI_:
    %count_SI_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SI_ = icmp eq i32 %count_SI_, 1
    br i1 %countis1_SI_, label %check_SI_, label %error_SI_

  check_SI_:
    %check_SI_char = load i8, i8* %buf
    %check_SI_charisspace = icmp eq i8 %check_SI_char, 32
    br i1 %check_SI_charisspace, label %error_SI_, label %check_SI_newline

  check_SI_newline:
    %check_SI_charisnewline = icmp eq i8 %check_SI_char, 10
    br i1 %check_SI_charisnewline, label %error_SI_, label %check_SI_X

  check_SI_X:
    %check_SI_charisX = icmp eq i8 %check_SI_char, 88
    br i1 %check_SI_charisX, label %read_SIX_, label %error_SI_char

  error_SI_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_SI_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_SI_char, i8* %buf
    br label %error_invalid_char


  read_SIX_:
    %count_SIX_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SIX_ = icmp eq i32 %count_SIX_, 1
    br i1 %countis1_SIX_, label %check_SIX_, label %ret_digit_newline

  check_SIX_:
    %check_SIX_char = load i8, i8* %buf
    %check_SIX_charisspace = icmp eq i8 %check_SIX_char, 32
    br i1 %check_SIX_charisspace, label %ret_digit_space, label %check_SIX_newline

  check_SIX_newline:
    %check_SIX_charisnewline = icmp eq i8 %check_SIX_char, 10
    br i1 %check_SIX_charisnewline, label %ret_digit_newline, label %error_SIX_char

  error_SIX_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 88, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_SIX_char, i8* %buf
    br label %error_invalid_char


  read_SE_:
    %count_SE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SE_ = icmp eq i32 %count_SE_, 1
    br i1 %countis1_SE_, label %check_SE_, label %error_SE_

  check_SE_:
    %check_SE_char = load i8, i8* %buf
    %check_SE_charisspace = icmp eq i8 %check_SE_char, 32
    br i1 %check_SE_charisspace, label %error_SE_, label %check_SE_newline

  check_SE_newline:
    %check_SE_charisnewline = icmp eq i8 %check_SE_char, 10
    br i1 %check_SE_charisnewline, label %error_SE_, label %check_SE_V

  check_SE_V:
    %check_SE_charisV = icmp eq i8 %check_SE_char, 86
    br i1 %check_SE_charisV, label %read_SEV_, label %error_SE_char

  error_SE_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_SE_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_SE_char, i8* %buf
    br label %error_invalid_char


  read_SEV_:
    %count_SEV_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SEV_ = icmp eq i32 %count_SEV_, 1
    br i1 %countis1_SEV_, label %check_SEV_, label %error_SEV_

  check_SEV_:
    %check_SEV_char = load i8, i8* %buf
    %check_SEV_charisspace = icmp eq i8 %check_SEV_char, 32
    br i1 %check_SEV_charisspace, label %error_SEV_, label %check_SEV_newline

  check_SEV_newline:
    %check_SEV_charisnewline = icmp eq i8 %check_SEV_char, 10
    br i1 %check_SEV_charisnewline, label %error_SEV_, label %check_SEV_E

  check_SEV_E:
    %check_SEV_charisE = icmp eq i8 %check_SEV_char, 69
    br i1 %check_SEV_charisE, label %read_SEVE_, label %error_SEV_char

  error_SEV_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_SEV_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_SEV_char, i8* %buf
    br label %error_invalid_char


  read_SEVE_:
    %count_SEVE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SEVE_ = icmp eq i32 %count_SEVE_, 1
    br i1 %countis1_SEVE_, label %check_SEVE_, label %error_SEVE_

  check_SEVE_:
    %check_SEVE_char = load i8, i8* %buf
    %check_SEVE_charisspace = icmp eq i8 %check_SEVE_char, 32
    br i1 %check_SEVE_charisspace, label %error_SEVE_, label %check_SEVE_newline

  check_SEVE_newline:
    %check_SEVE_charisnewline = icmp eq i8 %check_SEVE_char, 10
    br i1 %check_SEVE_charisnewline, label %error_SEVE_, label %check_SEVE_N

  check_SEVE_N:
    %check_SEVE_charisN = icmp eq i8 %check_SEVE_char, 78
    br i1 %check_SEVE_charisN, label %read_SEVEN_, label %error_SEVE_char

  error_SEVE_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_SEVE_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_SEVE_char, i8* %buf
    br label %error_invalid_char


  read_SEVEN_:
    %count_SEVEN_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SEVEN_ = icmp eq i32 %count_SEVEN_, 1
    br i1 %countis1_SEVEN_, label %check_SEVEN_, label %ret_digit_newline

  check_SEVEN_:
    %check_SEVEN_char = load i8, i8* %buf
    %check_SEVEN_charisspace = icmp eq i8 %check_SEVEN_char, 32
    br i1 %check_SEVEN_charisspace, label %ret_digit_space, label %check_SEVEN_newline

  check_SEVEN_newline:
    %check_SEVEN_charisnewline = icmp eq i8 %check_SEVEN_char, 10
    br i1 %check_SEVEN_charisnewline, label %ret_digit_newline, label %error_SEVEN_char

  error_SEVEN_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_SEVEN_char, i8* %buf
    br label %error_invalid_char


  read_E_:
    %count_E_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_E_ = icmp eq i32 %count_E_, 1
    br i1 %countis1_E_, label %check_E_, label %error_E_

  check_E_:
    %check_E_char = load i8, i8* %buf
    %check_E_charisspace = icmp eq i8 %check_E_char, 32
    br i1 %check_E_charisspace, label %error_E_, label %check_E_newline

  check_E_newline:
    %check_E_charisnewline = icmp eq i8 %check_E_char, 10
    br i1 %check_E_charisnewline, label %error_E_, label %check_E_I

  check_E_I:
    %check_E_charisI = icmp eq i8 %check_E_char, 73
    br i1 %check_E_charisI, label %read_EI_, label %error_E_char

  error_E_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_E_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_E_char, i8* %buf
    br label %error_invalid_char


  read_EI_:
    %count_EI_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_EI_ = icmp eq i32 %count_EI_, 1
    br i1 %countis1_EI_, label %check_EI_, label %error_EI_

  check_EI_:
    %check_EI_char = load i8, i8* %buf
    %check_EI_charisspace = icmp eq i8 %check_EI_char, 32
    br i1 %check_EI_charisspace, label %error_EI_, label %check_EI_newline

  check_EI_newline:
    %check_EI_charisnewline = icmp eq i8 %check_EI_char, 10
    br i1 %check_EI_charisnewline, label %error_EI_, label %check_EI_G

  check_EI_G:
    %check_EI_charisG = icmp eq i8 %check_EI_char, 71
    br i1 %check_EI_charisG, label %read_EIG_, label %error_EI_char

  error_EI_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_EI_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_EI_char, i8* %buf
    br label %error_invalid_char


  read_EIG_:
    %count_EIG_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_EIG_ = icmp eq i32 %count_EIG_, 1
    br i1 %countis1_EIG_, label %check_EIG_, label %error_EIG_

  check_EIG_:
    %check_EIG_char = load i8, i8* %buf
    %check_EIG_charisspace = icmp eq i8 %check_EIG_char, 32
    br i1 %check_EIG_charisspace, label %error_EIG_, label %check_EIG_newline

  check_EIG_newline:
    %check_EIG_charisnewline = icmp eq i8 %check_EIG_char, 10
    br i1 %check_EIG_charisnewline, label %error_EIG_, label %check_EIG_H

  check_EIG_H:
    %check_EIG_charisH = icmp eq i8 %check_EIG_char, 72
    br i1 %check_EIG_charisH, label %read_EIGH_, label %error_EIG_char

  error_EIG_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_EIG_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_EIG_char, i8* %buf
    br label %error_invalid_char


  read_EIGH_:
    %count_EIGH_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_EIGH_ = icmp eq i32 %count_EIGH_, 1
    br i1 %countis1_EIGH_, label %check_EIGH_, label %error_EIGH_

  check_EIGH_:
    %check_EIGH_char = load i8, i8* %buf
    %check_EIGH_charisspace = icmp eq i8 %check_EIGH_char, 32
    br i1 %check_EIGH_charisspace, label %error_EIGH_, label %check_EIGH_newline

  check_EIGH_newline:
    %check_EIGH_charisnewline = icmp eq i8 %check_EIGH_char, 10
    br i1 %check_EIGH_charisnewline, label %error_EIGH_, label %check_EIGH_T

  check_EIGH_T:
    %check_EIGH_charisT = icmp eq i8 %check_EIGH_char, 84
    br i1 %check_EIGH_charisT, label %read_EIGHT_, label %error_EIGH_char

  error_EIGH_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_EIGH_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_EIGH_char, i8* %buf
    br label %error_invalid_char


  read_EIGHT_:
    %count_EIGHT_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_EIGHT_ = icmp eq i32 %count_EIGHT_, 1
    br i1 %countis1_EIGHT_, label %check_EIGHT_, label %ret_digit_newline

  check_EIGHT_:
    %check_EIGHT_char = load i8, i8* %buf
    %check_EIGHT_charisspace = icmp eq i8 %check_EIGHT_char, 32
    br i1 %check_EIGHT_charisspace, label %ret_digit_space, label %check_EIGHT_newline

  check_EIGHT_newline:
    %check_EIGHT_charisnewline = icmp eq i8 %check_EIGHT_char, 10
    br i1 %check_EIGHT_charisnewline, label %ret_digit_newline, label %error_EIGHT_char

  error_EIGHT_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 84, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_EIGHT_char, i8* %buf
    br label %error_invalid_char


  read_N_:
    %count_N_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_N_ = icmp eq i32 %count_N_, 1
    br i1 %countis1_N_, label %check_N_, label %error_N_

  check_N_:
    %check_N_char = load i8, i8* %buf
    %check_N_charisspace = icmp eq i8 %check_N_char, 32
    br i1 %check_N_charisspace, label %error_N_, label %check_N_newline

  check_N_newline:
    %check_N_charisnewline = icmp eq i8 %check_N_char, 10
    br i1 %check_N_charisnewline, label %error_N_, label %check_N_I

  check_N_I:
    %check_N_charisI = icmp eq i8 %check_N_char, 73
    br i1 %check_N_charisI, label %read_NI_, label %error_N_char

  error_N_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_N_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_N_char, i8* %buf
    br label %error_invalid_char


  read_NI_:
    %count_NI_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_NI_ = icmp eq i32 %count_NI_, 1
    br i1 %countis1_NI_, label %check_NI_, label %error_NI_

  check_NI_:
    %check_NI_char = load i8, i8* %buf
    %check_NI_charisspace = icmp eq i8 %check_NI_char, 32
    br i1 %check_NI_charisspace, label %error_NI_, label %check_NI_newline

  check_NI_newline:
    %check_NI_charisnewline = icmp eq i8 %check_NI_char, 10
    br i1 %check_NI_charisnewline, label %error_NI_, label %check_NI_N

  check_NI_N:
    %check_NI_charisN = icmp eq i8 %check_NI_char, 78
    br i1 %check_NI_charisN, label %read_NIN_, label %error_NI_char

  error_NI_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_NI_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_NI_char, i8* %buf
    br label %error_invalid_char


  read_NIN_:
    %count_NIN_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_NIN_ = icmp eq i32 %count_NIN_, 1
    br i1 %countis1_NIN_, label %check_NIN_, label %error_NIN_

  check_NIN_:
    %check_NIN_char = load i8, i8* %buf
    %check_NIN_charisspace = icmp eq i8 %check_NIN_char, 32
    br i1 %check_NIN_charisspace, label %error_NIN_, label %check_NIN_newline

  check_NIN_newline:
    %check_NIN_charisnewline = icmp eq i8 %check_NIN_char, 10
    br i1 %check_NIN_charisnewline, label %error_NIN_, label %check_NIN_E

  check_NIN_E:
    %check_NIN_charisE = icmp eq i8 %check_NIN_char, 69
    br i1 %check_NIN_charisE, label %read_NINE_, label %error_NIN_char

  error_NIN_:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %ret_invalid

  error_NIN_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_NIN_char, i8* %buf
    br label %error_invalid_char


  read_NINE_:
    %count_NINE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_NINE_ = icmp eq i32 %count_NINE_, 1
    br i1 %countis1_NINE_, label %check_NINE_, label %ret_digit_newline

  check_NINE_:
    %check_NINE_char = load i8, i8* %buf
    %check_NINE_charisspace = icmp eq i8 %check_NINE_char, 32
    br i1 %check_NINE_charisspace, label %ret_digit_space, label %check_NINE_newline

  check_NINE_newline:
    %check_NINE_charisnewline = icmp eq i8 %check_NINE_char, 10
    br i1 %check_NINE_charisnewline, label %ret_digit_newline, label %error_NINE_char

  error_NINE_char:
    call i32 @write(i32 1, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    store i8 %check_NINE_char, i8* %buf
    br label %error_invalid_char


  ret_digit_space:
    %ret_digit_space_value = phi i32 [0, %check_ZERO_], [0, %check_OH_], [1, %check_ONE_], [2, %check_TWO_], [3, %check_THREE_], [4, %check_FOUR_], [5, %check_FIVE_], [6, %check_SIX_], [7, %check_SEVEN_], [8, %check_EIGHT_], [9, %check_NINE_]
    %ret_digit_space_struct = insertvalue {i4,i32} zeroinitializer, i32 %ret_digit_space_value, 1
    ret {i4,i32} %ret_digit_space_struct

  ret_digit_newline:
    %ret_digit_newline_value = phi i32 [0, %read_OH_], [0, %check_OH_newline], [0, %read_ZERO_], [0, %check_ZERO_newline], [1, %read_ONE_], [1, %check_ONE_newline], [2, %read_TWO_], [2, %check_TWO_newline], [3, %read_THREE_], [3, %check_THREE_newline], [4, %read_FOUR_], [4, %check_FOUR_newline], [5, %read_FIVE_], [5, %check_FIVE_newline], [6, %read_SIX_], [6, %check_SIX_newline], [7, %read_SEVEN_], [7, %check_SEVEN_newline], [8, %read_EIGHT_], [8, %check_EIGHT_newline], [9, %read_NINE_], [9, %check_NINE_newline]
    %ret_digit_newline_struct = insertvalue {i4,i32} insertvalue({i4,i32} zeroinitializer, i4 1, 0), i32 %ret_digit_newline_value, 1
    ret {i4,i32} %ret_digit_newline_struct

  ret_newline:
    ret {i4,i32} insertvalue({i4,i32} zeroinitializer, i4 2, 0)

  ret_eof:
    ret {i4,i32} insertvalue({i4,i32} zeroinitializer, i4 3, 0)

  error_invalid_char:
    call i32 @write(i32 1, i8* %buf, i32 1)
    br label %error_invalid

  error_invalid:
    %error_invalid_count = call i32 @read(i32 0, i8* %buf, i32 1)
    %error_invalid_countis1 = icmp eq i32 %error_invalid_count, 1
    br i1 %error_invalid_countis1, label %error_invalid_check_space, label %ret_invalid

  error_invalid_check_space:
    %error_invalid_check_char = load i8, i8* %buf
    %error_invalid_check_charisspace = icmp eq i8 %error_invalid_check_char, 32
    br i1 %error_invalid_check_charisspace, label %ret_invalid, label %error_invalid_check_newline

  error_invalid_check_newline:
    %error_invalid_check_charisnewline = icmp eq i8 %error_invalid_check_char, 10
    br i1 %error_invalid_check_charisnewline, label %ret_invalid, label %error_invalid_char

  ret_invalid:
    store i8 63, i8* %buf
    call i32 @write(i32 1, i8* %buf, i32 1)
    ret {i4,i32} insertvalue({i4,i32} zeroinitializer, i4 4, 0)
}

;...
