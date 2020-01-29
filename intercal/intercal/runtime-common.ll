; INTERLAC runtime library

declare i8* @malloc(i32)
declare void @free(i8*)
declare i32 @read(i32,i8*,i32)
declare i32 @write(i32,i8*,i32)
declare void @exit(i32) noreturn
declare void @srandom(i32)
declare i32 @random()
declare i32 @time(i8*)

; tag, value
; tag=0 16-bit value
; tag=1 32-bit value
; tag=2 error code
; tag=3 error code with special handling for the error message
; tag=3 used for error 000 and 579
%val = type {i2,i32}

; ignore flag, value linked-list
%vrbl = type {i1,%vrbl_val*}
; link,stash count,value
%vrbl_val = type {%vrbl_val*,i32,i32}

%arr_vrbl = type {i1,%arr_val*}
; link,stash count,dimensions,
; in the [0 x i32], 0..ndims-1 are the dimensions, ndims... are the values
%arr_val = type {%arr_val*,i32,i32,[0 x i32]}

@read_digit_error = private constant [43 x i8] c"ICL579I WHAT BASE AND/OR LANGUAGE INCLUDES "

define i1 @is_space(i8 %char) {
    %is32 = icmp eq i8 %char, 32
    %is9 = icmp eq i8 %char, 9
    %is13 = icmp eq i8 %char, 13
    %is32or9 = or i1 %is32, %is9
    %isspace = or i1 %is32or9, %is13
    ret i1 %isspace
}

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
    %check__charisspace = call i1 @is_space(i8 %check__char)
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
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    ; store i8 %check__char, i8* %buf ; redundant
    br label %error_invalid_char


  read_O_:
    %count_O_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_O_ = icmp eq i32 %count_O_, 1
    br i1 %countis1_O_, label %check_O_, label %error_O_

  check_O_:
    %check_O_char = load i8, i8* %buf
    %check_O_charisspace = call i1 @is_space(i8 %check_O_char)
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
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_O_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_O_char, i8* %buf
    br label %error_invalid_char


  read_OH_:
    %count_OH_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_OH_ = icmp eq i32 %count_OH_, 1
    br i1 %countis1_OH_, label %check_OH_, label %ret_digit_newline

  check_OH_:
    %check_OH_char = load i8, i8* %buf
    %check_OH_charisspace = call i1 @is_space(i8 %check_OH_char)
    br i1 %check_OH_charisspace, label %ret_digit_space, label %check_OH_newline

  check_OH_newline:
    %check_OH_charisnewline = icmp eq i8 %check_OH_char, 10
    br i1 %check_OH_charisnewline, label %ret_digit_newline, label %error_OH_char

  error_OH_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_OH_char, i8* %buf
    br label %error_invalid_char


  read_ON_:
    %count_ON_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ON_ = icmp eq i32 %count_ON_, 1
    br i1 %countis1_ON_, label %check_ON_, label %error_ON_

  check_ON_:
    %check_ON_char = load i8, i8* %buf
    %check_ON_charisspace = call i1 @is_space(i8 %check_ON_char)
    br i1 %check_ON_charisspace, label %error_ON_, label %check_ON_newline

  check_ON_newline:
    %check_ON_charisnewline = icmp eq i8 %check_ON_char, 10
    br i1 %check_ON_charisnewline, label %error_ON_, label %check_ON_E

  check_ON_E:
    %check_ON_charisE = icmp eq i8 %check_ON_char, 69
    br i1 %check_ON_charisE, label %read_ONE_, label %error_ON_char

  error_ON_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_ON_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_ON_char, i8* %buf
    br label %error_invalid_char


  read_ONE_:
    %count_ONE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ONE_ = icmp eq i32 %count_ONE_, 1
    br i1 %countis1_ONE_, label %check_ONE_, label %ret_digit_newline

  check_ONE_:
    %check_ONE_char = load i8, i8* %buf
    %check_ONE_charisspace = call i1 @is_space(i8 %check_ONE_char)
    br i1 %check_ONE_charisspace, label %ret_digit_space, label %check_ONE_newline

  check_ONE_newline:
    %check_ONE_charisnewline = icmp eq i8 %check_ONE_char, 10
    br i1 %check_ONE_charisnewline, label %ret_digit_newline, label %error_ONE_char

  error_ONE_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_ONE_char, i8* %buf
    br label %error_invalid_char


  read_Z_:
    %count_Z_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_Z_ = icmp eq i32 %count_Z_, 1
    br i1 %countis1_Z_, label %check_Z_, label %error_Z_

  check_Z_:
    %check_Z_char = load i8, i8* %buf
    %check_Z_charisspace = call i1 @is_space(i8 %check_Z_char)
    br i1 %check_Z_charisspace, label %error_Z_, label %check_Z_newline

  check_Z_newline:
    %check_Z_charisnewline = icmp eq i8 %check_Z_char, 10
    br i1 %check_Z_charisnewline, label %error_Z_, label %check_Z_E

  check_Z_E:
    %check_Z_charisE = icmp eq i8 %check_Z_char, 69
    br i1 %check_Z_charisE, label %read_ZE_, label %error_Z_char

  error_Z_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_Z_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_Z_char, i8* %buf
    br label %error_invalid_char


  read_ZE_:
    %count_ZE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ZE_ = icmp eq i32 %count_ZE_, 1
    br i1 %countis1_ZE_, label %check_ZE_, label %error_ZE_

  check_ZE_:
    %check_ZE_char = load i8, i8* %buf
    %check_ZE_charisspace = call i1 @is_space(i8 %check_ZE_char)
    br i1 %check_ZE_charisspace, label %error_ZE_, label %check_ZE_newline

  check_ZE_newline:
    %check_ZE_charisnewline = icmp eq i8 %check_ZE_char, 10
    br i1 %check_ZE_charisnewline, label %error_ZE_, label %check_ZE_R

  check_ZE_R:
    %check_ZE_charisR = icmp eq i8 %check_ZE_char, 82
    br i1 %check_ZE_charisR, label %read_ZER_, label %error_ZE_char

  error_ZE_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_ZE_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_ZE_char, i8* %buf
    br label %error_invalid_char


  read_ZER_:
    %count_ZER_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ZER_ = icmp eq i32 %count_ZER_, 1
    br i1 %countis1_ZER_, label %check_ZER_, label %error_ZER_

  check_ZER_:
    %check_ZER_char = load i8, i8* %buf
    %check_ZER_charisspace = call i1 @is_space(i8 %check_ZER_char)
    br i1 %check_ZER_charisspace, label %error_ZER_, label %check_ZER_newline

  check_ZER_newline:
    %check_ZER_charisnewline = icmp eq i8 %check_ZER_char, 10
    br i1 %check_ZER_charisnewline, label %error_ZER_, label %check_ZER_O

  check_ZER_O:
    %check_ZER_charisO = icmp eq i8 %check_ZER_char, 79
    br i1 %check_ZER_charisO, label %read_ZERO_, label %error_ZER_char

  error_ZER_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_ZER_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_ZER_char, i8* %buf
    br label %error_invalid_char


  read_ZERO_:
    %count_ZERO_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_ZERO_ = icmp eq i32 %count_ZERO_, 1
    br i1 %countis1_ZERO_, label %check_ZERO_, label %ret_digit_newline

  check_ZERO_:
    %check_ZERO_char = load i8, i8* %buf
    %check_ZERO_charisspace = call i1 @is_space(i8 %check_ZERO_char)
    br i1 %check_ZERO_charisspace, label %ret_digit_space, label %check_ZERO_newline

  check_ZERO_newline:
    %check_ZERO_charisnewline = icmp eq i8 %check_ZERO_char, 10
    br i1 %check_ZERO_charisnewline, label %ret_digit_newline, label %error_ZERO_char

  error_ZERO_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 90, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_ZERO_char, i8* %buf
    br label %error_invalid_char


  read_T_:
    %count_T_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_T_ = icmp eq i32 %count_T_, 1
    br i1 %countis1_T_, label %check_T_, label %error_T_

  check_T_:
    %check_T_char = load i8, i8* %buf
    %check_T_charisspace = call i1 @is_space(i8 %check_T_char)
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
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_T_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_T_char, i8* %buf
    br label %error_invalid_char


  read_TW_:
    %count_TW_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_TW_ = icmp eq i32 %count_TW_, 1
    br i1 %countis1_TW_, label %check_TW_, label %error_TW_

  check_TW_:
    %check_TW_char = load i8, i8* %buf
    %check_TW_charisspace = call i1 @is_space(i8 %check_TW_char)
    br i1 %check_TW_charisspace, label %error_TW_, label %check_TW_newline

  check_TW_newline:
    %check_TW_charisnewline = icmp eq i8 %check_TW_char, 10
    br i1 %check_TW_charisnewline, label %error_TW_, label %check_TW_O

  check_TW_O:
    %check_TW_charisO = icmp eq i8 %check_TW_char, 79
    br i1 %check_TW_charisO, label %read_TWO_, label %error_TW_char

  error_TW_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 87, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_TW_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 87, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_TW_char, i8* %buf
    br label %error_invalid_char


  read_TWO_:
    %count_TWO_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_TWO_ = icmp eq i32 %count_TWO_, 1
    br i1 %countis1_TWO_, label %check_TWO_, label %ret_digit_newline

  check_TWO_:
    %check_TWO_char = load i8, i8* %buf
    %check_TWO_charisspace = call i1 @is_space(i8 %check_TWO_char)
    br i1 %check_TWO_charisspace, label %ret_digit_space, label %check_TWO_newline

  check_TWO_newline:
    %check_TWO_charisnewline = icmp eq i8 %check_TWO_char, 10
    br i1 %check_TWO_charisnewline, label %ret_digit_newline, label %error_TWO_char

  error_TWO_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 87, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_TWO_char, i8* %buf
    br label %error_invalid_char


  read_TH_:
    %count_TH_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_TH_ = icmp eq i32 %count_TH_, 1
    br i1 %countis1_TH_, label %check_TH_, label %error_TH_

  check_TH_:
    %check_TH_char = load i8, i8* %buf
    %check_TH_charisspace = call i1 @is_space(i8 %check_TH_char)
    br i1 %check_TH_charisspace, label %error_TH_, label %check_TH_newline

  check_TH_newline:
    %check_TH_charisnewline = icmp eq i8 %check_TH_char, 10
    br i1 %check_TH_charisnewline, label %error_TH_, label %check_TH_R

  check_TH_R:
    %check_TH_charisR = icmp eq i8 %check_TH_char, 82
    br i1 %check_TH_charisR, label %read_THR_, label %error_TH_char

  error_TH_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_TH_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_TH_char, i8* %buf
    br label %error_invalid_char


  read_THR_:
    %count_THR_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_THR_ = icmp eq i32 %count_THR_, 1
    br i1 %countis1_THR_, label %check_THR_, label %error_THR_

  check_THR_:
    %check_THR_char = load i8, i8* %buf
    %check_THR_charisspace = call i1 @is_space(i8 %check_THR_char)
    br i1 %check_THR_charisspace, label %error_THR_, label %check_THR_newline

  check_THR_newline:
    %check_THR_charisnewline = icmp eq i8 %check_THR_char, 10
    br i1 %check_THR_charisnewline, label %error_THR_, label %check_THR_E

  check_THR_E:
    %check_THR_charisE = icmp eq i8 %check_THR_char, 69
    br i1 %check_THR_charisE, label %read_THRE_, label %error_THR_char

  error_THR_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_THR_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_THR_char, i8* %buf
    br label %error_invalid_char


  read_THRE_:
    %count_THRE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_THRE_ = icmp eq i32 %count_THRE_, 1
    br i1 %countis1_THRE_, label %check_THRE_, label %error_THRE_

  check_THRE_:
    %check_THRE_char = load i8, i8* %buf
    %check_THRE_charisspace = call i1 @is_space(i8 %check_THRE_char)
    br i1 %check_THRE_charisspace, label %error_THRE_, label %check_THRE_newline

  check_THRE_newline:
    %check_THRE_charisnewline = icmp eq i8 %check_THRE_char, 10
    br i1 %check_THRE_charisnewline, label %error_THRE_, label %check_THRE_E

  check_THRE_E:
    %check_THRE_charisE = icmp eq i8 %check_THRE_char, 69
    br i1 %check_THRE_charisE, label %read_THREE_, label %error_THRE_char

  error_THRE_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_THRE_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_THRE_char, i8* %buf
    br label %error_invalid_char


  read_THREE_:
    %count_THREE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_THREE_ = icmp eq i32 %count_THREE_, 1
    br i1 %countis1_THREE_, label %check_THREE_, label %ret_digit_newline

  check_THREE_:
    %check_THREE_char = load i8, i8* %buf
    %check_THREE_charisspace = call i1 @is_space(i8 %check_THREE_char)
    br i1 %check_THREE_charisspace, label %ret_digit_space, label %check_THREE_newline

  check_THREE_newline:
    %check_THREE_charisnewline = icmp eq i8 %check_THREE_char, 10
    br i1 %check_THREE_charisnewline, label %ret_digit_newline, label %error_THREE_char

  error_THREE_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    ; store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_THREE_char, i8* %buf
    br label %error_invalid_char


  read_F_:
    %count_F_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_F_ = icmp eq i32 %count_F_, 1
    br i1 %countis1_F_, label %check_F_, label %error_F_

  check_F_:
    %check_F_char = load i8, i8* %buf
    %check_F_charisspace = call i1 @is_space(i8 %check_F_char)
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
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_F_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_F_char, i8* %buf
    br label %error_invalid_char


  read_FO_:
    %count_FO_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FO_ = icmp eq i32 %count_FO_, 1
    br i1 %countis1_FO_, label %check_FO_, label %error_FO_

  check_FO_:
    %check_FO_char = load i8, i8* %buf
    %check_FO_charisspace = call i1 @is_space(i8 %check_FO_char)
    br i1 %check_FO_charisspace, label %error_FO_, label %check_FO_newline

  check_FO_newline:
    %check_FO_charisnewline = icmp eq i8 %check_FO_char, 10
    br i1 %check_FO_charisnewline, label %error_FO_, label %check_FO_U

  check_FO_U:
    %check_FO_charisU = icmp eq i8 %check_FO_char, 85
    br i1 %check_FO_charisU, label %read_FOU_, label %error_FO_char

  error_FO_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_FO_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_FO_char, i8* %buf
    br label %error_invalid_char


  read_FOU_:
    %count_FOU_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FOU_ = icmp eq i32 %count_FOU_, 1
    br i1 %countis1_FOU_, label %check_FOU_, label %error_FOU_

  check_FOU_:
    %check_FOU_char = load i8, i8* %buf
    %check_FOU_charisspace = call i1 @is_space(i8 %check_FOU_char)
    br i1 %check_FOU_charisspace, label %error_FOU_, label %check_FOU_newline

  check_FOU_newline:
    %check_FOU_charisnewline = icmp eq i8 %check_FOU_char, 10
    br i1 %check_FOU_charisnewline, label %error_FOU_, label %check_FOU_R

  check_FOU_R:
    %check_FOU_charisR = icmp eq i8 %check_FOU_char, 82
    br i1 %check_FOU_charisR, label %read_FOUR_, label %error_FOU_char

  error_FOU_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 85, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_FOU_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 85, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_FOU_char, i8* %buf
    br label %error_invalid_char


  read_FOUR_:
    %count_FOUR_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FOUR_ = icmp eq i32 %count_FOUR_, 1
    br i1 %countis1_FOUR_, label %check_FOUR_, label %ret_digit_newline

  check_FOUR_:
    %check_FOUR_char = load i8, i8* %buf
    %check_FOUR_charisspace = call i1 @is_space(i8 %check_FOUR_char)
    br i1 %check_FOUR_charisspace, label %ret_digit_space, label %check_FOUR_newline

  check_FOUR_newline:
    %check_FOUR_charisnewline = icmp eq i8 %check_FOUR_char, 10
    br i1 %check_FOUR_charisnewline, label %ret_digit_newline, label %error_FOUR_char

  error_FOUR_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 79, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 85, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 82, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_FOUR_char, i8* %buf
    br label %error_invalid_char


  read_FI_:
    %count_FI_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FI_ = icmp eq i32 %count_FI_, 1
    br i1 %countis1_FI_, label %check_FI_, label %error_FI_

  check_FI_:
    %check_FI_char = load i8, i8* %buf
    %check_FI_charisspace = call i1 @is_space(i8 %check_FI_char)
    br i1 %check_FI_charisspace, label %error_FI_, label %check_FI_newline

  check_FI_newline:
    %check_FI_charisnewline = icmp eq i8 %check_FI_char, 10
    br i1 %check_FI_charisnewline, label %error_FI_, label %check_FI_V

  check_FI_V:
    %check_FI_charisV = icmp eq i8 %check_FI_char, 86
    br i1 %check_FI_charisV, label %read_FIV_, label %error_FI_char

  error_FI_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_FI_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_FI_char, i8* %buf
    br label %error_invalid_char


  read_FIV_:
    %count_FIV_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FIV_ = icmp eq i32 %count_FIV_, 1
    br i1 %countis1_FIV_, label %check_FIV_, label %error_FIV_

  check_FIV_:
    %check_FIV_char = load i8, i8* %buf
    %check_FIV_charisspace = call i1 @is_space(i8 %check_FIV_char)
    br i1 %check_FIV_charisspace, label %error_FIV_, label %check_FIV_newline

  check_FIV_newline:
    %check_FIV_charisnewline = icmp eq i8 %check_FIV_char, 10
    br i1 %check_FIV_charisnewline, label %error_FIV_, label %check_FIV_E

  check_FIV_E:
    %check_FIV_charisE = icmp eq i8 %check_FIV_char, 69
    br i1 %check_FIV_charisE, label %read_FIVE_, label %error_FIV_char

  error_FIV_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_FIV_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_FIV_char, i8* %buf
    br label %error_invalid_char


  read_FIVE_:
    %count_FIVE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_FIVE_ = icmp eq i32 %count_FIVE_, 1
    br i1 %countis1_FIVE_, label %check_FIVE_, label %ret_digit_newline

  check_FIVE_:
    %check_FIVE_char = load i8, i8* %buf
    %check_FIVE_charisspace = call i1 @is_space(i8 %check_FIVE_char)
    br i1 %check_FIVE_charisspace, label %ret_digit_space, label %check_FIVE_newline

  check_FIVE_newline:
    %check_FIVE_charisnewline = icmp eq i8 %check_FIVE_char, 10
    br i1 %check_FIVE_charisnewline, label %ret_digit_newline, label %error_FIVE_char

  error_FIVE_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 70, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_FIVE_char, i8* %buf
    br label %error_invalid_char


  read_S_:
    %count_S_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_S_ = icmp eq i32 %count_S_, 1
    br i1 %countis1_S_, label %check_S_, label %error_S_

  check_S_:
    %check_S_char = load i8, i8* %buf
    %check_S_charisspace = call i1 @is_space(i8 %check_S_char)
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
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_S_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_S_char, i8* %buf
    br label %error_invalid_char


  read_SI_:
    %count_SI_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SI_ = icmp eq i32 %count_SI_, 1
    br i1 %countis1_SI_, label %check_SI_, label %error_SI_

  check_SI_:
    %check_SI_char = load i8, i8* %buf
    %check_SI_charisspace = call i1 @is_space(i8 %check_SI_char)
    br i1 %check_SI_charisspace, label %error_SI_, label %check_SI_newline

  check_SI_newline:
    %check_SI_charisnewline = icmp eq i8 %check_SI_char, 10
    br i1 %check_SI_charisnewline, label %error_SI_, label %check_SI_X

  check_SI_X:
    %check_SI_charisX = icmp eq i8 %check_SI_char, 88
    br i1 %check_SI_charisX, label %read_SIX_, label %error_SI_char

  error_SI_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_SI_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_SI_char, i8* %buf
    br label %error_invalid_char


  read_SIX_:
    %count_SIX_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SIX_ = icmp eq i32 %count_SIX_, 1
    br i1 %countis1_SIX_, label %check_SIX_, label %ret_digit_newline

  check_SIX_:
    %check_SIX_char = load i8, i8* %buf
    %check_SIX_charisspace = call i1 @is_space(i8 %check_SIX_char)
    br i1 %check_SIX_charisspace, label %ret_digit_space, label %check_SIX_newline

  check_SIX_newline:
    %check_SIX_charisnewline = icmp eq i8 %check_SIX_char, 10
    br i1 %check_SIX_charisnewline, label %ret_digit_newline, label %error_SIX_char

  error_SIX_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 88, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_SIX_char, i8* %buf
    br label %error_invalid_char


  read_SE_:
    %count_SE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SE_ = icmp eq i32 %count_SE_, 1
    br i1 %countis1_SE_, label %check_SE_, label %error_SE_

  check_SE_:
    %check_SE_char = load i8, i8* %buf
    %check_SE_charisspace = call i1 @is_space(i8 %check_SE_char)
    br i1 %check_SE_charisspace, label %error_SE_, label %check_SE_newline

  check_SE_newline:
    %check_SE_charisnewline = icmp eq i8 %check_SE_char, 10
    br i1 %check_SE_charisnewline, label %error_SE_, label %check_SE_V

  check_SE_V:
    %check_SE_charisV = icmp eq i8 %check_SE_char, 86
    br i1 %check_SE_charisV, label %read_SEV_, label %error_SE_char

  error_SE_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_SE_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_SE_char, i8* %buf
    br label %error_invalid_char


  read_SEV_:
    %count_SEV_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SEV_ = icmp eq i32 %count_SEV_, 1
    br i1 %countis1_SEV_, label %check_SEV_, label %error_SEV_

  check_SEV_:
    %check_SEV_char = load i8, i8* %buf
    %check_SEV_charisspace = call i1 @is_space(i8 %check_SEV_char)
    br i1 %check_SEV_charisspace, label %error_SEV_, label %check_SEV_newline

  check_SEV_newline:
    %check_SEV_charisnewline = icmp eq i8 %check_SEV_char, 10
    br i1 %check_SEV_charisnewline, label %error_SEV_, label %check_SEV_E

  check_SEV_E:
    %check_SEV_charisE = icmp eq i8 %check_SEV_char, 69
    br i1 %check_SEV_charisE, label %read_SEVE_, label %error_SEV_char

  error_SEV_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_SEV_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_SEV_char, i8* %buf
    br label %error_invalid_char


  read_SEVE_:
    %count_SEVE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SEVE_ = icmp eq i32 %count_SEVE_, 1
    br i1 %countis1_SEVE_, label %check_SEVE_, label %error_SEVE_

  check_SEVE_:
    %check_SEVE_char = load i8, i8* %buf
    %check_SEVE_charisspace = call i1 @is_space(i8 %check_SEVE_char)
    br i1 %check_SEVE_charisspace, label %error_SEVE_, label %check_SEVE_newline

  check_SEVE_newline:
    %check_SEVE_charisnewline = icmp eq i8 %check_SEVE_char, 10
    br i1 %check_SEVE_charisnewline, label %error_SEVE_, label %check_SEVE_N

  check_SEVE_N:
    %check_SEVE_charisN = icmp eq i8 %check_SEVE_char, 78
    br i1 %check_SEVE_charisN, label %read_SEVEN_, label %error_SEVE_char

  error_SEVE_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_SEVE_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_SEVE_char, i8* %buf
    br label %error_invalid_char


  read_SEVEN_:
    %count_SEVEN_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_SEVEN_ = icmp eq i32 %count_SEVEN_, 1
    br i1 %countis1_SEVEN_, label %check_SEVEN_, label %ret_digit_newline

  check_SEVEN_:
    %check_SEVEN_char = load i8, i8* %buf
    %check_SEVEN_charisspace = call i1 @is_space(i8 %check_SEVEN_char)
    br i1 %check_SEVEN_charisspace, label %ret_digit_space, label %check_SEVEN_newline

  check_SEVEN_newline:
    %check_SEVEN_charisnewline = icmp eq i8 %check_SEVEN_char, 10
    br i1 %check_SEVEN_charisnewline, label %ret_digit_newline, label %error_SEVEN_char

  error_SEVEN_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 83, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 86, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_SEVEN_char, i8* %buf
    br label %error_invalid_char


  read_E_:
    %count_E_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_E_ = icmp eq i32 %count_E_, 1
    br i1 %countis1_E_, label %check_E_, label %error_E_

  check_E_:
    %check_E_char = load i8, i8* %buf
    %check_E_charisspace = call i1 @is_space(i8 %check_E_char)
    br i1 %check_E_charisspace, label %error_E_, label %check_E_newline

  check_E_newline:
    %check_E_charisnewline = icmp eq i8 %check_E_char, 10
    br i1 %check_E_charisnewline, label %error_E_, label %check_E_I

  check_E_I:
    %check_E_charisI = icmp eq i8 %check_E_char, 73
    br i1 %check_E_charisI, label %read_EI_, label %error_E_char

  error_E_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_E_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_E_char, i8* %buf
    br label %error_invalid_char


  read_EI_:
    %count_EI_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_EI_ = icmp eq i32 %count_EI_, 1
    br i1 %countis1_EI_, label %check_EI_, label %error_EI_

  check_EI_:
    %check_EI_char = load i8, i8* %buf
    %check_EI_charisspace = call i1 @is_space(i8 %check_EI_char)
    br i1 %check_EI_charisspace, label %error_EI_, label %check_EI_newline

  check_EI_newline:
    %check_EI_charisnewline = icmp eq i8 %check_EI_char, 10
    br i1 %check_EI_charisnewline, label %error_EI_, label %check_EI_G

  check_EI_G:
    %check_EI_charisG = icmp eq i8 %check_EI_char, 71
    br i1 %check_EI_charisG, label %read_EIG_, label %error_EI_char

  error_EI_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_EI_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_EI_char, i8* %buf
    br label %error_invalid_char


  read_EIG_:
    %count_EIG_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_EIG_ = icmp eq i32 %count_EIG_, 1
    br i1 %countis1_EIG_, label %check_EIG_, label %error_EIG_

  check_EIG_:
    %check_EIG_char = load i8, i8* %buf
    %check_EIG_charisspace = call i1 @is_space(i8 %check_EIG_char)
    br i1 %check_EIG_charisspace, label %error_EIG_, label %check_EIG_newline

  check_EIG_newline:
    %check_EIG_charisnewline = icmp eq i8 %check_EIG_char, 10
    br i1 %check_EIG_charisnewline, label %error_EIG_, label %check_EIG_H

  check_EIG_H:
    %check_EIG_charisH = icmp eq i8 %check_EIG_char, 72
    br i1 %check_EIG_charisH, label %read_EIGH_, label %error_EIG_char

  error_EIG_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_EIG_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_EIG_char, i8* %buf
    br label %error_invalid_char


  read_EIGH_:
    %count_EIGH_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_EIGH_ = icmp eq i32 %count_EIGH_, 1
    br i1 %countis1_EIGH_, label %check_EIGH_, label %error_EIGH_

  check_EIGH_:
    %check_EIGH_char = load i8, i8* %buf
    %check_EIGH_charisspace = call i1 @is_space(i8 %check_EIGH_char)
    br i1 %check_EIGH_charisspace, label %error_EIGH_, label %check_EIGH_newline

  check_EIGH_newline:
    %check_EIGH_charisnewline = icmp eq i8 %check_EIGH_char, 10
    br i1 %check_EIGH_charisnewline, label %error_EIGH_, label %check_EIGH_T

  check_EIGH_T:
    %check_EIGH_charisT = icmp eq i8 %check_EIGH_char, 84
    br i1 %check_EIGH_charisT, label %read_EIGHT_, label %error_EIGH_char

  error_EIGH_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_EIGH_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_EIGH_char, i8* %buf
    br label %error_invalid_char


  read_EIGHT_:
    %count_EIGHT_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_EIGHT_ = icmp eq i32 %count_EIGHT_, 1
    br i1 %countis1_EIGHT_, label %check_EIGHT_, label %ret_digit_newline

  check_EIGHT_:
    %check_EIGHT_char = load i8, i8* %buf
    %check_EIGHT_charisspace = call i1 @is_space(i8 %check_EIGHT_char)
    br i1 %check_EIGHT_charisspace, label %ret_digit_space, label %check_EIGHT_newline

  check_EIGHT_newline:
    %check_EIGHT_charisnewline = icmp eq i8 %check_EIGHT_char, 10
    br i1 %check_EIGHT_charisnewline, label %ret_digit_newline, label %error_EIGHT_char

  error_EIGHT_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 71, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 72, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 84, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_EIGHT_char, i8* %buf
    br label %error_invalid_char


  read_N_:
    %count_N_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_N_ = icmp eq i32 %count_N_, 1
    br i1 %countis1_N_, label %check_N_, label %error_N_

  check_N_:
    %check_N_char = load i8, i8* %buf
    %check_N_charisspace = call i1 @is_space(i8 %check_N_char)
    br i1 %check_N_charisspace, label %error_N_, label %check_N_newline

  check_N_newline:
    %check_N_charisnewline = icmp eq i8 %check_N_char, 10
    br i1 %check_N_charisnewline, label %error_N_, label %check_N_I

  check_N_I:
    %check_N_charisI = icmp eq i8 %check_N_char, 73
    br i1 %check_N_charisI, label %read_NI_, label %error_N_char

  error_N_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_N_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_N_char, i8* %buf
    br label %error_invalid_char


  read_NI_:
    %count_NI_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_NI_ = icmp eq i32 %count_NI_, 1
    br i1 %countis1_NI_, label %check_NI_, label %error_NI_

  check_NI_:
    %check_NI_char = load i8, i8* %buf
    %check_NI_charisspace = call i1 @is_space(i8 %check_NI_char)
    br i1 %check_NI_charisspace, label %error_NI_, label %check_NI_newline

  check_NI_newline:
    %check_NI_charisnewline = icmp eq i8 %check_NI_char, 10
    br i1 %check_NI_charisnewline, label %error_NI_, label %check_NI_N

  check_NI_N:
    %check_NI_charisN = icmp eq i8 %check_NI_char, 78
    br i1 %check_NI_charisN, label %read_NIN_, label %error_NI_char

  error_NI_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_NI_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_NI_char, i8* %buf
    br label %error_invalid_char


  read_NIN_:
    %count_NIN_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_NIN_ = icmp eq i32 %count_NIN_, 1
    br i1 %countis1_NIN_, label %check_NIN_, label %error_NIN_

  check_NIN_:
    %check_NIN_char = load i8, i8* %buf
    %check_NIN_charisspace = call i1 @is_space(i8 %check_NIN_char)
    br i1 %check_NIN_charisspace, label %error_NIN_, label %check_NIN_newline

  check_NIN_newline:
    %check_NIN_charisnewline = icmp eq i8 %check_NIN_char, 10
    br i1 %check_NIN_charisnewline, label %error_NIN_, label %check_NIN_E

  check_NIN_E:
    %check_NIN_charisE = icmp eq i8 %check_NIN_char, 69
    br i1 %check_NIN_charisE, label %read_NINE_, label %error_NIN_char

  error_NIN_:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %ret_invalid

  error_NIN_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 %check_NIN_char, i8* %buf
    br label %error_invalid_char


  read_NINE_:
    %count_NINE_ = call i32 @read(i32 0, i8* %buf, i32 1)
    %countis1_NINE_ = icmp eq i32 %count_NINE_, 1
    br i1 %countis1_NINE_, label %check_NINE_, label %ret_digit_newline

  check_NINE_:
    %check_NINE_char = load i8, i8* %buf
    %check_NINE_charisspace = call i1 @is_space(i8 %check_NINE_char)
    br i1 %check_NINE_charisspace, label %ret_digit_space, label %check_NINE_newline

  check_NINE_newline:
    %check_NINE_charisnewline = icmp eq i8 %check_NINE_char, 10
    br i1 %check_NINE_charisnewline, label %ret_digit_newline, label %error_NINE_char

  error_NINE_char:
    call i32 @write(i32 2, i8* getelementptr([43 x i8], [43 x i8]* @read_digit_error, i32 0, i32 0), i32 43)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 73, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 78, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    store i8 69, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
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
    call i32 @write(i32 2, i8* %buf, i32 1)
    br label %error_invalid

  error_invalid:
    %error_invalid_count = call i32 @read(i32 0, i8* %buf, i32 1)
    %error_invalid_countis1 = icmp eq i32 %error_invalid_count, 1
    br i1 %error_invalid_countis1, label %error_invalid_check_space, label %ret_invalid

  error_invalid_check_space:
    %error_invalid_check_char = load i8, i8* %buf
    %error_invalid_check_charisspace = call i1 @is_space(i8 %error_invalid_check_char)
    br i1 %error_invalid_check_charisspace, label %ret_invalid, label %error_invalid_check_newline

  error_invalid_check_newline:
    %error_invalid_check_charisnewline = icmp eq i8 %error_invalid_check_char, 10
    br i1 %error_invalid_check_charisnewline, label %ret_invalid, label %error_invalid_char

  ret_invalid:
    store i8 63, i8* %buf
    call i32 @write(i32 2, i8* %buf, i32 1)
    ret {i4,i32} insertvalue({i4,i32} zeroinitializer, i4 4, 0)
}

define %val @input16() {
  entry:
    br label %loop

  loop:
    %total = phi i32 [0, %entry], [%next_total, %next_digit_good]
    %is_empty = phi i1 [1, %entry], [0, %next_digit_good]
    %digit_struct = call {i4,i32} @read_digit()
    %digit_tag = extractvalue {i4,i32} %digit_struct, 0
    %digit_tagis0 = icmp eq i4 %digit_tag, 0
    br i1 %digit_tagis0, label %read_next_digit, label %check_tag1

  check_tag1:
    %digit_tagis1 = icmp eq i4 %digit_tag, 1
    br i1 %digit_tagis1, label %read_next_digit, label %check_tag2

  check_tag2:
    %digit_tagis2 = icmp eq i4 %digit_tag, 2
    br i1 %digit_tagis2, label %read_eol, label %check_tag3

  check_tag3:
    %digit_tagis3 = icmp eq i4 %digit_tag, 3
    br i1 %digit_tagis3, label %read_eof, label %ret_err

  read_next_digit:
    %totalx10 = mul i32 %total, 10
    %next_digit = extractvalue {i4,i32} %digit_struct, 1
    %next_total = add i32 %totalx10, %next_digit
    %check6553 = icmp ult i32 %total, 6553
    br i1 %check6553, label %next_digit_good, label %next_digit_maybe

  next_digit_good:
    br i1 %digit_tagis0, label %loop, label %ret_number

  next_digit_maybe:
    %check65530 = icmp eq i32 %total, 6553
    br i1 %check65530, label %next_digit_maybe2, label %ret_err

  next_digit_maybe2:
    %check6 = icmp ult i32 %next_digit, 6
    br i1 %check6, label %next_digit_good, label %ret_err

  read_eol:
    br i1 %is_empty, label %ret_err, label %ret_number

  read_eof:
    br i1 %is_empty, label %ret_err, label %ret_number

  ret_err:
    %err = phi %val [insertvalue(%val insertvalue(%val zeroinitializer,i2 3,0),i32 579,1),%check_tag3],
                   [insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 275,1),%next_digit_maybe],
                   [insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 275,1),%next_digit_maybe2],
                   [insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 562,1),%read_eol],
                   [insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 562,1),%read_eof]
    ret %val %err

  ret_number:
    %number = phi i32 [%next_total, %next_digit_good], [%total, %read_eol], [%total, %read_eof]
    %result = insertvalue %val zeroinitializer, i32 %number, 1
    ret %val %result
}

define %val @input32() {
  entry:
    br label %loop

  loop:
    %total = phi i32 [0, %entry], [%next_total, %next_digit_good]
    %is_empty = phi i1 [1, %entry], [0, %next_digit_good]
    %digit_struct = call {i4,i32} @read_digit()
    %digit_tag = extractvalue {i4,i32} %digit_struct, 0
    %digit_tagis0 = icmp eq i4 %digit_tag, 0
    br i1 %digit_tagis0, label %read_next_digit, label %check_tag1

  check_tag1:
    %digit_tagis1 = icmp eq i4 %digit_tag, 1
    br i1 %digit_tagis1, label %read_next_digit, label %check_tag2

  check_tag2:
    %digit_tagis2 = icmp eq i4 %digit_tag, 2
    br i1 %digit_tagis2, label %read_eol, label %check_tag3

  check_tag3:
    %digit_tagis3 = icmp eq i4 %digit_tag, 3
    br i1 %digit_tagis3, label %read_eof, label %ret_err

  read_next_digit:
    %totalx10 = mul i32 %total, 10
    %next_digit = extractvalue {i4,i32} %digit_struct, 1
    %next_total = add i32 %totalx10, %next_digit
    %check6553 = icmp ult i32 %total, 429496729
    br i1 %check6553, label %next_digit_good, label %next_digit_maybe

  next_digit_good:
    br i1 %digit_tagis0, label %loop, label %ret_number

  next_digit_maybe:
    %check65530 = icmp eq i32 %total, 429496729
    br i1 %check65530, label %next_digit_maybe2, label %ret_err

  next_digit_maybe2:
    %check6 = icmp ult i32 %next_digit, 6
    br i1 %check6, label %next_digit_good, label %ret_err

  read_eol:
    br i1 %is_empty, label %ret_err, label %ret_number

  read_eof:
    br i1 %is_empty, label %ret_err, label %ret_number

  ret_err:
    %err = phi %val [insertvalue(%val insertvalue(%val zeroinitializer,i2 3,0),i32 579,1),%check_tag3],
                   [insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 533,1),%next_digit_maybe],
                   [insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 533,1),%next_digit_maybe2],
                   [insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 562,1),%read_eol],
                   [insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 562,1),%read_eof]
    ret %val %err

  ret_number:
    %number = phi i32 [%next_total, %next_digit_good], [%total, %read_eol], [%total, %read_eof]
    %result = insertvalue %val zeroinitializer, i32 %number, 1
    %result32 = insertvalue %val %result, i2 1, 0
    ret %val %result32
}

@output_data = private constant [20 x i8] [
    i8 226, i8 128, i8 190, ; zero: 0:2
    i8 10, ; newline 3
    i8 204, i8 133, ; overline 4:5
    i8 73, i8 86, i8 88, i8 76, i8 67, i8 68, i8 77, ; IVXLCDM 6:12
    i8 105, i8 118, i8 120, i8 108, i8 99, i8 100, i8 109 ; ivxlcdm 13:19
]

define void @output(i32 %n) {
    %niszero = icmp eq i32 %n, 0
    br i1 %niszero, label %output0, label %output_4gig

  output0:
    call i32 @write(i32 1, i8* getelementptr([20 x i8], [20 x i8]* @output_data, i32 0, i32 0), i32 4)
    ret void

  output_4gig:
    %n_ge4gig = icmp uge i32 %n, 4000000000
    br i1 %n_ge4gig, label %write_4gig, label %output_4meg

  write_4gig:
    %n_mod1gig = urem i32 %n, 1000000000
    %n_div1gig = udiv i32 %n, 1000000000
    call void @output_portion(i32 %n_div1gig, i32 13, i1 1)
    br label %output_4meg

  output_4meg:
    %n_4meg = phi i32 [%n, %output_4gig], [%n_mod1gig, %write_4gig]
    %n_ge4meg = icmp uge i32 %n_4meg, 4000000
    br i1 %n_ge4meg, label %write_4meg, label %output_4k

  write_4meg:
    %n_mod1meg = urem i32 %n_4meg, 1000000
    %n_div1meg = udiv i32 %n_4meg, 1000000
    call void @output_portion(i32 %n_div1meg, i32 13, i1 0)
    br label %output_4k

  output_4k:
    %n_4k = phi i32 [%n_4meg, %output_4meg], [%n_mod1meg, %write_4meg]
    %n_ge4k = icmp uge i32 %n_4k, 4000
    br i1 %n_ge4k, label %write_4k, label %output_1

  write_4k:
    %n_mod1k = urem i32 %n_4k, 1000
    %n_div1k = udiv i32 %n_4k, 1000
    call void @output_portion(i32 %n_div1k, i32 6, i1 1)
    br label %output_1

  output_1:
    %n_1 = phi i32 [%n_4k, %output_4k], [%n_mod1k, %write_4k]
    call void @output_portion(i32 %n_1, i32 6, i1 0)
    call i32 @write(i32 1, i8* getelementptr([20 x i8], [20 x i8]* @output_data, i32 0, i32 3), i32 1)
    ret void
}

define void @output_portion(i32 %n, i32 %index, i1 %overline) {
    %ptrI = getelementptr [20 x i8], [20 x i8]* @output_data, i32 0, i32 %index
    %indexV = add i32 %index, 1
    %ptrV = getelementptr [20 x i8], [20 x i8]* @output_data, i32 0, i32 %indexV
    %indexX = add i32 %index, 2
    %ptrX = getelementptr [20 x i8], [20 x i8]* @output_data, i32 0, i32 %indexX
    %indexL = add i32 %index, 3
    %ptrL = getelementptr [20 x i8], [20 x i8]* @output_data, i32 0, i32 %indexL
    %indexC = add i32 %index, 4
    %ptrC = getelementptr [20 x i8], [20 x i8]* @output_data, i32 0, i32 %indexC
    %indexD = add i32 %index, 5
    %ptrD = getelementptr [20 x i8], [20 x i8]* @output_data, i32 0, i32 %indexD
    %indexM = add i32 %index, 6
    %ptrM = getelementptr [20 x i8], [20 x i8]* @output_data, i32 0, i32 %indexM

    %n_div1000 = udiv i32 %n, 1000
    call void @output_decade(i32 %n_div1000, i8* %ptrM, i8* %ptrL, i8* %ptrV, i1 %overline)

    %n_mod1000 = urem i32 %n, 1000
    %n_mod100 = urem i32 %n, 100
    %n_mod10 = urem i32 %n, 10
    %n_mod1000_eq999 = icmp eq i32 %n_mod1000, 999
    br i1 %n_mod1000_eq999, label %out999, label %check990

  out999:
    call void @output_digit(i8* %ptrI, i1 %overline)
    call void @output_digit(i8* %ptrM, i1 %overline)
    ret void

  check990:
    %n_mod1000_ge990 = icmp uge i32 %n_mod1000, 990
    br i1 %n_mod1000_ge990, label %out990, label %check499

  out990:
    call void @output_digit(i8* %ptrX, i1 %overline)
    call void @output_digit(i8* %ptrM, i1 %overline)
    call void @output_decade(i32 %n_mod10, i8* %ptrI, i8* %ptrV, i8* %ptrX, i1 %overline)
    ret void

  check499:
    %n_mod1000_eq499 = icmp eq i32 %n_mod1000, 499
    br i1 %n_mod1000_eq499, label %out499, label %check490

  out499:
    call void @output_digit(i8* %ptrI, i1 %overline)
    call void @output_digit(i8* %ptrD, i1 %overline)
    ret void

  check490:
    %n_mod1000_ge490 = icmp uge i32 %n_mod1000, 490
    br i1 %n_mod1000_ge490, label %check500, label %out100s

  check500:
    %n_mod1000_ge500 = icmp uge i32 %n_mod1000, 500
    br i1 %n_mod1000_ge500, label %out100s, label %out490

  out490:
    call void @output_digit(i8* %ptrX, i1 %overline)
    call void @output_digit(i8* %ptrD, i1 %overline)
    call void @output_decade(i32 %n_mod10, i8* %ptrI, i8* %ptrV, i8* %ptrX, i1 %overline)
    ret void

  out100s:
    %n_div100 = udiv i32 %n_mod1000, 100
    call void @output_decade(i32 %n_div100, i8* %ptrC, i8* %ptrD, i8* %ptrM, i1 %overline)
    %n_mod100_eq99 = icmp eq i32 %n_mod100, 99
    br i1 %n_mod100_eq99, label %out99, label %check49

  out99:
    call void @output_digit(i8* %ptrI, i1 %overline)
    call void @output_digit(i8* %ptrC, i1 %overline)
    ret void

  check49:
    %n_mod100_eq49 = icmp eq i32 %n_mod100, 49
    br i1 %n_mod100_eq49, label %out49, label %out10s

  out49:
    call void @output_digit(i8* %ptrI, i1 %overline)
    call void @output_digit(i8* %ptrL, i1 %overline)
    ret void

  out10s:
    %n_div10 = udiv i32 %n_mod100, 10
    call void @output_decade(i32 %n_div10, i8* %ptrX, i8* %ptrL, i8* %ptrC, i1 %overline)
    call void @output_decade(i32 %n_mod10, i8* %ptrI, i8* %ptrV, i8* %ptrX, i1 %overline)
    ret void
}

define void @output_decade(i32 %n, i8* %ptrI, i8* %ptrV, i8* %ptrX, i1 %overline) {
    %is9 = icmp eq i32 %n, 9
    br i1 %is9, label %out9, label %check8

  check8:
    %is8 = icmp eq i32 %n, 8
    br i1 %is8, label %out8, label %check7

  check7:
    %is7 = icmp eq i32 %n, 7
    br i1 %is7, label %out7, label %check6

  check6:
    %is6 = icmp eq i32 %n, 6
    br i1 %is6, label %out6, label %check5

  check5:
    %is5 = icmp eq i32 %n, 5
    br i1 %is5, label %out5, label %check4

  check4:
    %is4 = icmp eq i32 %n, 4
    br i1 %is4, label %out4, label %check3

  check3:
    %is3 = icmp eq i32 %n, 3
    br i1 %is3, label %out3, label %check2

  check2:
    %is2 = icmp eq i32 %n, 2
    br i1 %is2, label %out2, label %check1

  check1:
    %is1 = icmp eq i32 %n, 1
    br i1 %is1, label %out1, label %done

  out9:
    call void @output_digit(i8* %ptrI, i1 %overline)
    call void @output_digit(i8* %ptrX, i1 %overline)
    br label %done

  out8:
    call void @output_digit(i8* %ptrV, i1 %overline)
    br label %out3

  out7:
    call void @output_digit(i8* %ptrV, i1 %overline)
    br label %out2

  out6:
    call void @output_digit(i8* %ptrV, i1 %overline)
    br label %out1

  out5:
    call void @output_digit(i8* %ptrV, i1 %overline)
    br label %done

  out4:
    call void @output_digit(i8* %ptrI, i1 %overline)
    call void @output_digit(i8* %ptrV, i1 %overline)
    br label %done

  out3:
    call void @output_digit(i8* %ptrI, i1 %overline)
    br label %out2

  out2:
    call void @output_digit(i8* %ptrI, i1 %overline)
    br label %out1

  out1:
    call void @output_digit(i8* %ptrI, i1 %overline)
    br label %done

  done:
    ret void
}

define void @output_digit(i8* %ptr_digit, i1 %overline) {
    call i32 @write(i32 1, i8* %ptr_digit, i32 1)
    br i1 %overline, label %out_overline, label %done

  out_overline:
    call i32 @write(i32 1, i8* getelementptr([20 x i8], [20 x i8]* @output_data, i32 0, i32 4), i32 2)
    br label %done

  done:
    ret void
}

define %val @op_mingle(%val %l, %val %r) {
    %ltag = extractvalue %val %l, 0
    %lval = extractvalue %val %l, 1
    %rtag = extractvalue %val %r, 0
    %rval = extractvalue %val %r, 1

    %liserror = icmp uge i2 %ltag, 2
    br i1 %liserror, label %ret_lerror, label %check_rerror

  ret_lerror:
    ret %val %l

  check_rerror:
    %riserror = icmp uge i2 %rtag, 2
    br i1 %riserror, label %ret_rerror, label %check_loverflow

  ret_rerror:
    ret %val %r

  check_loverflow:
    %loverflow = icmp uge i32 %lval, 65536
    br i1 %loverflow, label %ret_overflow, label %check_roverflow

  ret_overflow:
    ret %val insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 533,1)

  check_roverflow:
    %roverflow = icmp uge i32 %rval, 65536
    br i1 %roverflow, label %ret_overflow, label %compute_result

  compute_result:
    %l_8000 = and i32 %lval, shl(i32 1,i32 15)
    %l_4000 = and i32 %lval, shl(i32 1,i32 14)
    %l_2000 = and i32 %lval, shl(i32 1,i32 13)
    %l_1000 = and i32 %lval, shl(i32 1,i32 12)
    %l_800  = and i32 %lval, shl(i32 1,i32 11)
    %l_400  = and i32 %lval, shl(i32 1,i32 10)
    %l_200  = and i32 %lval, shl(i32 1,i32 9)
    %l_100  = and i32 %lval, shl(i32 1,i32 8)
    %l_80   = and i32 %lval, shl(i32 1,i32 7)
    %l_40   = and i32 %lval, shl(i32 1,i32 6)
    %l_20   = and i32 %lval, shl(i32 1,i32 5)
    %l_10   = and i32 %lval, shl(i32 1,i32 4)
    %l_8    = and i32 %lval, shl(i32 1,i32 3)
    %l_4    = and i32 %lval, shl(i32 1,i32 2)
    %l_2    = and i32 %lval, shl(i32 1,i32 1)
    %l_1    = and i32 %lval, shl(i32 1,i32 0)

    %r_8000 = and i32 %rval, shl(i32 1,i32 15)
    %r_4000 = and i32 %rval, shl(i32 1,i32 14)
    %r_2000 = and i32 %rval, shl(i32 1,i32 13)
    %r_1000 = and i32 %rval, shl(i32 1,i32 12)
    %r_800  = and i32 %rval, shl(i32 1,i32 11)
    %r_400  = and i32 %rval, shl(i32 1,i32 10)
    %r_200  = and i32 %rval, shl(i32 1,i32 9)
    %r_100  = and i32 %rval, shl(i32 1,i32 8)
    %r_80   = and i32 %rval, shl(i32 1,i32 7)
    %r_40   = and i32 %rval, shl(i32 1,i32 6)
    %r_20   = and i32 %rval, shl(i32 1,i32 5)
    %r_10   = and i32 %rval, shl(i32 1,i32 4)
    %r_8    = and i32 %rval, shl(i32 1,i32 3)
    %r_4    = and i32 %rval, shl(i32 1,i32 2)
    %r_2    = and i32 %rval, shl(i32 1,i32 1)
    %r_1    = and i32 %rval, shl(i32 1,i32 0)

    %l_8000_80000000 = shl i32 %l_8000, 16
    %l_4000_20000000 = shl i32 %l_4000, 15
    %l_2000__8000000 = shl i32 %l_2000, 14
    %l_1000__2000000 = shl i32 %l_1000, 13
    %l__800___800000 = shl i32 %l_800,  12
    %l__400___200000 = shl i32 %l_400,  11
    %l__200____80000 = shl i32 %l_200,  10
    %l__100____20000 = shl i32 %l_100,  9
    %l___80_____8000 = shl i32 %l_80,   8
    %l___40_____2000 = shl i32 %l_40,   7
    %l___20______800 = shl i32 %l_20,   6
    %l___10______200 = shl i32 %l_10,   5
    %l____8_______80 = shl i32 %l_8,    4
    %l____4_______20 = shl i32 %l_4,    3
    %l____2________8 = shl i32 %l_2,    2
    %l____1________2 = shl i32 %l_1,    1

    %r_8000_40000000 = shl i32 %r_8000, 15
    %r_4000_10000000 = shl i32 %r_4000, 14
    %r_2000__4000000 = shl i32 %r_2000, 13
    %r_1000__1000000 = shl i32 %r_1000, 12
    %r__800___400000 = shl i32 %r_800,  11
    %r__400___100000 = shl i32 %r_400,  10
    %r__200____40000 = shl i32 %r_200,  9
    %r__100____10000 = shl i32 %r_100,  8
    %r___80_____4000 = shl i32 %r_80,   7
    %r___40_____1000 = shl i32 %r_40,   6
    %r___20______400 = shl i32 %r_20,   5
    %r___10______100 = shl i32 %r_10,   4
    %r____8_______40 = shl i32 %r_8,    3
    %r____4_______10 = shl i32 %r_4,    2
    %r____2________4 = shl i32 %r_2,    1
    %r____1________1 = shl i32 %r_1,    0

    %result_c0000000 = or i32 %l_8000_80000000, %r_8000_40000000
    %result_30000000 = or i32 %l_4000_20000000, %r_4000_10000000
    %result_0c000000 = or i32 %l_2000__8000000, %r_2000__4000000
    %result_03000000 = or i32 %l_1000__2000000, %r_1000__1000000
    %result_00c00000 = or i32 %l__800___800000, %r__800___400000
    %result_00300000 = or i32 %l__400___200000, %r__400___100000
    %result_000c0000 = or i32 %l__200____80000, %r__200____40000
    %result_00030000 = or i32 %l__100____20000, %r__100____10000
    %result_0000c000 = or i32 %l___80_____8000, %r___80_____4000
    %result_00003000 = or i32 %l___40_____2000, %r___40_____1000
    %result_00000c00 = or i32 %l___20______800, %r___20______400
    %result_00000300 = or i32 %l___10______200, %r___10______100
    %result_000000c0 = or i32 %l____8_______80, %r____8_______40
    %result_00000030 = or i32 %l____4_______20, %r____4_______10
    %result_0000000c = or i32 %l____2________8, %r____2________4
    %result_00000003 = or i32 %l____1________2, %r____1________1

    %result_f0000000 = or i32 %result_c0000000, %result_30000000
    %result_0f000000 = or i32 %result_0c000000, %result_03000000
    %result_00f00000 = or i32 %result_00c00000, %result_00300000
    %result_000f0000 = or i32 %result_000c0000, %result_00030000
    %result_0000f000 = or i32 %result_0000c000, %result_00003000
    %result_00000f00 = or i32 %result_00000c00, %result_00000300
    %result_000000f0 = or i32 %result_000000c0, %result_00000030
    %result_0000000f = or i32 %result_0000000c, %result_00000003

    %result_ff000000 = or i32 %result_f0000000, %result_0f000000
    %result_00ff0000 = or i32 %result_00f00000, %result_000f0000
    %result_0000ff00 = or i32 %result_0000f000, %result_00000f00
    %result_000000ff = or i32 %result_000000f0, %result_0000000f

    %result_ffff0000 = or i32 %result_ff000000, %result_00ff0000
    %result_0000ffff = or i32 %result_0000ff00, %result_000000ff

    %result_ffffffff = or i32 %result_ffff0000, %result_0000ffff
    %result = insertvalue %val insertvalue(%val zeroinitializer,i2 1,0), i32 %result_ffffffff, 1
    ret %val %result
}

define %val @op_select(%val %l, %val %r) {
    %ltag = extractvalue %val %l, 0
    %lval = extractvalue %val %l, 1
    %rtag = extractvalue %val %r, 0
    %rval = extractvalue %val %r, 1

    %liserror = icmp uge i2 %ltag, 2
    br i1 %liserror, label %ret_lerror, label %check_rerror

  ret_lerror:
    ret %val %l

  check_rerror:
    %riserror = icmp uge i2 %rtag, 2
    br i1 %riserror, label %ret_rerror, label %compute_result_loop

  ret_rerror:
    ret %val %r

  compute_result_loop:
    %mask = phi i32 [2147483648, %check_rerror], [%next_mask, %next_result_loop]
    %result = phi i32 [0, %check_rerror], [%next_result, %next_result_loop]
    %bitcount = phi i32 [0, %check_rerror], [%next_bitcount, %next_result_loop]
    %next_mask = lshr i32 %mask, 1
    %rbit = and i32 %mask, %rval
    %rbit0 = icmp eq i32 %rbit, 0
    br i1 %rbit0, label %next_result_loop, label %add_result_bit

  add_result_bit:
    %shift_result = shl i32 %result, 1
    %bitcount1 = add i32 %bitcount, 1
    %lbit = and i32 %mask, %lval
    %lbit0 = icmp eq i32 %lbit, 0
    br i1 %lbit0, label %next_result_loop, label %add_result_bit1

  add_result_bit1:
    %shift_result1 = or i32 %shift_result, 1
    br label %next_result_loop

  next_result_loop:
    %next_result = phi i32 [%result, %compute_result_loop], [%shift_result, %add_result_bit], [%shift_result1, %add_result_bit1]
    %next_bitcount = phi i32 [%bitcount, %compute_result_loop], [%bitcount1, %add_result_bit], [%bitcount1, %add_result_bit1]
    %done = icmp eq i32 %next_mask, 0
    br i1 %done, label %ret_result, label %compute_result_loop

  ret_result:
    %is16 = icmp ule i32 %next_bitcount, 16
    %res_tag = select i1 %is16, i2 0, i2 1
    %res1 = insertvalue %val zeroinitializer, i2 %res_tag, 0
    %res = insertvalue %val %res1, i32 %next_result, 1
    ret %val %res
}

define %val @op_and(%val %arg) {
    %tag = extractvalue %val %arg, 0
    %v = extractvalue %val %arg, 1

    %iserror = icmp uge i2 %tag, 2
    br i1 %iserror, label %ret_error, label %compute_result

  ret_error:
    ret %val %arg

  compute_result:
    %shiftval = lshr i32 %v, 1
    %is16 = icmp eq i2 %tag, 0
    %bit0 = and i32 %v, 1
    %shl_count = select i1 %is16, i32 15, i32 31
    %high_bit = shl i32 %bit0, %shl_count
    %rval = or i32 %shiftval, %high_bit
    %result_val = and i32 %v, %rval
    %result_tag = select i1 %is16, i2 0, i2 1
    %result1 = insertvalue %val zeroinitializer, i2 %result_tag, 0
    %result = insertvalue %val %result1, i32 %result_val, 1
    ret %val %result
}

define %val @op_or(%val %arg) {
    %tag = extractvalue %val %arg, 0
    %v = extractvalue %val %arg, 1

    %iserror = icmp uge i2 %tag, 2
    br i1 %iserror, label %ret_error, label %compute_result

  ret_error:
    ret %val %arg

  compute_result:
    %shiftval = lshr i32 %v, 1
    %is16 = icmp eq i2 %tag, 0
    %bit0 = and i32 %v, 1
    %shl_count = select i1 %is16, i32 15, i32 31
    %high_bit = shl i32 %bit0, %shl_count
    %rval = or i32 %shiftval, %high_bit
    %result_val = or i32 %v, %rval
    %result_tag = select i1 %is16, i2 0, i2 1
    %result1 = insertvalue %val zeroinitializer, i2 %result_tag, 0
    %result = insertvalue %val %result1, i32 %result_val, 1
    ret %val %result
}

define %val @op_xor(%val %arg) {
    %tag = extractvalue %val %arg, 0
    %v = extractvalue %val %arg, 1

    %iserror = icmp uge i2 %tag, 2
    br i1 %iserror, label %ret_error, label %compute_result

  ret_error:
    ret %val %arg

  compute_result:
    %shiftval = lshr i32 %v, 1
    %is16 = icmp eq i2 %tag, 0
    %bit0 = and i32 %v, 1
    %shl_count = select i1 %is16, i32 15, i32 31
    %high_bit = shl i32 %bit0, %shl_count
    %rval = or i32 %shiftval, %high_bit
    %result_val = xor i32 %v, %rval
    %result_tag = select i1 %is16, i2 0, i2 1
    %result1 = insertvalue %val zeroinitializer, i2 %result_tag, 0
    %result = insertvalue %val %result1, i32 %result_val, 1
    ret %val %result
}

define i1 @random_check(i32 %per1000) {
  entry:
    br label %loop

  loop:
    %count = phi i32 [0, %entry], [%next_count, %check_retry]
    %r = call i32 @random()
    %r1024 = and i32 %r, 1023
    %r1024_ge1000 = icmp uge i32 %r1024, 1000
    br i1 %r1024_ge1000, label %check_retry, label %ret_result

  check_retry:
    %next_count = add i32 %count, 1
    %next_count_gt1000 = icmp ugt i32 %next_count, 1000
    br i1 %next_count_gt1000, label %ret_result, label %loop

  ret_result:
    %result = icmp ult i32 %r1024, %per1000
    ret i1 %result
}

@error_message_000_prefix = private constant [8 x i8] c"ICL000I "
@error_message_on_the_way = private constant [26 x i8] c"\0A\09ON THE WAY TO STATEMENT "
@error_message_correct_source = private constant [30 x i8] c"\0A\09CORRECT SOURCE AND RESUBMIT\0A"

define void @finish_fatal_error(i8* %message, i32 %message_len, i8* %stmt_number, i32 %stmt_number_len) noreturn {
    %message_isnull = icmp eq i8* %message, null
    br i1 %message_isnull, label %end_message, label %start_message

  start_message:
    call i32 @write(i32 2, i8* %message, i32 %message_len)
    br label %end_message

  end_message:
    call i32 @write(i32 2, i8* getelementptr([26 x i8], [26 x i8]* @error_message_on_the_way, i32 0, i32 0), i32 26)
    call i32 @write(i32 2, i8* %stmt_number, i32 %stmt_number_len)
    call i32 @write(i32 2, i8* getelementptr([30 x i8], [30 x i8]* @error_message_correct_source, i32 0, i32 0), i32 30)
    call void @exit(i32 1) noreturn
    ret void
}

define i1 @retrieve_var(%vrbl* %var) {
    %valptr = getelementptr %vrbl,%vrbl* %var,i32 0,i32 1
    %val = load %vrbl_val*,%vrbl_val** %valptr
    %valnextptr = getelementptr %vrbl_val,%vrbl_val* %val,i32 0,i32 0
    %valnext = load %vrbl_val*,%vrbl_val** %valnextptr
    %valnext_isnull = icmp eq %vrbl_val* %valnext, null
    br i1 %valnext_isnull,label %retfail,label %dopop

  dopop:
    store %vrbl_val* %valnext,%vrbl_val** %valptr
    %val_addr = bitcast %vrbl_val* %val to i8*
    call void @free(i8* %val_addr)
    ret i1 true

  retfail:
    ret i1 false
}

define i1 @retrieve_arr(%arr_vrbl* %arr) {
    %valptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %val = load %arr_val*,%arr_val** %valptr
    %val_isnull = icmp eq %arr_val* %val, null
    br i1 %val_isnull,label %retfail,label %checkval

  checkval:
    %valnextptr = getelementptr %arr_val,%arr_val* %val,i32 0,i32 0
    %valnext = load %arr_val*,%arr_val** %valnextptr
    %valnext_isnull = icmp eq %arr_val* %valnext, null
    br i1 %valnext_isnull,label %retfail,label %dopop

  dopop:
    store %arr_val* %valnext,%arr_val** %valptr
    %val_addr = bitcast %arr_val* %val to i8*
    call void @free(i8* %val_addr)
    ret i1 true

  retfail:
    ret i1 false
}

define void @lazy_stash_arr(%arr_vrbl* %arr) {
    %arr_val_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %arr_val = load %arr_val*,%arr_val** %arr_val_ptr
    %uninitialized = icmp eq %arr_val* %arr_val,null
    br i1 %uninitialized,label %stash_uninitialized,label %incr_stash_count

  stash_uninitialized:
    call void @stash_arr(%arr_vrbl* %arr)
    ret void

  incr_stash_count:
    %stash_count_ptr = getelementptr %arr_val,%arr_val* %arr_val,i32 0,i32 1
    %stash_count = load i32,i32* %stash_count_ptr
    %new_stash_count = add i32 %stash_count,1
    store i32 %new_stash_count,i32* %stash_count_ptr
    ret void
}

define i1 @lazy_retrieve_arr(%arr_vrbl* %arr) {
    %arr_val_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %arr_val = load %arr_val*,%arr_val** %arr_val_ptr
    %uninitialized = icmp eq %arr_val* %arr_val,null
    br i1 %uninitialized,label %ret_fail,label %check_stash_count

  check_stash_count:
    %stash_count_ptr = getelementptr %arr_val,%arr_val* %arr_val,i32 0,i32 1
    %stash_count = load i32,i32* %stash_count_ptr
    %stash_count_is_zero = icmp eq i32 %stash_count,0
    br i1 %stash_count_is_zero,label %dopop,label %decr_stash_count

  dopop:
    %result = call i1 @retrieve_arr(%arr_vrbl* %arr)
    ret i1 %result

  decr_stash_count:
    %new_stash_count = add i32 %stash_count,1
    store i32 %new_stash_count,i32* %stash_count_ptr
    ret i1 true

  ret_fail:
    ret i1 false
}

; if do_copy is true, this is for assigning an element
;     if the %arr_val* is null, return null
;     otherwise, if stash count is zero, return the %arr_val*
;     otherwise, decrement stash count, call @stash_arr,
;                set stash count to zero in new copy of %arr_val*
;                return the new copy of %arr_val*
; if do_copy is false, this is for redimensioning the array
;     if the %arr_val* is null, return null
;     otherwise, if stash count is zero, free the %arr_val*,
;                return the next %arr_val*
;     otherwise, decrement stash count, return the %arr_val*
define %arr_val* @lazy_stash_arr_cow(%arr_vrbl* %arr,i1 %do_copy) {
    %arr_val_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %arr_val = load %arr_val*,%arr_val** %arr_val_ptr
    %uninitialized = icmp eq %arr_val* %arr_val,null
    br i1 %uninitialized,label %ret_null,label %check_stash_count

  ret_null:
    ret %arr_val* null

  check_stash_count:
    %stash_count_ptr = getelementptr %arr_val,%arr_val* %arr_val,i32 0,i32 1
    %stash_count = load i32,i32* %stash_count_ptr
    %stash_count_is_zero = icmp eq i32 %stash_count,0
    br i1 %stash_count_is_zero,label %no_cow,label %yes_cow

  no_cow:
    br i1 %do_copy,label %no_cow_for_assign,label %no_cow_for_redim

  no_cow_for_assign:
    ret %arr_val* %arr_val

  no_cow_for_redim:
    %next_arr_val_ptr = getelementptr %arr_val,%arr_val* %arr_val,i32 0,i32 0
    %next_arr_val = load %arr_val*,%arr_val** %next_arr_val_ptr
    %arr_val_addr = bitcast %arr_val* %arr_val to i8*
    call void @free(i8* %arr_val_addr)
    ret %arr_val* %next_arr_val

  yes_cow:
    %new_stash_count = sub i32 %stash_count,1
    store i32 %new_stash_count,i32* %stash_count_ptr
    br i1 %do_copy,label %yes_cow_for_assign,label %yes_cow_for_redim

  yes_cow_for_assign:
    call void @stash_arr(%arr_vrbl* %arr)
    %new_arr_val = load %arr_val*,%arr_val** %arr_val_ptr
    %new_stash_count_ptr = getelementptr %arr_val,%arr_val* %new_arr_val,i32 0,i32 1
    store i32 0,i32* %new_stash_count_ptr
    ret %arr_val* %new_arr_val

  yes_cow_for_redim:
    ret %arr_val* %arr_val    
}

define {i32,i32,i1} @arr_writein_range(%arr_vrbl* %arr) {
    %arr_val_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %arr_val = load %arr_val*,%arr_val** %arr_val_ptr
    %arr_valisnull = icmp eq %arr_val* %arr_val,null
    br i1 %arr_valisnull,label %ret_fail,label %calc_range

  ret_fail:
    ret {i32,i32,i1} zeroinitializer

  calc_range:
    %dims_ptr = getelementptr %arr_val,%arr_val* %arr_val,i32 0,i32 2
    %dims = load i32,i32* %dims_ptr
    %dims_iszero = icmp eq i32 %dims,0
    br i1 %dims_iszero,label %ret_fail,label %loop

  loop:
    %current_index = phi i32 [0,%calc_range],[%next_index,%next_loop]
    %current_size = phi i32 [1,%calc_range],[%next_size,%next_loop]
    %last_index_done = icmp uge i32 %current_index,%dims
    br i1 %last_index_done,label %ret_result,label %next_loop

  next_loop:
    %next_index = add i32 %current_index,1
    %current_dim_ptr = getelementptr %arr_val,%arr_val* %arr_val,i32 0,i32 3,i32 %current_index
    %current_dim = load i32,i32* %current_dim_ptr
    %next_size = mul i32 %current_size,%current_dim
    br label %loop

  ret_result:
    %start_index = select i1 1,i32 %dims,i32 %dims
    %end_index = add i32 %start_index,%current_size
    %result1 = insertvalue {i32,i32,i1} insertvalue({i32,i32,i1} zeroinitializer,i1 1,2), i32 %start_index, 0
    %result = insertvalue {i32,i32,i1} %result1, i32 %end_index, 1
    ret {i32,i32,i1} %result
}

define %val @writein16(%arr_vrbl* %arr) {
    %range = call {i32,i32,i1} @arr_writein_range(%arr_vrbl* %arr)
    %rangeok = extractvalue {i32,i32,i1} %range,2
    br i1 %rangeok,label %dowritein,label %ret_fail

  ret_fail:
    ret %val insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 241,1)

  dowritein:
    %arrval_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %arrval = load %arr_val*,%arr_val** %arrval_ptr
    %start_index = extractvalue {i32,i32,i1} %range,0
    %end_index = extractvalue {i32,i32,i1} %range,1
    br label %loop

  loop:
    %index = phi i32 [%start_index,%dowritein],[%next_index,%next_loop]
    %index_ok = icmp ult i32 %index,%end_index
    br i1 %index_ok,label %input,label %done

  input:
    %input_val = call %val @input16()
    %input_tag = extractvalue %val %input_val,0
    %input_iserr = icmp uge i2 %input_tag,2
    br i1 %input_iserr,label %input_err,label %next_loop

  next_loop:
    %next_index = add i32 %index,1
    %elem_ptr = getelementptr %arr_val,%arr_val* %arrval,i32 0,i32 3,i32 %index
    %new_elem = extractvalue %val %input_val, 1
    store i32 %new_elem,i32* %elem_ptr
    br label %loop

  input_err:
    ret %val %input_val

  done:
    ret %val zeroinitializer
}

define %val @writein32(%arr_vrbl* %arr) {
    %range = call {i32,i32,i1} @arr_writein_range(%arr_vrbl* %arr)
    %rangeok = extractvalue {i32,i32,i1} %range,2
    br i1 %rangeok,label %dowritein,label %ret_fail

  ret_fail:
    ret %val insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 241,1)

  dowritein:
    %arrval_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %arrval = load %arr_val*,%arr_val** %arrval_ptr
    %start_index = extractvalue {i32,i32,i1} %range,0
    %end_index = extractvalue {i32,i32,i1} %range,1
    br label %loop

  loop:
    %index = phi i32 [%start_index,%dowritein],[%next_index,%next_loop]
    %index_ok = icmp ult i32 %index,%end_index
    br i1 %index_ok,label %input,label %done

  input:
    %input_val = call %val @input32()
    %input_tag = extractvalue %val %input_val,0
    %input_iserr = icmp uge i2 %input_tag,2
    br i1 %input_iserr,label %input_err,label %next_loop

  next_loop:
    %next_index = add i32 %index,1
    %elem_ptr = getelementptr %arr_val,%arr_val* %arrval,i32 0,i32 3,i32 %index
    %new_elem = extractvalue %val %input_val, 1
    store i32 %new_elem,i32* %elem_ptr
    br label %loop

  input_err:
    ret %val %input_val

  done:
    ret %val zeroinitializer
}

@binary_output_index = global i8 1
@binary_output_buffer = global i8 0

define void @output_binary(i1 %bit) {
    %current_index = load i8,i8* @binary_output_index
    br i1 %bit,label %set_bit,label %advance_index

  set_bit:
    %current_buffer = load i8,i8* @binary_output_buffer
    %new_buffer = or i8 %current_buffer,%current_index
    store i8 %new_buffer,i8* @binary_output_buffer
    br label %advance_index

  advance_index:
    %new_index = shl i8 %current_index,1
    %flush_buffer_check = icmp eq i8 %new_index,0
    br i1 %flush_buffer_check,label %flush_buffer,label %store_new_index

  flush_buffer:
    call i32 @write(i32 1,i8* @binary_output_buffer,i32 1)
    store i8 0,i8* @binary_output_buffer
    br label %store_new_index

  store_new_index:
    %next_index = phi i8 [%new_index,%advance_index],[1,%flush_buffer]
    store i8 %next_index,i8* @binary_output_index
    ret void
}

define void @output_binary_array(%arr_vrbl* %arr, i32 %bitcount) {
    ; future optimization: alloca a 16 bit buffer, run through %arr
    ; looking for 16 bits at a time, then write out the up to 16 bits,
    ; iterate until all the %bitcount bits are written

  entry:
    %range = call {i32,i32,i1} @arr_writein_range(%arr_vrbl* %arr)
    %rangeok = extractvalue {i32,i32,i1} %range,2
    br i1 %rangeok,label %do_write_out,label %do_write_null_loop

  do_write_null_loop:
    %bit_index_null = phi i32 [0,%entry],[%next_bit_index_null,%do_write_null]
    %bit_index_null_check = icmp ult i32 %bit_index_null,%bitcount
    br i1 %bit_index_null_check,label %do_write_null,label %done

  do_write_null:
    call void @output_binary(i1 0)
    %next_bit_index_null = add i32 %bit_index_null,1
    br label %do_write_null_loop

  do_write_out:
    %arrval_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %arrval = load %arr_val*,%arr_val** %arrval_ptr
    %start_index = extractvalue {i32,i32,i1} %range,0
    %end_index = extractvalue {i32,i32,i1} %range,1
    br label %do_write_out_loop

  do_write_out_loop:
    %bit_index = phi i32 [1,%do_write_out],[%next_bit_index,%do_write_out_loop_end]
    %bit_index_check = icmp ule i32 %bit_index,%bitcount
    br i1 %bit_index_check,label %do_write_out_loop_body,label %done

  do_write_out_loop_body:
    %index = phi i32 [%start_index,%do_write_out_loop],[%next_index,%do_write_out_loop_body_end]
    %next_index = add i32 %index,1
    %index_ok = icmp ult i32 %index,%end_index
    br i1 %index_ok,label %do_write_out_loop_body_end,label %do_write_out_loop_end

  do_write_out_loop_body_end:
    %elem_ptr = getelementptr %arr_val,%arr_val* %arrval,i32 0,i32 3,i32 %index
    %elem = load i32,i32* %elem_ptr
    %elem_test = icmp eq i32 %elem,%bit_index
    br i1 %elem_test,label %do_write_out_loop_end,label %do_write_out_loop_body

  do_write_out_loop_end:
    %bit = phi i1 [0,%do_write_out_loop_body],[1,%do_write_out_loop_body_end]
    call void @output_binary(i1 %bit)
    %next_bit_index = add i32 %bit_index,1
    br label %do_write_out_loop

  done:
    ret void
}

@binary_input_index = global i8 0
@binary_input_buffer = global i8 0
@binary_input_eof = global i1 0

; {bit,eof/error flag}
define {i1,i1} @input_binary() {
    %eof = load i1,i1* @binary_input_eof
    br i1 %eof,label %ret_eof,label %read

  read:
    %current_index = load i8,i8* @binary_input_index
    %next_byte_check = icmp eq i8 %current_index,0
    br i1 %next_byte_check,label %read_next_byte,label %advance_index

  read_next_byte:
    store i8 2,i8* @binary_input_index
    %read_result = call i32 @read(i32 0, i8* @binary_input_buffer, i32 1)
    %read_success = icmp eq i32 %read_result,1
    br i1 %read_success,label %ret_bit,label %ret_error

  advance_index:
    %next_index = shl i8 %current_index,1
    store i8 %next_index,i8* @binary_input_index
    br label %ret_bit

  ret_bit:
    %index = phi i8 [%current_index,%advance_index],[1,%read_next_byte]
    %buffer_byte = load i8,i8* @binary_input_buffer
    %buffer_bit = and i8 %buffer_byte,%index
    %result_bit = icmp ne i8 %buffer_bit,0
    %result = insertvalue {i1,i1} zeroinitializer,i1 %result_bit,0
    ret {i1,i1} %result

  ret_error:
    store i1 1,i1* @binary_input_eof
    ret {i1,i1} insertvalue({i1,i1} zeroinitializer,i1 1,1)

  ret_eof:
    ret {i1,i1} insertvalue({i1,i1} zeroinitializer,i1 1,1)
}

; {bit,bit valid flag}, bit is valid if it is in the buffer
define {i1,i1} @peek_input_binary() {
    %eof = load i1,i1* @binary_input_eof
    br i1 %eof,label %ret_invalid,label %peek

  peek:
    %current_index = load i8,i8* @binary_input_index
    %buffer_empty_check = icmp eq i8 %current_index,0
    br i1 %buffer_empty_check,label %ret_invalid,label %ret_peek

  ret_invalid:
    ret {i1,i1} zeroinitializer

  ret_peek:
    %buffer_byte = load i8,i8* @binary_input_buffer
    %buffer_bit = and i8 %buffer_byte,%current_index
    %result_bit = icmp ne i8 %buffer_bit,0
    %result = insertvalue {i1,i1} insertvalue({i1,i1} zeroinitializer,i1 1,1),i1 %result_bit,0
    ret {i1,i1} %result
}

define %val @input_binary_array(%arr_vrbl* %arr) {
    %range = call {i32,i32,i1} @arr_writein_range(%arr_vrbl* %arr)
    %rangeok = extractvalue {i32,i32,i1} %range,2
    br i1 %rangeok,label %init_loop,label %ret_err241

  ret_err241:
    ret %val insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 241,1)

  init_loop:
    %arrval_ptr = getelementptr %arr_vrbl,%arr_vrbl* %arr,i32 0,i32 1
    %arrval = load %arr_val*,%arr_val** %arrval_ptr
    %start_index = extractvalue {i32,i32,i1} %range,0
    %end_index = extractvalue {i32,i32,i1} %range,1
    %last_index = sub i32 %end_index,1
    br label %index_loop

  index_loop:
    %index = phi i32 [%start_index,%init_loop],[%next_index,%store_count]
    %eof = phi i1 [0,%init_loop],[%next_eof,%store_count]
    %bit = phi i1 [0,%init_loop],[%next_bit,%store_count]
    %count = phi i32 [0,%init_loop],[%next_start_count,%store_count]
    %next_index = add i32 %index,1
    %next_bit = xor i1 %bit,1
    %index_check = icmp ult i32 %index,%last_index
    br i1 %index_check,label %check_eof_index,label %do_last_index

  check_eof_index:
    br i1 %eof,label %store_count,label %bit_loop

  bit_loop:
    %bit_count = phi i32 [%count,%check_eof_index],[%inc_bit_count,%check_bit]
    %count_overflow = icmp uge i32 %bit_count,65535
    br i1 %count_overflow,label %store_count,label %read_bit

  read_bit:
    %input = call {i1,i1} @input_binary()
    %in_bit = extractvalue {i1,i1} %input,0
    %in_eof = extractvalue {i1,i1} %input,1
    br i1 %in_eof,label %store_count,label %check_bit

  check_bit:
    %inc_bit_count = add i32 %bit_count,1
    %bit_check = icmp eq i1 %in_bit,%bit
    br i1 %bit_check,label %bit_loop,label %store_count

  store_count:
    %count_to_store = phi i32 [0,%check_eof_index],[%bit_count,%bit_loop],[%bit_count,%read_bit],[%bit_count,%check_bit]
    %next_start_count = phi i32 [0,%check_eof_index],[0,%bit_loop],[0,%read_bit],[1,%check_bit]
    %next_eof = phi i1 [1,%check_eof_index],[0,%bit_loop],[1,%read_bit],[0,%check_bit]
    %elem_ptr = getelementptr %arr_val,%arr_val* %arrval,i32 0,i32 3,i32 %index
    store i32 %count_to_store,i32* %elem_ptr
    br label %index_loop

    ret %val insertvalue(%val insertvalue(%val zeroinitializer,i2 2,0),i32 778,1)

  do_last_index:
    br i1 %eof,label %store_last_index,label %last_index_loop

  last_index_loop:
    %last_bit_count = phi i32 [%count,%do_last_index],[%inc_last_bit_count,%last_bit_continues]
    %peek = call {i1,i1} @peek_input_binary()
    %peek_bit = extractvalue {i1,i1} %peek,0
    %peek_valid = extractvalue {i1,i1} %peek,1
    br i1 %peek_valid,label %last_check_bit,label %store_last_index

  last_check_bit:
    %last_bit_check = icmp eq i1 %peek_bit,%bit
    br i1 %last_bit_check,label %last_bit_continues,label %store_last_index

  last_bit_continues:
    %inc_last_bit_count = add i32 %last_bit_count,1
    call {i1,i1} @input_binary()
    br label %last_index_loop

  store_last_index:
    %last_bit_count_to_store = phi i32 [0,%do_last_index],[%last_bit_count,%last_index_loop],[%last_bit_count,%last_check_bit]
    %last_elem_ptr = getelementptr %arr_val,%arr_val* %arrval,i32 0,i32 3,i32 %index
    store i32 %last_bit_count_to_store,i32* %last_elem_ptr
    ret %val zeroinitializer
}

define i32 @lib1900() {
    %r = call i32 @random()
    %r0 = and i32 %r,65535
    %r0_check = icmp eq i32 %r0,0
    br i1 %r0_check,label %retry1,label %ret0
  ret0:
    ret i32 %r0

  retry1:
    %r1_shift = lshr i32 %r,1
    %r1 = and i32 %r1_shift,65535
    %r1_check = icmp eq i32 %r1,0
    br i1 %r1_check,label %retry2,label %ret1
  ret1:
    ret i32 %r1

  retry2:
    %r2_shift = lshr i32 %r,2
    %r2 = and i32 %r2_shift,65535
    %r2_check = icmp eq i32 %r2,0
    br i1 %r2_check,label %retry3,label %ret2
  ret2:
    ret i32 %r2

  retry3:
    %r3_shift = lshr i32 %r,3
    %r3 = and i32 %r3_shift,65535
    %r3_check = icmp eq i32 %r3,0
    br i1 %r3_check,label %retry4,label %ret3
  ret3:
    ret i32 %r3

  retry4:
    %r4_shift = lshr i32 %r,4
    %r4 = and i32 %r4_shift,65535
    %r4_check = icmp eq i32 %r4,0
    br i1 %r4_check,label %retry5,label %ret4
  ret4:
    ret i32 %r4

  retry5:
    %r5_shift = lshr i32 %r,5
    %r5 = and i32 %r5_shift,65535
    %r5_check = icmp eq i32 %r5,0
    br i1 %r5_check,label %retry6,label %ret5
  ret5:
    ret i32 %r5

  retry6:
    %r6_shift = lshr i32 %r,6
    %r6 = and i32 %r6_shift,65535
    %r6_check = icmp eq i32 %r6,0
    br i1 %r6_check,label %retry7,label %ret6
  ret6:
    ret i32 %r6

  retry7:
    %r7_shift = lshr i32 %r,7
    %r7 = and i32 %r7_shift,65535
    %r7_check = icmp eq i32 %r7,0
    br i1 %r7_check,label %retry8,label %ret7
  ret7:
    ret i32 %r7

  retry8:
    %r8_shift = lshr i32 %r,1
    %r8 = and i32 %r1_shift,65535
    %r8_check = icmp eq i32 %r8,0
    br i1 %r8_check,label %retry9,label %ret8
  ret8:
    ret i32 %r8

  retry9:
    %r9_shift = lshr i32 %r,9
    %r9 = and i32 %r9_shift,65535
    %r9_check = icmp eq i32 %r9,0
    br i1 %r9_check,label %retry10,label %ret9
  ret9:
    ret i32 %r9

  retry10:
    %r10_shift = lshr i32 %r,10
    %r10 = and i32 %r10_shift,65535
    %r10_check = icmp eq i32 %r10,0
    br i1 %r10_check,label %retry11,label %ret10
  ret10:
    ret i32 %r10

  retry11:
    %r11_shift = lshr i32 %r,11
    %r11 = and i32 %r11_shift,65535
    %r11_check = icmp eq i32 %r11,0
    br i1 %r11_check,label %retry12,label %ret11
  ret11:
    ret i32 %r11

  retry12:
    %r12_shift = lshr i32 %r,12
    %r12 = and i32 %r12_shift,65535
    %r12_check = icmp eq i32 %r12,0
    br i1 %r12_check,label %retry13,label %ret12
  ret12:
    ret i32 %r12

  retry13:
    %r13_shift = lshr i32 %r,13
    %r13 = and i32 %r13_shift,65535
    %r13_check = icmp eq i32 %r13,0
    br i1 %r13_check,label %retry14,label %ret13
  ret13:
    ret i32 %r13

  retry14:
    %r14_shift = lshr i32 %r,14
    %r14 = and i32 %r14_shift,65535
    %r14_check = icmp eq i32 %r14,0
    br i1 %r14_check,label %retry15,label %ret14
  ret14:
    ret i32 %r14

  retry15:
    %r15_shift = lshr i32 %r,15
    %r15 = and i32 %r15_shift,65535
    %r15_check = icmp eq i32 %r15,0
    br i1 %r15_check,label %retry16,label %ret15
  ret15:
    ret i32 %r15

  retry16:
    %r16_shift = lshr i32 %r,16
    %r16 = and i32 %r16_shift,65535
    %r16_check = icmp eq i32 %r16,0
    br i1 %r16_check,label %giveup,label %ret16
  ret16:
    ret i32 %r16

  giveup:
    ret i32 1
}

define i32 @lib1910(i32 %arg) {
    %arg_too_small = icmp ule i32 %arg,1
    br i1 %arg_too_small,label %ret0,label %dorand

  ret0:
    ret i32 0

  dorand:
    %divisor = udiv i32 2147483648,%arg
    %r0 = call i32 @random()
    %s0 = udiv i32 %r0,%divisor
    %r1 = call i32 @random()
    %a1 = udiv i32 %r1,%divisor
    %s1 = add i32 %a1,%s0
    %r2 = call i32 @random()
    %a2 = udiv i32 %r2,%divisor
    %s2 = add i32 %a2,%s1
    %r3 = call i32 @random()
    %a3 = udiv i32 %r3,%divisor
    %s3 = add i32 %a3,%s2
    %r4 = call i32 @random()
    %a4 = udiv i32 %r4,%divisor
    %s4 = add i32 %a4,%s3
    %r5 = call i32 @random()
    %a5 = udiv i32 %r5,%divisor
    %s5 = add i32 %a5,%s4
    %r6 = call i32 @random()
    %a6 = udiv i32 %r6,%divisor
    %s6 = add i32 %a6,%s5
    %r7 = call i32 @random()
    %a7 = udiv i32 %r7,%divisor
    %s7 = add i32 %a7,%s6
    %r8 = call i32 @random()
    %a8 = udiv i32 %r8,%divisor
    %s8 = add i32 %a8,%s7
    %r9 = call i32 @random()
    %a9 = udiv i32 %r9,%divisor
    %s9 = add i32 %a9,%s8
    %r10 = call i32 @random()
    %a10 = udiv i32 %r10,%divisor
    %s10 = add i32 %a10,%s9
    %r11 = call i32 @random()
    %a11 = udiv i32 %r11,%divisor
    %s11 = add i32 %a11,%s10
    %r = udiv i32 %s11,12
    ret i32 %r
}
