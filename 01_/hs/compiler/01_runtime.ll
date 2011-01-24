; Runtime library for 01_

@.alloc_count = global i32 0

declare i8* @malloc(i32)
declare void @free(i8*)

define fastcc void @.free(i8* %ptr) {
    %alloc_count_old = load i32* @.alloc_count
    %alloc_count_new = sub i32 %alloc_count_old, 1
    store i32 %alloc_count_new, i32* @.alloc_count
    call void @free(i8* %ptr)
    ret void
}

define fastcc i8* @.alloc(i32 %size) {
    %alloc_count_old = load i32* @.alloc_count
    %alloc_count_new = add i32 %alloc_count_old, 1
    store i32 %alloc_count_new, i32* @.alloc_count
    %ptr = call i8* @malloc(i32 %size)
    ret i8* %ptr
}

; struct val {
;    int refcount;
;    bool bit;
;    struct val *next;
;    struct { bool bit, struct val *next } (void *) *eval;
;    void (void *) *freeenv;
;    void *env;
; }
%.val = type { i32, i1, %.val*, { i1, %.val* } (i8*)*, void (i8*)*, i8* }

@.nil = linkonce_odr global %.val { i32 1, i1 undef, %.val* null, { i1, %.val* } (i8*)* null, void (i8*)* undef, i8* undef }

define fastcc %.val* @.addref(%.val* %val) {
    %_refcount = getelementptr %.val* %val, i32 0, i32 0
    %refcount_old = load i32* %_refcount
    %refcount_new = add i32 %refcount_old, 1
    store i32 %refcount_new, i32* %_refcount
    ret %.val* %val
}

define fastcc void @.deref(%.val* %val) {
    ; decrement reference count
    %_refcount = getelementptr %.val* %val, i32 0, i32 0
    %refcount_old = load i32* %_refcount
    %refcount_new = sub i32 %refcount_old, 1
    store i32 %refcount_new, i32* %_refcount
    %refcount_positive = icmp sgt i32 %refcount_new, 0
    br i1 %refcount_positive, label %alive, label %dead
  alive:
    ret void
  dead:
    tail call fastcc void @.freeval(%.val* %val)
    ret void
}

define fastcc void @.freeval(%.val* %val) {
    %_eval = getelementptr %.val* %val, i32 0, i32 3
    %eval = load { i1, %.val* } (i8*)** %_eval
    %eval_null = icmp eq { i1, %.val* } (i8*)* %eval, null
    br i1 %eval_null, label %evaluated, label %unevaluated
  evaluated:
    ; dereference cdr
    %_next = getelementptr %.val* %val, i32 0, i32 2
    %next = load %.val** %_next
    %val_for_free_1 = bitcast %.val* %val to i8*
    call fastcc void @.free(i8* %val_for_free_1)
    %next_is_nil = icmp eq %.val* %next, null
    br i1 %next_is_nil, label %done, label %deref_next
  done:
    ret void
  deref_next:
    tail call fastcc void @.deref(%.val* %next)
    ret void
  unevaluated:
    ; free promise
    %_freeenv = getelementptr %.val* %val, i32 0, i32 4
    %freeenv = load void (i8*)** %_freeenv
    %_env = getelementptr %.val* %val, i32 0, i32 5
    %env = load i8** %_env
    %val_for_free_2 = bitcast %.val* %val to i8*
    call fastcc void @.free(i8* %val_for_free_2)
    tail call fastcc void %freeenv(i8* %env)
    ret void
}

define fastcc %.val* @.newval({i1, %.val*} (i8*)* %eval, void (i8*)* %freeenv, i8* %env) {
    ; http://nondot.org/sabre/LLVMNotes/SizeOf-OffsetOf-VariableSizedStructs.txt
    %val_size = ptrtoint %.val* getelementptr (%.val* null, i32 1) to i32
    %val_from_alloc = call fastcc i8* @.alloc(i32 %val_size)
    %val = bitcast i8* %val_from_alloc to %.val*
    %_refcount = getelementptr %.val* %val, i32 0, i32 0
    store i32 1, i32* %_refcount
    %_eval = getelementptr %.val* %val, i32 0, i32 3
    store { i1, %.val* } (i8*)* %eval, { i1, %.val* } (i8*)** %_eval
    %_freeenv = getelementptr %.val* %val, i32 0, i32 4
    store void (i8*)* %freeenv, void (i8*)** %_freeenv
    %_env = getelementptr %.val* %val, i32 0, i32 5
    store i8* %env, i8** %_env
    ret %.val* %val
}

define fastcc { i1, %.val* } @.eval(%.val* %val) {
    %_eval = getelementptr %.val* %val, i32 0, i32 3
    %eval = load { i1, %.val* } (i8*)** %_eval
    %eval_null = icmp eq { i1, %.val* } (i8*)* %eval, null
    %_bit = getelementptr %.val* %val, i32 0, i32 1
    %_next = getelementptr %.val* %val, i32 0, i32 2
    br i1 %eval_null, label %evaluated, label %unevaluated
  evaluated:
    %bit = load i1* %_bit
    %result_1 = insertvalue { i1, %.val* } undef, i1 %bit, 0
    %next = load %.val** %_next
    %result_2 = insertvalue { i1, %.val* } %result_1, %.val* %next, 1
    ret { i1, %.val* } %result_2
  unevaluated:
    %_env = getelementptr %.val* %val, i32 0, i32 5
    %env = load i8** %_env
    %result = call fastcc { i1, %.val* } %eval(i8* %env)
    %_freeenv = getelementptr %.val* %val, i32 0, i32 4
    %freeenv = load void (i8*)** %_freeenv
    call fastcc void %freeenv(i8* %env)
    %new_bit = extractvalue { i1, %.val* } %result, 0
    store i1 %new_bit, i1* %_bit
    %new_next = extractvalue { i1, %.val* } %result, 1
    store %.val* %new_next, %.val** %_next
    store { i1, %.val* } (i8*)* null, { i1, %.val* } (i8*)** %_eval
    ret { i1, %.val* } %result
}

; literal values

; struct literalenv {
;     bool *bits;
;     int length;
;     int index;
; }
%.literalenv = type { [0 x i1]*, i32, i32 }

define fastcc %.val* @.literalval([0 x i1]* %bits, i32 %length, i32 %index) {
    %nil = icmp uge i32 %index, %length
    br i1 %nil, label %is_nil, label %not_nil
  is_nil:
    %nil_result = tail call fastcc %.val* @.addref(%.val* getelementptr (%.val* @.nil))
    ret %.val* %nil_result
  not_nil:
    %literalenv_size = ptrtoint %.literalenv* getelementptr (%.literalenv* null, i32 1) to i32
    %literalenv_from_alloc = call fastcc i8* @.alloc(i32 %literalenv_size)
    %literalenv = bitcast i8* %literalenv_from_alloc to %.literalenv*
    %_bits = getelementptr %.literalenv* %literalenv, i32 0, i32 0
    store [0 x i1]* %bits, [0 x i1]** %_bits
    %_length = getelementptr %.literalenv* %literalenv, i32 0, i32 1
    store i32 %length, i32* %_length
    %_index = getelementptr %.literalenv* %literalenv, i32 0, i32 2
    store i32 %index, i32* %_index
    %eval = bitcast { i1, %.val* } (%.literalenv*)* @.literalval.eval to { i1, %.val* } (i8*)*
    %freeenv = bitcast void (%.literalenv*)* @.literalval.freeenv to void (i8*)*
    %result = tail call fastcc %.val* @.newval({i1, %.val*} (i8*)* %eval, void (i8*)* %freeenv, i8* %literalenv_from_alloc)
    ret %.val* %result
}

define private fastcc { i1, %.val* } @.literalval.eval(%.literalenv* %env) {
    %_bits = getelementptr %.literalenv* %env, i32 0, i32 0
    %bits = load [0 x i1]** %_bits
    %_length = getelementptr %.literalenv* %env, i32 0, i32 1
    %length = load i32* %_length
    %_index = getelementptr %.literalenv* %env, i32 0, i32 2
    %index = load i32* %_index
    %_bit = getelementptr [0 x i1]* %bits, i32 0, i32 %index
    %bit = load i1* %_bit
    %next_index = add i32 %index, 1
    %next = call fastcc %.val* @.literalval([0 x i1]* %bits, i32 %length, i32 %next_index)
    %result_1 = insertvalue { i1, %.val* } undef, i1 %bit, 0
    %result_2 = insertvalue { i1, %.val* } %result_1, %.val* %next, 1
    ret { i1, %.val* } %result_2
}

define private fastcc void @.literalval.freeenv(%.literalenv* %env) {
    %env_for_free = bitcast %.literalenv* %env to i8*
    tail call fastcc void @.free(i8* %env_for_free)
    ret void
}

; files

declare i32 @open(i8*,i32)
declare i32 @read(i32,i8*,i32)
declare i32 @write(i32,i8*,i32)
declare i32 @close(i32)

; this can leak open files (files not completely read will never get closed)
; but that is not a concern for 01_

; struct fileenv {
;     int fd;
;     char byte;
;     char shift;
; }
%.fileenv = type { i32, i8, i8 }

define fastcc %.val* @.fileval(i32 %fd, i8 %byte, i8 %shift) {
    %fileenv_size = ptrtoint %.fileenv* getelementptr (%.fileenv* null, i32 1) to i32
    %fileenv_from_alloc = call fastcc i8* @.alloc(i32 %fileenv_size)
    %fileenv = bitcast i8* %fileenv_from_alloc to %.fileenv*
    %_fd = getelementptr %.fileenv* %fileenv, i32 0, i32 0
    store i32 %fd, i32* %_fd
    %_byte = getelementptr %.fileenv* %fileenv, i32 0, i32 1
    store i8 %byte, i8* %_byte
    %_shift = getelementptr %.fileenv* %fileenv, i32 0, i32 2
    store i8 %shift, i8* %_shift
    %eval = bitcast { i1, %.val* } (%.fileenv*)* @.fileval.eval to { i1, %.val* } (i8*)*
    %freeenv = bitcast void (%.fileenv*)* @.fileval.freeenv to void (i8*)*
    %result = tail call fastcc %.val* @.newval({ i1, %.val* } (i8*)* %eval, void (i8*)* %freeenv, i8* %fileenv_from_alloc)
    ret %.val* %result
}

define private fastcc { i1, %.val* } @.fileval.eval(%.fileenv* %env) {
    %_fd = getelementptr %.fileenv* %env, i32 0, i32 0
    %fd = load i32* %_fd
    %_shift = getelementptr %.fileenv* %env, i32 0, i32 2
    %shift = load i8* %_shift
    %need_to_read_new_byte = icmp eq i8 %shift, 7
    br i1 %need_to_read_new_byte, label %read_new_byte, label %use_old_byte
  read_new_byte:
    %result = tail call fastcc { i1, %.val* } @.fileval.eval.read(i32 %fd)
    ret { i1, %.val* } %result
  use_old_byte:
    %_byte = getelementptr %.fileenv* %env, i32 0, i32 1
    %byte = load i8* %_byte
    %shifted_byte = lshr i8 %byte, %shift
    %bit = trunc i8 %shifted_byte to i1
    %decremented_shift = sub i8 %shift, 1
    %finished_old_byte = icmp eq i8 %shift, 0
    %new_shift = select i1 %finished_old_byte, i8 7, i8 %decremented_shift
    %next = call fastcc %.val* @.fileval(i32 %fd, i8 %byte, i8 %new_shift)
    %result_1 = insertvalue { i1, %.val* } undef, i1 %bit, 0
    %result_2 = insertvalue { i1, %.val* } %result_1, %.val* %next, 1
    ret { i1, %.val* } %result_2
}

define private fastcc { i1, %.val* } @.fileval.eval.read(i32 %fd) {
    %_byte = alloca i8
    %count = call i32 @read(i32 %fd, i8* %_byte, i32 1)
    %count_is_one = icmp eq i32 %count, 1
    br i1 %count_is_one, label %read_success, label %read_fail
  read_success:
    %byte = load i8* %_byte
    %shifted_byte = lshr i8 %byte, 7
    %bit = trunc i8 %shifted_byte to i1
    %next = call fastcc %.val* @.fileval(i32 %fd, i8 %byte, i8 6)
    %result_1 = insertvalue { i1, %.val* } undef, i1 %bit, 0
    %result_2 = insertvalue { i1, %.val* } %result_1, %.val* %next, 1
    ret { i1, %.val* } %result_2
  read_fail:
    call i32 @close(i32 %fd)
    %nil_value = insertvalue { i1, %.val* } undef, %.val* null, 1
    ret { i1, %.val* } %nil_value
}

define private fastcc void @.fileval.freeenv(%.fileenv* %env) {
    %env_for_free = bitcast %.fileenv* %env to i8*
    tail call fastcc void @.free(i8* %env_for_free)
    ret void
}

; unopened files

define fastcc %.val* @.unopenedfileval(i8* %filename) {
    %result = tail call fastcc %.val* @.newval({i1, %.val*} (i8*)* @.unopenedfileval.eval, void (i8*)* @.unopenedfileval.freeenv, i8* %filename)
    ret %.val* %result
}

define private fastcc { i1, %.val* } @.unopenedfileval.eval(i8* %filename) {
    %fd = call i32 @open(i8* %filename, i32 0)
    %fd_valid = icmp sge i32 %fd, 0
    br i1 %fd_valid, label %open_success, label %open_fail
  open_success:
    %result = tail call fastcc { i1, %.val* } @.fileval.eval.read(i32 %fd)
    ret { i1, %.val* } %result
  open_fail:
    %nil_value = insertvalue { i1, %.val* } undef, %.val* null, 1
    ret { i1, %.val* } %nil_value
}

define private fastcc void @.unopenedfileval.freeenv(i8* %filename) {
    ; do nothing - %filename should come from argv
    ret void
}

; concat

; struct concatenv {
;     struct val *first;
;     struct val *second;
; }
%.concatenv = type { %.val*, %.val* }

define fastcc %.val* @.concatval(%.val* %first, %.val* %second) {
    call fastcc %.val* @.addref(%.val* %first)
    call fastcc %.val* @.addref(%.val* %second)
    %concatenv_size = ptrtoint %.concatenv* getelementptr (%.concatenv* null, i32 1) to i32
    %concatenv_from_alloc = call fastcc i8* @.alloc(i32 %concatenv_size)
    %concatenv = bitcast i8* %concatenv_from_alloc to %.concatenv*
    %_first = getelementptr %.concatenv* %concatenv, i32 0, i32 0
    store %.val* %first, %.val** %_first
    %_second = getelementptr %.concatenv* %concatenv, i32 0, i32 1
    store %.val* %second, %.val** %_second
    %eval = bitcast { i1, %.val* } (%.concatenv*)* @.concatval.eval to { i1, %.val* } (i8*)*
    %freeenv = bitcast void (%.concatenv*)* @.concatval.freeenv to void (i8*)*
    %result = tail call fastcc %.val* @.newval({ i1, %.val* } (i8*)* %eval, void (i8*)* %freeenv, i8* %concatenv_from_alloc)
    ret %.val* %result
}

define private fastcc { i1, %.val* } @.concatval.eval(%.concatenv* %env) {
    %_first = getelementptr %.concatenv* %env, i32 0, i32 0
    %first = load %.val** %_first
    %_second = getelementptr %.concatenv* %env, i32 0, i32 1
    %second = load %.val** %_second
    %eval_first = call fastcc { i1, %.val* } @.eval(%.val* %first)
    %first_next = extractvalue { i1, %.val* } %eval_first, 1
    %first_is_nil = icmp eq %.val* %first_next, null
    br i1 %first_is_nil, label %start_second, label %continue_first
  start_second:
    %eval_second = call fastcc { i1, %.val* } @.eval(%.val* %second)
    %second_next = extractvalue { i1, %.val* } %eval_second, 1
    %second_is_nil = icmp eq %.val* %second_next, null
    br i1 %second_is_nil, label %return_second, label %addref_to_second_next
  return_second:
    ret { i1, %.val* } %eval_second
  addref_to_second_next:
    call fastcc %.val* @.addref(%.val* %second_next)
    br label %return_second
  continue_first:
    %next = call fastcc %.val* @.concatval(%.val* %first_next, %.val* %second)
    %result = insertvalue { i1, %.val* } %eval_first, %.val* %next, 1
    ret { i1, %.val* } %result
}

define private fastcc void @.concatval.freeenv(%.concatenv* %env) {
    %_first = getelementptr %.concatenv* %env, i32 0, i32 0
    %first = load %.val** %_first
    call fastcc void @.deref(%.val* %first)
    %_second = getelementptr %.concatenv* %env, i32 0, i32 1
    %second = load %.val** %_second
    call fastcc void @.deref(%.val* %second)
    %env_for_free = bitcast %.concatenv* %env to i8*
    tail call fastcc void @.free(i8* %env_for_free)
    ret void
}

; output
declare void @putchar(i32)

define fastcc void @.print(%.val* %val) {
    %_v = alloca %.val*
    store %.val* %val, %.val** %_v
    %_shift = alloca i32
    %_byte = alloca i32
    br label %next_byte
  next_byte:
    store i32 7, i32* %_shift
    store i32 0, i32* %_byte
    br label %next_bit
  next_bit:
    %v = load %.val** %_v
    %eval = call fastcc { i1, %.val* } @.eval(%.val* %v)
    %next = extractvalue { i1, %.val* } %eval, 1
    %is_nil = icmp eq %.val* %next, null
    br i1 %is_nil, label %done, label %continue
  done:
    call fastcc void @.deref(%.val* %v)
    ret void
  continue:
    store %.val* %next, %.val** %_v
    call fastcc %.val* @.addref(%.val* %next)
    call fastcc void @.deref(%.val* %v)
    %bit = extractvalue { i1, %.val* } %eval, 0
    %bit1 = zext i1 %bit to i32
    %shift = load i32* %_shift
    %bit2 = shl i32 %bit1, %shift
    %byte = load i32* %_byte
    %new_byte = or i32 %byte, %bit2
    %finished_byte = icmp eq i32 %shift, 0
    br i1 %finished_byte, label %byte_done, label %continue_byte
  byte_done:
    call void @putchar(i32 %new_byte)
    br label %next_byte
  continue_byte:
    store i32 %new_byte, i32* %_byte
    %new_shift = sub i32 %shift, 1
    store i32 %new_shift, i32* %_shift
    br label %next_bit
}

; command line arguments

define fastcc %.val* @.getarg(i32 %index, %.val* %stdin, i32 %argc, i8** %argv) {
    %1 = icmp eq i32 %index, %argc
    br i1 %1, label %return_stdin, label %test_for_nil
  test_for_nil:
    %2 = icmp ugt i32 %index, %argc
    br i1 %2, label %return_nil, label %check_argv
  check_argv:
    %_arg = getelementptr i8** %argv, i32 %index
    %arg = load i8** %_arg
    %3 = icmp eq i8* %arg, null
    br i1 %3, label %return_nil, label %check_arg_1
  check_arg_1:
    %_c0 = getelementptr i8* %arg, i32 0
    %c0 = load i8* %_c0
    %4 = icmp eq i8 45, %c0 ; -
    br i1 %4, label %check_arg_2, label %return_fileval
  check_arg_2:
    %_c1 = getelementptr i8* %arg, i32 1
    %c1 = load i8* %_c1
    %5 = icmp eq i8 0, %c1 ; NUL
    br i1 %5, label %return_stdin, label %return_fileval
  return_fileval:
    %fileval_result = tail call fastcc %.val* @.unopenedfileval(i8* %arg)
    ret %.val* %fileval_result
  return_stdin:
    %stdin_result = tail call fastcc %.val* @.addref(%.val* %stdin)
    ret %.val* %stdin_result
  return_nil:
    %nil_result = tail call fastcc %.val* @.addref(%.val* @.nil)
    ret %.val* %nil_result
}

declare void @printf(i8*,...)

declare void @exit(i32) noreturn

@.match_failure_format = private constant [19 x i8] c"Match failure: %s\0A\00"

define fastcc void @.match_failure(i8* %function_name) noreturn {
    call void (i8*,...)* @printf(i8* getelementptr ([19 x i8]* @.match_failure_format, i32 0, i32 0), i8* %function_name)
    call void @exit(i32 1)
    unreachable
}

; debug memory

@.print_debug_memory_format = private constant [33 x i8] c"\0Aalloc_count=%d nil.refcount=%d\0A\00"

define fastcc void @.print_debug_memory() {
    %alloc_count = load i32* @.alloc_count
    %nil_refcount = load i32* getelementptr (%.val* @.nil, i32 0, i32 0)
    tail call void (i8*,...)* @printf(i8* getelementptr ([33 x i8]* @.print_debug_memory_format, i32 0, i32 0), i32 %alloc_count, i32 %nil_refcount)
    ret void
}
