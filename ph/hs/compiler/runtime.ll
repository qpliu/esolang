; Parenthesis Hell (minus eval) runtime library

; struct val {
;     int refcount;
;     struct val *car;
;     struct val *cdr;
;     struct eval (void *) *eval;
;     void (void *) *freeenv;
;     void *env;
; }
;
; struct eval {
;     struct val *car;
;     struct val *cdr;
; }
%val = type { i32, %val*, %val*, %eval (i8*)*, void (i8*)*, i8* }
%eval = type { %val*, %val* }

@C0 = global %val { i32 1, %val* null, %val* undef, %eval (i8*)* null, void (i8*)* undef, i8* undef }

declare i8* @malloc(i32)
declare void @free(i8*)

@"(alloc-count)" = global i32 0

define fastcc void @"(free)"(i8* %ptr) {
    %alloc_count_old = load i32* @"(alloc-count)"
    %alloc_count_new = sub i32 %alloc_count_old, 1
    store i32 %alloc_count_new, i32* @"(alloc-count)"
    call void @free(i8* %ptr)
    ret void
}

define fastcc i8* @"(alloc)"(i32 %size) {
    %alloc_count_old = load i32* @"(alloc-count)"
    %alloc_count_new = add i32 %alloc_count_old, 1
    store i32 %alloc_count_new, i32* @"(alloc-count)"
    %ptr = call i8* @malloc(i32 %size)
    ret i8* %ptr
}

declare void @printf(i8*,...)

@"(print-debug-memory-format)" = private constant [33 x i8] c"\0Aalloc_count=%d nil.refcount=%d\0A\00"

define fastcc void @"(print-debug-memory)"() {
    %alloc_count = load i32* @"(alloc-count)"
    %nil_refcount = load i32* getelementptr (%val* @C0, i32 0, i32 0)
    tail call void (i8*,...)* @printf(i8* getelementptr ([33 x i8]* @"(print-debug-memory-format)", i32 0, i32 0), i32 %alloc_count, i32 %nil_refcount)
    ret void
}

define fastcc %val* @"(addref)"(%val* %val) {
    %_refcount = getelementptr %val* %val, i32 0, i32 0
    %refcount_old = load i32* %_refcount
    %refcount_new = add i32 %refcount_old, 1
    store i32 %refcount_new, i32* %_refcount
    ret %val* %val
}

define fastcc void @"(deref)"(%val* %val) {
    %_refcount = getelementptr %val* %val, i32 0, i32 0
    %refcount_old = load i32* %_refcount
    %refcount_new = sub i32 %refcount_old, 1
    %refcount_positive = icmp sgt i32 %refcount_new, 0
    br i1 %refcount_positive, label %alive, label %dead
  alive:
    store i32 %refcount_new, i32* %_refcount
    ret void
  dead:
    %_eval = getelementptr %val* %val, i32 0, i32 3
    %eval = load %eval (i8*)** %_eval
    %eval_null = icmp eq %eval (i8*)* %eval, null
    %val_i8 = bitcast %val* %val to i8*
    br i1 %eval_null, label %evaluated, label %unevaluated
  evaluated:
    %_car = getelementptr %val* %val, i32 0, i32 1
    %car = load %val** %_car
    %_cdr = getelementptr %val* %val, i32 0, i32 2
    %cdr = load %val** %_cdr
    call fastcc void @"(free)"(i8* %val_i8)
    %car_null = icmp eq %val* %car, null
    br i1 %car_null, label %deref_car_done, label %deref_car
  deref_car:
    call fastcc void @"(deref)"(%val* %car)
    br label %deref_car_done
  deref_car_done:
    %cdr_null = icmp eq %val* %cdr, null
    br i1 %cdr_null, label %deref_cdr_done, label %deref_cdr
  deref_cdr:
    tail call fastcc void @"(deref)"(%val* %cdr)
    br label %deref_cdr_done
  deref_cdr_done:
    ret void
  unevaluated:
    %_freeenv = getelementptr %val* %val, i32 0, i32 4
    %freeenv = load void (i8*)** %_freeenv
    %_env = getelementptr %val* %val, i32 0, i32 5
    %env = load i8** %_env
    call fastcc void @"(free)"(i8* %val_i8)
    tail call fastcc void %freeenv(i8* %env)
    ret void
}

define fastcc void @"(eval)"(%val* %val) {
    %_eval = getelementptr %val* %val, i32 0, i32 3
    %eval = load %eval (i8*)** %_eval
    %eval_null = icmp eq %eval (i8*)* %eval, null
    br i1 %eval_null, label %evaluated, label %unevaluated
  evaluated:
    ret void
  unevaluated:
    store %eval (i8*)* null, %eval (i8*)** %_eval
    %_env = getelementptr %val* %val, i32 0, i32 5
    %env = load i8** %_env
    %eval_result = call fastcc %eval %eval(i8* %env)
    %eval_car = extractvalue %eval %eval_result, 0
    %eval_cdr = extractvalue %eval %eval_result, 1
    %_car = getelementptr %val* %val, i32 0, i32 1
    %_cdr = getelementptr %val* %val, i32 0, i32 2
    store %val* %eval_car, %val** %_car
    store %val* %eval_cdr, %val** %_cdr
    ret void
}

define fastcc i1 @"(nil?)"(%val* %val) {
    call fastcc void @"(eval)"(%val* %val)
    %_car = getelementptr %val* %val, i32 0, i32 1
    %car = load %val** %_car
    %result = icmp eq %val* %car, null
    ret i1 %result
}

define fastcc %val* @"(new-val)"(%eval (i8*)* %eval, void (i8*)* %freeenv, i8* %env) {
    %val_size = ptrtoint %val* getelementptr (%val* null, i32 1) to i32
    %val_i8 = call fastcc i8* @"(alloc)"(i32 %val_size)
    %val = bitcast i8* %val_i8 to %val*
    %_refcount = getelementptr %val* %val, i32 0, i32 0
    store i32 1, i32* %_refcount
    %_eval = getelementptr %val* %val, i32 0, i32 3
    store %eval (i8*)* %eval, %eval (i8*)** %_eval
    %_freeenv = getelementptr %val* %val, i32 0, i32 4
    store void (i8*)* %freeenv, void (i8*)** %_freeenv
    %_env = getelementptr %val* %val, i32 0, i32 5
    store i8* %env, i8** %_env
    ret %val* %val
}

define fastcc %val* @"(car)"(%val* %val) {
    %eval = bitcast %eval (%val*)* @"(car-eval)" to %eval (i8*)*
    %freeenv = bitcast void (%val*)* @"(deref)" to void (i8*)*
    %env = bitcast %val* %val to i8*
    %result = tail call fastcc %val* @"(new-val)"(%eval (i8*)* %eval, void (i8*)* %freeenv, i8* %env)
    ret %val* %result
}

define fastcc %eval @"(car-eval)"(%val* %val) {
    call fastcc void @"(eval)"(%val* %val)
    %_car = getelementptr %val* %val, i32 0, i32 1
    %car = load %val** %_car
    call fastcc void @"(eval)"(%val* %car)
    %_caar = getelementptr %val* %car, i32 0, i32 1
    %caar = load %val** %_caar
    call fastcc %val* @"(addref)"(%val* %caar)
    %_cdar = getelementptr %val* %car, i32 0, i32 2
    %cdar = load %val** %_cdar
    call fastcc %val* @"(addref)"(%val* %cdar)
    call fastcc void @"(deref)"(%val* %val)
    %result_1 = insertvalue %eval undef, %val* %caar, 0
    %result = insertvalue %eval %result_1, %val* %cdar, 1
    ret %eval %result
}

define fastcc %val* @"(cdr)"(%val* %val) {
    %eval = bitcast %eval (%val*)* @"(cdr-eval)" to %eval (i8*)*
    %freeenv = bitcast void (%val*)* @"(deref)" to void (i8*)*
    %env = bitcast %val* %val to i8*
    %result = tail call fastcc %val* @"(new-val)"(%eval (i8*)* %eval, void (i8*)* %freeenv, i8* %env)
    ret %val* %result
}

define fastcc %eval @"(cdr-eval)"(%val* %val) {
    call fastcc void @"(eval)"(%val* %val)
    %_cdr = getelementptr %val* %val, i32 0, i32 2
    %cdr = load %val** %_cdr
    call fastcc void @"(eval)"(%val* %cdr)
    %_cadr = getelementptr %val* %cdr, i32 0, i32 1
    %cadr = load %val** %_cadr
    call fastcc %val* @"(addref)"(%val* %cadr)
    %_cddr = getelementptr %val* %cdr, i32 0, i32 2
    %cddr = load %val** %_cddr
    call fastcc %val* @"(addref)"(%val* %cddr)
    call fastcc void @"(deref)"(%val* %val)
    %result_1 = insertvalue %eval undef, %val* %cadr, 0
    %result = insertvalue %eval %result_1, %val* %cddr, 1
    ret %eval %result
}

define fastcc %val* @"(cons)"(%val* %car, %val* %cdr) {
    %val_size = ptrtoint %val* getelementptr (%val* null, i32 1) to i32
    %val_i8 = call fastcc i8* @"(alloc)"(i32 %val_size)
    %val = bitcast i8* %val_i8 to %val*
    %_refcount = getelementptr %val* %val, i32 0, i32 0
    store i32 1, i32* %_refcount
    %_car = getelementptr %val* %val, i32 0, i32 1
    store %val* %car, %val** %_car
    %_cdr = getelementptr %val* %val, i32 0, i32 2
    store %val* %cdr, %val** %_cdr
    %_eval = getelementptr %val* %val, i32 0, i32 3
    store %eval (i8*)* null, %eval (i8*)** %_eval
    ret %val* %val
}

define fastcc %val* @"(if)"(%val* %cond, %val* %if_true, %val* %if_false) {
    %eval = bitcast %eval ([3 x %val*]*)* @"(if-eval)" to %eval (i8*)*
    %freeenv = bitcast void ([3 x %val*]*)* @"(if-freeenv)" to void (i8*)*
    %env_size = ptrtoint [3 x %val*]* getelementptr ([3 x %val*]* null, i32 1) to i32
    %env_i8 = call fastcc i8* @"(alloc)"(i32 %env_size)
    %env = bitcast i8* %env_i8 to [3 x %val*]*
    %_cond = getelementptr [3 x %val*]* %env, i32 0, i32 0
    store %val* %cond, %val** %_cond
    %_if_true = getelementptr [3 x %val*]* %env, i32 0, i32 1
    store %val* %if_true, %val** %_if_true
    %_if_false = getelementptr [3 x %val*]* %env, i32 0, i32 2
    store %val* %if_false, %val** %_if_false
    %result = tail call fastcc %val* @"(new-val)"(%eval (i8*)* %eval, void (i8*)* %freeenv, i8* %env_i8)
    ret %val* %result
}

define fastcc %eval @"(if-eval)"([3 x %val*]* %env) {
    %_cond = getelementptr [3 x %val*]* %env, i32 0, i32 0
    %cond = load %val** %_cond
    %cond_is_nil = call fastcc i1 @"(nil?)"(%val* %cond)
    br i1 %cond_is_nil, label %cond_is_false, label %cond_is_true
  cond_is_true:
    %_if_true = getelementptr [3 x %val*]* %env, i32 0, i32 1
    br label %eval_if
  cond_is_false:
    %_if_false = getelementptr [3 x %val*]* %env, i32 0, i32 2
    br label %eval_if
  eval_if:
    %_if_expr = phi %val** [ %_if_true, %cond_is_true ], [ %_if_false, %cond_is_false ]
    %if_expr = load %val** %_if_expr
    call fastcc %val* @"(addref)"(%val* %if_expr)
    call fastcc void @"(if-freeenv)"([3 x %val*]* %env)
    call fastcc void @"(eval)"(%val* %if_expr)
    %_car = getelementptr %val* %if_expr, i32 0, i32 1
    %car = load %val** %_car
    call fastcc %val* @"(addref)"(%val* %car)
    %_cdr = getelementptr %val* %if_expr, i32 0, i32 2
    %cdr = load %val** %_cdr
    call fastcc %val* @"(addref)"(%val* %cdr)
    call fastcc void @"(deref)"(%val* %if_expr)
    %result_1 = insertvalue %eval undef, %val* %car, 0
    %result = insertvalue %eval %result_1, %val* %cdr, 1
    ret %eval %result
}

define fastcc void @"(if-freeenv)"([3 x %val*]* %env) {
    %_cond = getelementptr [3 x %val*]* %env, i32 0, i32 0
    %cond = load %val** %_cond
    call fastcc void @"(deref)"(%val* %cond)
    %_if_true = getelementptr [3 x %val*]* %env, i32 0, i32 1
    %if_true = load %val** %_if_true
    call fastcc void @"(deref)"(%val* %if_true)
    %_if_false = getelementptr [3 x %val*]* %env, i32 0, i32 2
    %if_false = load %val** %_if_false
    call fastcc void @"(deref)"(%val* %if_false)
    %env_i8 = bitcast [3 x %val*]* %env to i8*
    tail call fastcc void @"(free)"(i8* %env_i8)
    ret void
}

declare i32 @read(i32,i8*,i32)
declare i32 @write(i32,i8*,i32)

define fastcc %val* @"(stdin)"() {
    %val = tail call fastcc %val* @"(stdin-val)"(i8 undef, i8 0)
    ret %val* %val
}

define fastcc %val* @"(stdin-val)"(i8 %byte, i8 %bit) {
    %stdin_eval = bitcast %eval ([2 x i8]*)* @"(stdin-eval)" to %eval (i8*)*
    %env_size = ptrtoint [2 x i8]* getelementptr ([2 x i8]* null, i32 1) to i32
    %env_i8 = call fastcc i8* @"(alloc)"(i32 %env_size)
    %env = bitcast i8* %env_i8 to [2 x i8]*
    %_byte = getelementptr [2 x i8]* %env, i32 0, i32 0
    store i8 %byte, i8* %_byte
    %_bit = getelementptr [2 x i8]* %env, i32 0, i32 1
    store i8 %bit, i8* %_bit
    %val = tail call fastcc %val* @"(new-val)"(%eval (i8*)* %stdin_eval, void (i8*)* @"(free)", i8* %env_i8)
    ret %val* %val
}

define fastcc %eval @"(stdin-eval)"([2 x i8]* %env) {
    %_byte = getelementptr [2 x i8]* %env, i32 0, i32 0
    %byte = load i8* %_byte
    %_bit = getelementptr [2 x i8]* %env, i32 0, i32 1
    %bit = load i8* %_bit
    %need_read_new_byte = icmp sle i8 %bit, 0
    %env_i8 = bitcast [2 x i8]* %env to i8*
    call fastcc void @"(free)"(i8* %env_i8)
    br i1 %need_read_new_byte, label %read_new_byte, label %continue_byte
  read_new_byte:
    %_alloca_byte = alloca i8
    %count = call i32 @read(i32 0, i8* %_alloca_byte, i32 1)
    %read_byte = load i8* %_alloca_byte
    %count_is_one = icmp eq i32 %count, 1
    br i1 %count_is_one, label %return_next_bit, label %read_fail
  read_fail:
    %nil_result_1 = insertvalue %eval undef, %val* null, 0
    %nil_result = insertvalue %eval %nil_result_1, %val* null, 1
    ret %eval %nil_result
  continue_byte:
    %continue_bit = sub i8 %bit, 1
    br label %return_next_bit
  return_next_bit:
    %new_bit = phi i8 [ %continue_bit, %continue_byte ], [ 7, %read_new_byte ]
    %new_byte = phi i8 [ %byte, %continue_byte ], [ %read_byte, %read_new_byte ]
    %next_val = call fastcc %val* @"(stdin-val)"(i8 %new_byte, i8 %new_bit)
    %nil_val = call fastcc %val* @"(addref)"(%val* @C0)
    %shifted_byte = lshr i8 %new_byte, %new_bit
    %bit_value = trunc i8 %shifted_byte to i1
    br i1 %bit_value, label %bit_is_1, label %bit_is_0
  bit_is_1:
    %result1_1 = insertvalue %eval undef, %val* %next_val, 0
    %result1 = insertvalue %eval %result1_1, %val* %nil_val, 1
    ret %eval %result1
  bit_is_0:
    %result0_1 = insertvalue %eval undef, %val* %nil_val, 0
    %result0 = insertvalue %eval %result0_1, %val* %next_val, 1
    ret %eval %result0
}

define fastcc void @"(print)"(%val* %initial_val) {
  init:
    %_byte = alloca i8
    br label %loop
  loop:
    %byte = phi i8 [ 0, %init ], [ %next_byte, %continue_loop ], [ 0, %write_byte ]
    %bit = phi i8 [ 128, %init ], [ %next_bit, %continue_loop ], [ 128, %write_byte ]
    %val = phi %val* [ %initial_val, %init ], [ %next_val, %continue_loop ], [ %next_val, %write_byte ]
    %val_is_nil = call fastcc i1 @"(nil?)"(%val* %val)
    br i1 %val_is_nil, label %on_val_is_nil, label %on_val_is_not_nil
  on_val_is_nil:
    tail call fastcc void @"(deref)"(%val* %val)
    ret void
  on_val_is_not_nil:
    %_car = getelementptr %val* %val, i32 0, i32 1
    %car = load %val** %_car
    %car_is_nil = call fastcc i1 @"(nil?)"(%val* %car)
    br i1 %car_is_nil, label %on_car_is_nil, label %on_car_is_not_nil
  on_car_is_not_nil:
    %next_byte_one = or i8 %byte, %bit
    br label %continue_loop
  on_car_is_nil:
    %_cdr = getelementptr %val* %val, i32 0, i32 2
    %cdr = load %val** %_cdr
    br label %continue_loop
  continue_loop:
    %next_byte = phi i8 [ %next_byte_one, %on_car_is_not_nil ], [ %byte, %on_car_is_nil ]
    %next_val = phi %val* [ %car, %on_car_is_not_nil ], [ %cdr, %on_car_is_nil ]
    call fastcc %val* @"(addref)"(%val* %next_val)
    call fastcc void @"(deref)"(%val* %val)
    %next_bit = lshr i8 %bit, 1
    %need_to_write_byte = icmp eq i8 %next_bit, 0
    br i1 %need_to_write_byte, label %write_byte, label %loop
  write_byte:
    store i8 %next_byte, i8* %_byte
    call i32 @write(i32 1, i8* %_byte, i32 1)
    br label %loop
}

define fastcc void @"(print-val)"(%val* %val) {
    %buffer = alloca i8
    store i8 40, i8* %buffer
    call i32 @write(i32 1, i8* %buffer, i32 1)
    call fastcc void @"(print-val-internal)"(%val* %val, i8* %buffer)
    store i8 41, i8* %buffer
    call i32 @write(i32 1, i8* %buffer, i32 1)
    ret void
}

define fastcc void @"(print-val-internal)"(%val* %val, i8* %buffer) {
    %is_nil = call fastcc i1 @"(nil?)"(%val* %val)
    br i1 %is_nil, label %on_nil, label %on_not_nil
  on_nil:
    call fastcc void @"(deref)"(%val* %val)
    ret void
  on_not_nil:
    %_car = getelementptr %val* %val, i32 0, i32 1
    %car = load %val** %_car
    call fastcc %val* @"(addref)"(%val* %car)
    %_cdr = getelementptr %val* %val, i32 0, i32 2
    %cdr = load %val** %_cdr
    call fastcc %val* @"(addref)"(%val* %cdr)
    call fastcc void @"(deref)"(%val* %val)
    store i8 40, i8* %buffer
    call i32 @write(i32 1, i8* %buffer, i32 1)
    call fastcc void @"(print-val-internal)"(%val* %car, i8* %buffer)
    store i8 41, i8* %buffer
    call i32 @write(i32 1, i8* %buffer, i32 1)
    call fastcc void @"(print-val-internal)"(%val* %cdr, i8* %buffer)
    ret void
}

declare fastcc %val* @"(main)"(%val*)

define fastcc void @"((main))"(void (%val*)* %print, void ()* %print_debug) {
    %stdin = call fastcc %val* @"(stdin)"()
    %result = call fastcc %val* @"(main)"(%val* %stdin)
    call fastcc void %print(%val* %result)
    %print_debug_null = icmp eq void ()* %print_debug, null
    br i1 %print_debug_null, label %no_debug, label %debug
  debug:
    tail call fastcc void %print_debug()
    ret void
  no_debug:
    ret void
}

define fastcc void @"(main-)"() {
    tail call fastcc void @"((main))"(void (%val*)* @"(print)", void ()* null)
    ret void
}

define fastcc void @"(main-d)"() {
    tail call fastcc void @"((main))"(void (%val*)* @"(print)", void ()* @"(print-debug-memory)")
    ret void
}

define fastcc void @"(main-v)"() {
    tail call fastcc void @"((main))"(void (%val*)* @"(print-val)", void ()* null)
    ret void
}

define fastcc void @"(main-d-v)"() {
    tail call fastcc void @"((main))"(void (%val*)* @"(print-val)", void ()* @"(print-debug-memory)")
    ret void
}

@debug_fmt = private constant [8 x i8] c"[%d:%d]\00"
define fastcc void @debug(i32 %i, i32 %j) {
    tail call void (i8*,...)* @printf(i8* getelementptr ([8 x i8]* @debug_fmt, i32 0, i32 0), i32 %i, i32 %j)
    ret void
}
