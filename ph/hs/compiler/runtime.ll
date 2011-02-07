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

@C0 = global %val { i32 1, %val* null, %val* null, %eval (i8*)* null, void (i8*)* undef, i8* undef }

declare i8* @malloc(i32)
declare void @free(i8*)

@"(alloc_count)" = global i32 0

define fastcc void @"(free)"(i8* %ptr) {
    %alloc_count_old = load i32* @"(alloc_count)"
    %alloc_count_new = sub i32 %alloc_count_old, 1
    store i32 %alloc_count_new, i32* @"(alloc_count)"
    call void @free(i8* %ptr)
    ret void
}

define fastcc i8* @"(alloc)"(i32 %size) {
    %alloc_count_old = load i32* @"(alloc_count)"
    %alloc_count_new = add i32 %alloc_count_old, 1
    store i32 %alloc_count_new, i32* @"(alloc_count)"
    %ptr = call i8* @malloc(i32 %size)
    ret i8* %ptr
}

declare void @printf(i8*,...)

@"(print_debug_memory_format)" = private constant [33 x i8] c"\0Aalloc_count=%d nil.refcount=%d\0A\00"

define fastcc void @"(print_debug_memory)"() {
    %alloc_count = load i32* @"(alloc_count)"
    %nil_refcount = load i32* getelementptr (%val* @C0, i32 0, i32 0)
    tail call void (i8*,...)* @printf(i8* getelementptr ([33 x i8]* @"(print_debug_memory_format)", i32 0, i32 0), i32 %alloc_count, i32 %nil_refcount)
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
