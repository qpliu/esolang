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
