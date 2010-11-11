;;;; ファイル名: operator.ll
;;;;
;;;; [説明]
;;;;

include(`type.inc')
include(`global.inc')

@llvm.global_ctors = appending global [1 x %Ctor] 
                                      [%Ctor {i32 100, void ()* @register_operator_ppdf}]
 
define %Object* @make_operator_object(i32 %op) {
  %p_tmp = call i8* @malloc(i32 _SIZEOF(%OperatorObject))
  %p_opobj = bitcast i8* %p_tmp to %OperatorObject*

  %p_type= getelementptr %OperatorObject* %p_opobj, i32 0, i32 0
  %p_op  = getelementptr %OperatorObject* %p_opobj, i32 0, i32 1
  store i8 _OP_TYPE, i8* %p_type
  store i32 %op, i32* %p_op
  
  %p_obj = bitcast %OperatorObject* %p_opobj to %Object*
  ret %Object* %p_obj
}

define %Object* @make_operator_object_from_char(i8 %c) {
MUL:
  %b1 = icmp eq i8 42, %c
  br i1 %b1, label %RETURN, label %ADD

ADD:
  %b2 = icmp eq i8 43, %c
  br i1 %b2, label %RETURN, label %SUB

SUB:
  %b3 = icmp eq i8 45, %c
  br i1 %b3, label %RETURN, label %DIV

DIV:
  br label %RETURN
  
RETURN:
  %op = phi i32 [_OP_MUL, %MUL],
                [_OP_ADD, %ADD],
                [_OP_SUB, %SUB],
                [_OP_DIV, %DIV]

  %obj = call %Object* @make_operator_object(i32 %op)
  ret %Object* %obj
}

define void @operator_ppdf(%Object* %p_obj) {
  %p_opobj = bitcast %Object* %p_obj to %OperatorObject*
  %p_op = getelementptr %OperatorObject* %p_opobj, i32 0, i32 1
  %op = load i32* %p_op

  switch i32 %op, label %Otherwise 
                  [i32 _OP_ADD, label %Add
                   i32 _OP_SUB, label %Sub
                   i32 _OP_MUL, label %Mul
                   i32 _OP_DIV, label %Div]

Otherwise:
  call void @out.write_char(i8 63) ; '?'
  ret void

Add:
  call void @out.write_char(i8 43) ; '+'
  ret void

Sub:
  call void @out.write_char(i8 45) ; '-'
  ret void

Mul:
  call void @out.write_char(i8 42) ; '*'
  ret void

Div:
  call void @out.write_char(i8 47) ; '/'
  ret void
}

define void @register_operator_ppdf() {
  call void @register_pp_dispatch_fn(i8 _OP_TYPE, %PrintFunction @operator_ppdf)
  ret void
}

;define void @main() {
;  %p_obj = call %Object* @make_operator_object(i32 _OP_ADD)
;  call void @pprint(%Object* %p_obj)
;  ret void
;}