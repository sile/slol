;;;; ファイル名: eval.ll
;;;;
;;;; [説明]
;;;;

include(`type.inc')
include(`global.inc')

define i32 @eval_exp(%Object* %exp) {
  %p_type = getelementptr %Object* %exp, i32 0, i32 0
  %type = load i8* %p_type
  switch i8 %type, label %ERROR [i8 _OBJ_TYPE, label %ERROR
                                 i8 _NUM_TYPE, label %NUM
                                 i8 _OP_TYPE, label %ERROR
                                 i8 _LIST_TYPE, label %LIST]
  
ERROR:
  ret i32 -1  ; XXX:

NUM:
  %numobj = bitcast %Object* %exp to %NumberObject*
  %p_num = getelementptr %NumberObject* %numobj, i32 0, i32 1
  %num = load i32* %p_num
  ret i32 %num

LIST:
  %num2 = call i32 @eval_list(%Object* %exp)
  ret i32 %num2
}

%OpFn = type i32 (i32,i32)*

define i32 @eval_list(%Object* %exp) {
  %car = call %Object* @list.car(%Object* %exp)
  %fn = call %OpFn (%Object*)* @get_operator_fn(%Object* %car)

  %exp2 = call %Object* @list.cdr(%Object* %exp)
  %car2 = call %Object* @list.car(%Object* %exp2)

  %first_num = call i32 @eval_exp(%Object* %exp2)
  %exp3 = call %Object* @list.cdr(%Object* %exp2)

  %num = call i32 @reduce(%OpFn %fn, i32 %first_num, %Object* %exp3)
  ret i32 %num
}

define i32 @reduce(%OpFn %fn, i32 %num, %Object* %exp) {
  %empty = call i1 @list.is_empty(%Object* %exp)
  br i1 %empty, label %END, label %NEXT

END:
  ret i32 %num

NEXT:
  %car = call %Object* @list.car(%Object* %exp)
  %cdr = call %Object* @list.cdr(%Object* %exp)
  %n = call i32 @eval_exp(%Object* %car)
  %new_num = call i32 %fn(i32 %num, i32 %n)

  %rlt = call i32 @reduce(%OpFn %fn, i32 %new_num, %Object* %cdr)
  ret i32 %rlt
}

define %OpFn @get_operator_fn(%Object* %obj) {
  %opobj = bitcast %Object* %obj to %OperatorObject*
  %p_op = getelementptr %OperatorObject* %opobj, i32 0, i32 1
  %op = load i32* %p_op

  switch i32 %op, label %UNKNOWN [i32 _OP_ADD, label %ADD
                                  i32 _OP_MUL, label %MUL
                                  i32 _OP_SUB, label %SUB
                                  i32 _OP_DIV, label %DIV]

UNKNOWN:
  ret %OpFn null ; XXX

ADD:
  ret %OpFn @add_fn

MUL:
  ret %OpFn @mul_fn

SUB:
  ret %OpFn @sub_fn

DIV:
  ret %OpFn @div_fn
}

define i32 @add_fn(i32 %a, i32 %b) {
  %c = add i32 %a, %b
  ret i32 %c
}

define i32 @mul_fn(i32 %a, i32 %b) {
  %c = mul i32 %a, %b
  ret i32 %c
}

define i32 @sub_fn(i32 %a, i32 %b) {
  %c = sub i32 %a, %b
  ret i32 %c
}

define i32 @div_fn(i32 %a, i32 %b) {
  %c = sdiv i32 %a, %b
  ret i32 %c
}

define i32 @main() {
  %obj = call %Object* @parse()
  %rlt = call i32 @eval_exp(%Object* %obj)
;  call void @pprint(%Object* %obj)
  call void @out.write_int(i32 %rlt)
  call void @out.newline()
  ret i32 0
}
