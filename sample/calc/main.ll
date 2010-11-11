;;;; ファイル名: main.ll
;;;;
;;;; [説明]
;;;;

include(`type.inc')
include(`global.inc')
define i32 @main() {
HEAD:
  %init_obj = call %Object* @parse()
  br label %LOOP

LOOP:
  %obj = phi %Object* [%init_obj, %HEAD], [%next_obj, %NEXT]
  %end = icmp eq %Object* null, %obj
  br i1 %end, label %END, label %NEXT

NEXT:
  call void @out.write_char(i8 61)
  call void @out.write_char(i8 62)
  call void @out.write_char(i8 32)
  call void @pprint(%Object* %obj)
  call void @out.newline()

  %num = call i32 @eval_exp(%Object* %obj)
  call void @out.write_char(i8 61)
  call void @out.write_char(i8 61)
  call void @out.write_char(i8 62)
  call void @out.write_char(i8 32)
  call void @out.write_int(i32 %num)
  call void @out.newline()
  call void @out.newline()

  %next_obj = call %Object* @parse()
  br label %LOOP

END:
  ret i32 0
}
