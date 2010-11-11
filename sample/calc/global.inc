;;;; ファイル名: global.inc
;;;;
;;;; [説明]
;;;;

declare void @out.write_char(i8)
declare void @out.write_string(i8*)
declare void @out.write_int(i32)
declare void @out.newline()
declare void @out.space()

declare i8* @malloc(i32)
declare void @free(i8*)

declare void @register_pp_dispatch_fn(i8, %PrintFunction)
declare void @pprint(%Object*)

declare %Object* @make_number_object(i32)
declare %Object* @make_operator_object(i32)
declare %Object* @make_list_object()

declare %Object* @list.cons(%Object*, %Object*)
declare %Object* @list.car(%Object*)
declare %Object* @list.cdr(%Object*)
declare %Object* @list.reverse(%Object*)

declare i8 @in.read_char()
declare void @in.unread_char()
declare i1 @in.eof()
