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
