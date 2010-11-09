;;;; ファイル名: main.ll
;;;;
;;;; [説明]
;;;;

include(`type.inc')
include(`global.inc')

@MSG = internal constant _String(4) c"abc\00"

define void @main() {
  %s = _GetString(@MSG,4)
  call void @out.write_string(i8* %s)  
  call void @out.space()
  call void @out.write_int(i32 -1)
  call void @out.newline()
  ret void
}