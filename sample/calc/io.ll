;;;; ファイル名: io.ll
;;;;
;;;; [説明]
;;;;
include(`type.inc')

@NUM_FMT = internal constant _String(3) c"%d\00"

declare i32 @putchar(i8)
declare i32 @printf(i8*, ...)

define void @out.write_char(i8 %c) {
  call i32 @putchar(i8 %c)
  ret void
}

define void @out.write_string(i8* %s) {
Head:
  br label %Loop

Loop:
  %i = phi i32 [0, %Head], [%next_i, %NextLoop]
  %p_ch = getelementptr i8* %s, i32 %i
  %ch = load i8* %p_ch
  %ret = icmp eq i8 %ch, 0
  br i1 %ret, label %End, label %NextLoop

NextLoop:
  call void @out.write_char(i8 %ch)
  %next_i = add i32 %i, 1
  br label %Loop

End:
  ret void
}

define void @out.newline() {
  call void @out.write_char(i8 10)
  ret void
}

define void @out.space() {
  call void @out.write_char(i8 32)
  ret void
}

define void @out.write_int(i32 %n) {
  %fmt = _GetString(@NUM_FMT,3)
  call i32 (i8*,...)* @printf(i8* %fmt, i32 %n)
  ret void
}

