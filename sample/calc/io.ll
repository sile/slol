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


;;;; input
%File = type opaque*
@stdin  = external global %File
@cur_ch = internal global i32 0

declare i32 @ungetc(i32, %File)
declare i32 @getc(%File)

define i8 @in.read_char() {
  %stdin = load %File* @stdin
  %ch = call i32 @getc(%File %stdin)
  store i32 %ch, i32* @cur_ch
  
  %c = trunc i32 %ch to i8
  ret i8 %c
}

define i1 @in.eof() {
  %ch = load i32* @cur_ch
  %ret = icmp eq i32 %ch, _EOF
  br i1 %ret, label %EOF, label %NORMAL

EOF:
  ret i1 true

NORMAL:
  ret i1 false
}

define void @in.unread_char() {
 %ch = load i32* @cur_ch
 %stdin = load %File* @stdin 
 call i32 @ungetc(i32 %ch, %File %stdin)
 ret void
}

; define i32 @main() {
; HEAD:
;   %first = call i8 @in.read_char()
;   br label %LOOP

; LOOP:
;   %ch = phi i8 [%first, %HEAD],
;                [%new_ch, %NEXT]
;   %eof = call i1 @in.eof()
;   br i1 %eof, label %FINISH, label %NEXT
  
; NEXT:
;   call void @out.write_char(i8 %ch)
;   %new_ch = call i8 @in.read_char()
;   br label %LOOP
  
; FINISH:
;   ret i32 0
; }