;;;; ファイル名: stream.ll
;;;; 
;;;; [概要]
;;;; - IOストリーム関連の操作をラッピングしたモジュール

%FILE = type opaque*

@stdin = external global %FILE
@stdout = external global %FILE
@EOF = internal constant i32 -1 ;external constant i32

declare i32 @getc(%FILE)
declare i32 @ungetc(i32, %FILE)
declare i32 @putchar(i8)

%STREAM = type {%FILE, i8, i1}

define void @in.init(%STREAM* %in) {
  %src = load %FILE* @stdin

  %in.src_p  = getelementptr %STREAM* %in, i32 0, i32 0
  %in.prev_p = getelementptr %STREAM* %in, i32 0, i32 1
  %in.eos_p  = getelementptr %STREAM* %in, i32 0, i32 2

  store %FILE %src, %FILE* %in.src_p
  store i8 0, i8* %in.prev_p
  store i1 0, i1* %in.eos_p
  ret void
}

define i1 @in.eos(%STREAM* %in) {
  %in.eos_p  = getelementptr %STREAM* %in, i32 0, i32 2
  %eos = load i1* %in.eos_p
  ret i1 %eos
}

define i8 @in.read(%STREAM* %in) {
  %in.src_p  = getelementptr %STREAM* %in, i32 0, i32 0
  %in.prev_p = getelementptr %STREAM* %in, i32 0, i32 1
  %in.eos_p  = getelementptr %STREAM* %in, i32 0, i32 2
 
  %src = load %FILE* %in.src_p
  %ret = call i32 @getc(%FILE %src)
  %eos = load i32* @EOF

  %is_eos = icmp eq i32 %eos, %ret
  br i1 %is_eos, label %EOS, label %NORMAL

NORMAL:
  %c = trunc i32 %ret to i8
  store i8 %c, i8* %in.prev_p
  ret i8 %c

EOS:
  store i1 1, i1* %in.eos_p
  ret i8 0
}

define void @in.unread(%STREAM* %in) {
  %in.src_p  = getelementptr %STREAM* %in, i32 0, i32 0
  %in.prev_p = getelementptr %STREAM* %in, i32 0, i32 1

  %src  = load %FILE* %in.src_p
  %prev = load i8* %in.prev_p
  %c    = zext i8 %prev to i32
  call i32 @ungetc(i32 %c, %FILE %src) 
  ret void
}

define void @out.write(i8 %c) {
  call i32 @putchar(i8 %c)
  ret void
}