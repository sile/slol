;;;; ファイル名: parse-sexp.ll
;;;;
;;;; [概要]
;;;; - 標準入力からS式を読み込む
;;;; - XXX形式で標準出力に出力する

%FILE = type opaque*
%STREAM = type {%FILE, i8, i1}

;declare void @in.init(%STREAM* %in) 
;declare i1 @in.eos(%STREAM* %in)
;declare i8 @in.read(%STREAM* %in)
;declare void @in.unread(%STREAM* %in)
;declare void @out.write(i8)

declare i32 @llvm.objectsize.i32(i8*, i1)

define i32 @main() {
;  %in = alloca %STREAM
;  call void @in.init(%STREAM* %in)
  
;  %c = call i8 @in.read(%STREAM* %in)

;  call void @out.write(i8 10)
;  call void @out.write(i8 %c)
;  call void @out.write(i8 10)

  %beg = getelementptr %STREAM* null, i32 1
  %size = ptrtoint %STREAM* %beg to i32 
  ret i32 %size
}

