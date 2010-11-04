;;; ファイル名: hanoi.ll
; printf関数に渡すフォーマット文字列
@.MSG = internal constant [10 x i8] c"%c -> %c\0A\00"

declare i32 @printf(i8*, i8, i8)

; ハノイの塔 : 再帰関数
define void @hanoi(i8 %start, i8 %tmp, i8 %dist, i32 %level) {
  %cond = icmp eq i32 %level, 0
  br i1 %cond, label %end, label %recur
recur:
  %msg = getelementptr [10 x i8]* @.MSG, i64 0, i64 0

  %level2 = sub i32 %level, 1
  call void @hanoi(i8 %start, i8 %dist, i8 %tmp, i32 %level2)
  call i32 @printf(i8* %msg, i8 %start, i8 %dist)
  call void @hanoi(i8 %tmp, i8 %start, i8 %dist, i32 %level2)
  ret void
end:
  ret void
}

; メイン関数
define i32 @main() {
  call void @hanoi(i8 65, i8 66, i8 67, i32 5)
  ret i32 0
}
