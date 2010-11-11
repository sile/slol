;;;; ファイル名: parse.ll
;;;;
;;;; [説明]
;;;; - S式を標準入力から読み込み

include(`type.inc')
include(`global.inc')

define %Object* @parse() {
  %c = call i8 @in.read_char()
  %eof = call i1 @in.eof()

  br i1 %eof, label %EOF, label %NORMAL

EOF:
  ret %Object* null  ; XXX: may raise segmentation fault

NORMAL:
  %type = call i8 @get_char_type(i8 %c)
  switch i8 %type, label %UNKNOWN [i8 0, label %OPEN_PAREN
                                   i8 1, label %CLOSE_PAREN
                                   i8 2, label %NUMBER
                                   i8 3, label %OPERATOR
                                   i8 4, label %SPACE]
UNKNOWN:
  br label %EOF  ; XXX:

OPEN_PAREN:
  %exp = call %Object* @parse_list(i8 %c)
  ret %Object* %exp

CLOSE_PAREN:
  br label %EOF  ; XXX:

NUMBER:
  %num = call %Object* @parse_number(i8 %c)
  ret %Object* %num

OPERATOR:
  %op = call %Object* @parse_operator(i8 %c)
  ret %Object* %op

SPACE:
  %obj = call %Object* @parse()
  ret %Object* %obj
}

define i8 @get_char_type(i8 %c) {
OPEN_PAREN:
  %b1 = icmp eq i8 40, %c
  br i1 %b1, label %RETURN, label %CLOSE_PAREN

CLOSE_PAREN:
  %b2 = icmp eq i8 41, %c
  br i1 %b2, label %RETURN, label %NUMBER

NUMBER:
  %b3 = icmp sle i8 48, %c
  %b4 = icmp sge i8 57, %c
  %b5 = and i1 %b3, %b4
  br i1 %b5, label %RETURN, label %OPERATOR

OPERATOR:
  %b6 = icmp eq i8 42, %c
  %b7 = icmp eq i8 43, %c
  %b8 = icmp eq i8 45, %c
  %b9 = icmp eq i8 47, %c
  %b10 = or i1 %b6, %b7
  %b11 = or i1 %b8, %b9
  %b12 = or i1 %b10, %b11
  br i1 %b12, label %RETURN, label %SPACE

SPACE:
  %b13 = icmp eq i8 32, %c
  %b14 = icmp eq i8 10, %c
  %b15 = or i1 %b13, %b14
  br i1 %b15, label %RETURN, label %OTHERWISE

OTHERWISE:
  br label %RETURN
        
RETURN:
  %type = phi i8 [0, %OPEN_PAREN],
                 [1, %CLOSE_PAREN],
                 [2, %NUMBER],
                 [3, %OPERATOR],
                 [4, %SPACE],
                 [5, %OTHERWISE]
  ret i8 %type                     
}

define %Object* @parse_list(i8 %c) {
HEAD:
  %init_list = call %Object* @make_list_object()
  br label %LOOP

LOOP:
  %list = phi %Object* [%init_list, %HEAD], [%new_list, %NEXT]
  %obj = call %Object* @parse()
  ;; XXX: close?
  %close = icmp eq %Object* null, %obj
  br i1 %close, label %FINISH, label %NEXT

NEXT:
  %new_list = call %Object* @list.cons(%Object* %obj, %Object* %list)
  br label %LOOP  

FINISH:
  %rev = call %Object* @list.reverse(%Object* %list)      
  ret %Object* %rev
}

define %Object* @parse_number(i8 %init_c) {
HEAD:
  %init_c32 = zext i8 %init_c to i32
  %init_num = sub i32 %init_c32, 48
  br label %LOOP

LOOP:
  %num = phi i32 [%init_num, %HEAD], [%next_num, %NEXT]
  %c = call i8 @in.read_char()
  %eof = call i1 @in.eof()
  br i1 %eof, label %FINISH, label %CHECK

CHECK:
  %type = call i8 @get_char_type(i8 %c)
  %b = icmp eq i8 2, %type 
  br i1 %b, label %NEXT, label %UNREAD

NEXT:
  %c32 = zext i8 %c to i32
  %tmp1 = sub i32 %c32, 48
  %tmp2 = mul i32 %num, 10
  %next_num = add i32 %tmp1, %tmp2
  br label %LOOP

UNREAD:
  call void @in.unread_char()
  br label %FINISH

FINISH:
  %obj = call %Object* @make_number_object(i32 %num)
  ret %Object* %obj  
}

define %Object* @parse_operator(i8 %c) {
  %obj = call %Object* @make_operator_object_from_char(i8 %c)
  ret %Object* %obj
}

; define i32 @main() {
;   %obj = call %Object* @parse()
;   call void @pprint(%Object* %obj)
;   call void @out.newline()
;   ret i32 0
; }
