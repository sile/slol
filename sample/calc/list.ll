;;;; ファイル名: list.ll
;;;;
;;;; [説明]
;;;;

include(`type.inc')
include(`global.inc')

@list.null = constant %Cons {%ObjType _LIST_TYPE, %Car null, %Cdr @list.null}

@llvm.global_ctors = appending global [1 x %Ctor] 
                                      [%Ctor {i32 100, void ()* @register_list_ppdf}]

define %Car @list.car(%Object* %list) {
  %tmp = bitcast %Object* %list to %ListObject
  %p_car = getelementptr %ListObject %tmp, i32 0, i32 1
  %car = load %Car* %p_car
  ret %Car %car
}

define %Object* @list.cdr(%Object* %list) {
  %tmp = bitcast %Object* %list to %ListObject
  %p_cdr = getelementptr %ListObject %tmp, i32 0, i32 2
  %cdr = load %Cdr* %p_cdr

  %cdrobj = bitcast %Cdr %cdr to %Object*
  ret %Object* %cdrobj
}

define %Object* @make_list_object() {
  %p_obj = bitcast %ListObject @list.null to %Object*
  ret %Object* %p_obj
}
 
define %Object* @list.cons(%Object* %elem, %Object* %listobj) {
  %list = bitcast %Object* %listobj to %ListObject
     
  %mem = call i8* @malloc(i32 _SIZEOF(%Cons))
  %p_cons = bitcast i8* %mem to %Cons*

  %p_type = getelementptr %Cons* %p_cons, i32 0, i32 0
  %p_car  = getelementptr %Cons* %p_cons, i32 0, i32 1
  %p_cdr  = getelementptr %Cons* %p_cons, i32 0, i32 2

  store %ObjType _LIST_TYPE, %ObjType* %p_type
  store %Car %elem, %Car* %p_car
  store %Cdr %list, %Cdr* %p_cdr
 
  %newlistobj = bitcast %ListObject %p_cons to %Object*
  ret %Object* %newlistobj
}

define void @list_ppdf(%Object* %listobj) {
  %list = bitcast %Object* %listobj to %ListObject
  call void @out.write_char(i8 40)
  call void @list_print_elements(%ListObject %list, i1 1)
  call void @out.write_char(i8 41)
  ret void
}

define void @list_print_elements(%ListObject %list, i1 %is_first) {
  %i_null = ptrtoint %ListObject @list.null to i64
  %i_list = ptrtoint %ListObject %list to i64

  %ret = icmp eq i64 %i_null, %i_list
  br i1 %ret, label %NULL, label %CONS
  
NULL:
  ret void

CONS:
  br i1 %is_first, label %PRINT_ELEM, label %PRINT_DELIM

PRINT_DELIM:
  call void @out.write_char(i8 32)
  br label %PRINT_ELEM

PRINT_ELEM:   
  %listobj = bitcast %ListObject %list to %Object*
        
  %car = call %Car @list.car(%Object* %listobj)
  call void @pprint(%Object* %car)

  %cdrobj = call %Object* @list.cdr(%Object* %listobj)
  %cdr = bitcast %Object* %cdrobj to %Cdr
  call void @list_print_elements(%ListObject %cdr, i1 0)

  ret void
}

define %Object* @list.reverse(%Object* %listobj) {
  %empty = call %Object* @make_list_object()
  %rev = call %Object* @list.reverse_impl(%Object* %listobj, %Object* %empty)
  ret %Object* %rev
}

define %Object* @list.reverse_impl(%Object* %listobj, %Object* %revlist) {
  %i_null = ptrtoint %ListObject @list.null to i64
  %i_list = ptrtoint %Object* %listobj to i64

  %ret = icmp eq i64 %i_null, %i_list
  br i1 %ret, label %NULL, label %CONS

NULL:
  ret %Object* %revlist

CONS:
  %elem = call %Car @list.car(%Object* %listobj)
  %rest = call %Object* @list.cdr(%Object* %listobj)

  %revlist2 = call %Object* @list.cons(%Object* %elem, %Object* %revlist)
  %revlist3 = call %Object* @list.reverse_impl(%Object* %rest, %Object* %revlist2)
  ret %Object* %revlist3
}

define void @register_list_ppdf() {
  call void @register_pp_dispatch_fn(i8 _LIST_TYPE, %PrintFunction @list_ppdf)
  ret void
}

; define void @main() {
;   %p_obj = call %Object* @make_list_object()
;   %numobj = call %Object* @make_number_object(i32 22)
;   %p_obj2 = call %Object* @list.cons(%Object* %numobj, %Object* %p_obj)
;   %p_obj3 = call %Object* @list.cons(%Object* %numobj, %Object* %p_obj2)
;   %p_obj4 = call %Object* @list.cons(%Object* %p_obj2, %Object* %p_obj3)
;   %p_obj5 = call %Object* @list.reverse(%Object* %p_obj4)
;   call void @pprint(%Object* %p_obj5)
;   ret void
; }
