;;;; ファイル名: number.ll
;;;;
;;;; [説明]
;;;;

include(`type.inc')
include(`global.inc')

@llvm.global_ctors = appending global [1 x %Ctor] [%Ctor {i32 100, void ()* @register_number_ppdf}]
 
define %Object* @make_number_object(i32 %num) {
  %p_tmp = call i8* @malloc(i32 _SIZEOF(%NumberObject))
  %p_numobj = bitcast i8* %p_tmp to %NumberObject*

  %p_type= getelementptr %NumberObject* %p_numobj, i32 0, i32 0
  %p_num = getelementptr %NumberObject* %p_numobj, i32 0, i32 1
  store i8 _NUM_TYPE, i8* %p_type
  store i32 %num, i32* %p_num
  
  %p_obj = bitcast %NumberObject* %p_numobj to %Object*
  ret %Object* %p_obj
}

define void @number_ppdf(%Object* %p_obj) {
  %p_numobj = bitcast %Object* %p_obj to %NumberObject*
  %p_num = getelementptr %NumberObject* %p_numobj, i32 0, i32 1
  %num = load i32* %p_num
  call void @out.write_int(i32 %num) 
  ret void
}

define void @register_number_ppdf() {
  call void @register_pp_dispatch_fn(i8 _NUM_TYPE, %PrintFunction @number_ppdf)
  ret void
}


define void @main() {
  %p_obj = call %Object* @make_number_object(i32 98)
  call void @pprint(%Object* %p_obj)
  ret void
}