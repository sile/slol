;;;; ファイル名: object.ll
;;;;
;;;; [説明]
;;;;

include(`type.inc')
include(`global.inc')

@g_ppdt = global %PPDT zeroinitializer

declare i32 @printf(i8*, ...)
@PP_OBJ_MSG = internal constant _String(15) c"#<object 0x%x>\00"

@llvm.global_ctors = appending global [1 x %Ctor] [%Ctor {i32 100, void ()* @register_object_ppdf}]

define void @register_pp_dispatch_fn(%ObjType %type, %PrintFunction %fn) {
  %p_fn = getelementptr %PPDT* @g_ppdt, i32 0, i8 %type
  store %PrintFunction %fn, %PrintFunction* %p_fn
  ret void
}

define void @object_ppdf(%Object* %obj) {
  %msg = _GetString(@PP_OBJ_MSG, 15)
  %addr = ptrtoint %Object* %obj to i32
  call i32 (i8*,...)* @printf(i8* %msg, i32 %addr)
  ret void
}

define void @register_object_ppdf() {
  call void @register_pp_dispatch_fn(i8 _OBJ_TYPE, %PrintFunction @object_ppdf)
  ret void
}

define void @pprint(%Object* %p_obj) {
  %p_type = getelementptr %Object* %p_obj, i32 0, i32 0
  %type = load i8* %p_type
  %p_fn = getelementptr %PPDT* @g_ppdt, i32 0, i8 %type
  %fn = load %PrintFunction* %p_fn  ; XXX: assert(%p_fn==null)
  call void %fn (%Object* %p_obj)  
  ret void
}

;define void @main() {
;  %p_obj = alloca %Object
;  %p_type = getelementptr %Object* %p_obj, i32 0, i32 0
;  store i8 _OBJ_TYPE, i8* %p_type
;  call void @pprint(%Object* %p_obj)
;  ret void
;}
