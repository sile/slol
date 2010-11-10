;;;; ファイル名: type.inc
;;;;
;;;; [説明]
;;;; - 共通の型定義を含むヘッダファイル
;;;; - TODO: global.incに併合する?

%ObjType = type i8
%Object  = type {%ObjType, opaque*}

%NumberObject = type {%ObjType, i32} 
%OperatorObject = type {%ObjType, i32}

%Car = type %Object*
%Cdr = type %Cons*
%Cons= type {%ObjType, %Car, %Cdr}
%ListObject = type %Cons*

%PrintFunction = type void (%Object*)*
%PrettyPrintDispatchTable = type [256 x %PrintFunction]
%PPDT = type %PrettyPrintDispatchTable

%Ctor = type {i32, void ()*}

define(`_OBJ_TYPE', 0)
define(`_NUM_TYPE', 1)
define(`_LIST_TYPE', 2)
define(`_OP_TYPE', 3)

define(`_String', `[$1 x i8]')
define(`_GetString', `getelementptr [$2 x i8]* $1, i32 0, i32 0')

define(`_SIZEOF', `ptrtoint ($1* getelementptr ($1* null, i32 1) to i32)')

define(`_OP_ADD', 0)
define(`_OP_SUB', 1)
define(`_OP_MUL', 2)
define(`_OP_DIV', 3)
