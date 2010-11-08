;;@list.NULL = constant %CONS* null
@list.NULL = alias %CONS* null
@list.TYPE = constant i8 0

%TYPE = type i8
%ANY  = type opaque
%CAR  = type {%TYPE, %ANY*}
%CDR  = type %CONS*
%CONS = type {%CAR, %CDR}
%LIST = type %CONS*

declare i8* @malloc(i32)
declare i32 @llvm.objectsize.i32(i8*, i1)

define %CAR @list.car(%LIST %list) {
  %car_p = getelementptr %LIST %list, i32 0, i32 0
  %car = load %CAR* %car_p
  ret %CAR %car
}

define %CDR @list.cdr(%LIST %list) {
  %cdr_p = getelementptr %LIST %list, i32 0, i32 1
  %cdr = load %CDR* %cdr_p
  ret %CDR %cdr
}

define %LIST @list.cons(%CAR %obj, %LIST %list) {
  %size = ptrtoint %CONS* getelementptr(%CONS* null, i32 1) to i32

  %opaq = call i8* @malloc(i32 %size)
  %cons = bitcast i8* %opaq to %CONS*

  %car_p = getelementptr %CONS* %cons, i32 0, i32 0
  %cdr_p = getelementptr %CONS* %cons, i32 0, i32 1

  store %CAR %obj, %CAR* %car_p
  store %CDR %list, %CDR* %cdr_p

  ret %LIST %cons
}

define i32 @main() {
   ret i32 0
}