;;;; SLOCL: small lisp on common lisp
(defpackage slocl
  (:use :common-lisp)
  (:shadow :common-lisp read eval)
  (:export read eval))
(in-package :slocl)

(defstruct @number value)
(defstruct @symbol value)
(defstruct @char value)
(defstruct @quote value)
(defstruct @list value)
(defstruct @array value)
(defstruct @string value)
  
(defun get-value (obj)
  (slot-value obj 'value))

(defmethod print-object ((o @number) stream)
  (format stream "~A" (get-value o)))

(defmethod print-object ((o @symbol) stream)
  (format stream "~A" (get-value o)))

(defmethod print-object ((o @char) stream)
  (format stream "$~A" (get-value o)))

(defmethod print-object ((o @quote) stream)
  (format stream "'~A" (get-value o)))

(defmethod print-object ((o @list) stream)
  (format stream "(~{~A~^ ~})" (get-value o)))

(defmethod print-object ((o @array) stream)
  (format stream "[~{~A~^ ~}]" (coerce (get-value o) 'list)))

(defmethod print-object ((o @string) stream)
  (format stream "~S" (map 'string #'get-value (get-value o))))

(defun space-char-p (c)
  (find c '(#\Space #\Tab #\Newline #\Return)))

(defun delim-char-p (c)
  (or (space-char-p c)
      (find c ")]")))

(defun read-until (in end-char-p)
  (coerce
   (loop FOR c = (peek-char nil in nil nil)
         WHILE (and c (not (funcall end-char-p c)))
         COLLECT (read-char in))
   'string))

(defun eat-spaces (in)
  (loop FOR c = (peek-char nil in nil nil)
        WHILE (and c (space-char-p c))
    DO (read-char in)))

(defun @read-number (in)
  (let ((v (read-from-string (read-until in #'delim-char-p))))
    (check-type v number)
    (make-@number :value v)))

(defun @read-symbol (in)
  (make-@symbol :value (intern (string-upcase (read-until in #'delim-char-p))
                               :keyword)))

(defun @read-string (in)
  (read-char in)
  (prog1 (make-@string 
          :value (map 'vector (lambda (x) (make-@char :value x))
                      (read-until in (lambda (c) (char= c #\")))))
    (read-char in)))

(defun @read-char (in)
  (read-char in nil nil)
  (make-@char :value (read-char in)))
   
(defun @read-quote (in)
  (read-char in nil nil)
  (make-@quote :value (read in)))

(defun @read-list (in)
  (read-char in nil nil)
  (make-@list 
   :value
   (loop FOR c = (progn (eat-spaces in) (peek-char nil in nil nil))
         UNTIL (char= c #\))
     COLLECT (read in)
     FINALLY (read-char in nil nil))))

(defun @read-array (in)
  (read-char in nil nil)
  (make-@array
   :value
   (loop FOR c = (progn (eat-spaces in) (peek-char nil in nil nil))
         UNTIL (char= c #\])
         COLLECT (read in) INTO elems
         FINALLY (read-char in nil nil)
                 (return (coerce elems 'vector)))))

(defun read (&optional (in *standard-input*))
  (eat-spaces in)
  (let ((c (peek-char nil in)))
    (cond ((digit-char-p c) (@read-number in))
          ((char= c #\")    (@read-string in))
          ((char= c #\$)    (@read-char in))
          ((char= c #\')    (@read-quote in))
          ((char= c #\()    (@read-list in))
          ((char= c #\[)    (@read-array in))
          (t                (@read-symbol in)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter +nil+ (make-@symbol :value :nil))
(defparameter +t+ (make-@symbol :value :t))
(defparameter +lambda+ (make-@symbol :value :lambda))
(defparameter +macro+ (make-@symbol :value :macro))
(defparameter +progn+ (make-@symbol :value :progn))
(defparameter +quote+ (make-@symbol :value :quote))
(defparameter +define+ (make-@symbol :value :define))
(defparameter +if+ (make-@symbol :value :if))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct env bindings)

(defun self-reference (symbol)
  (cons (get-value symbol) symbol))

(defun self-references (&rest symbols)
  (mapcar #'self-reference symbols))

(defun init-env ()
  (make-env :bindings (self-references +nil+ +t+
                                       +lambda+ +macro+
                                       +progn+ +define+
                                       +if+ +quote+)))

(defun local-binded-env (bindings env)
  (make-env :bindings (append bindings (env-bindings env))))

(defparameter *env* (init-env))

(defun get-symbol-value (symbol env)
  (cdr (or (assoc (get-value symbol) (env-bindings env))
           (error "未束縛のシンボル'~A'が参照されました。" symbol))))

(defun eval (exp &optional (env *env*) (quote nil))
  (if quote
      exp
    (typecase exp
      (@list (@eval-list exp env))
      (@quote (eval (get-value exp) env t))
      (@symbol (get-symbol-value exp env))
      (otherwise exp))))  ;; TODO: check-type

(defstruct @function params body)
(defstruct @macro params body)

(defun @apply (fn args-exp env)
  (with-slots (params body) fn
    (assert (= (length params) (length args-exp)))
    (eval body
          (local-binded-env
           (loop FOR param IN params
                 FOR arg-exp IN args-exp
                 COLLECT (cons (get-value param) (eval arg-exp env)))
           env))))

(defun @macroexpand (macro args-exp env)
  (@apply macro args-exp env))

(defun implicit-progn (body-exp)
  (make-@list :value (cons +progn+ body-exp)))

(defun @lambda (lambda-exp env)
  (declare (ignore env))
  (destructuring-bind (params-exp . body-exp) lambda-exp
    (check-type params-exp @list)
    (let ((params (get-value params-exp)))
      (assert (every #'@symbol-p params))
      (make-@function :params params
                      :body (implicit-progn body-exp)))))

(defun @macro (macro-exp env)
  (with-slots (params body) (@lambda macro-exp env)
    (make-@macro :params params :body body)))

(defun @quote (exp env)
  (declare (ignore env))
  (assert (= 1 (length exp)))
  (car exp))

(defun @if (if-exp env)
  (destructuring-bind (cond-exp then-exp &optional else-exp) if-exp
    (if (not (eq (eval cond-exp env) +NIL+))
        (eval then-exp env)
      (and else-exp (eval else-exp env)))))

(defun @progn (progn-exp env)
  (loop FOR e IN progn-exp
        FOR v = (eval e env)
    FINALLY (return v)))

(defun @define (define-exp env)
  (destructuring-bind (sym value-exp) define-exp
    (check-type sym @symbol)
    (push (cons (get-value sym) (eval value-exp env))
          (env-bindings env))
    sym))

(defun @eval-list (exp env)
  (destructuring-bind (head-exp . tail-exp) (get-value exp)
    (let ((head (eval head-exp env)))
      (typecase head
        (@function (@apply head tail-exp env))
        (@macro    (eval (@macroexpand head tail-exp env)))
        (@symbol
         (ecase (get-value head)
           (:lambda (@lambda tail-exp env))
           (:macro  (@macro tail-exp env))
           (:define (@define tail-exp env))
           (:quote  (@quote tail-exp env))
           (:if     (@if tail-exp env))
           (:progn  (@progn tail-exp env))))))))
