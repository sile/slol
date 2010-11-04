;;;; ファイル名: decode.lisp
;;;; 
;;;; - LLVMのビットコードをデコード(パース)する: read-bitcode関数

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bit-stream.lispをロード
(load "bit-stream")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; デバッグ用変数 と 関数
(defvar *depth* 0)          ; 階層のネスト数
(defvar *show-progress* nil)  ; 進捗(デバッグ表示)を出力するかどうか

;; 進捗表示
(defun progress (fmt &rest args)
  (when *show-progress*
    (format *error-output* "~V@T; ~?~%" (* *depth* 2) fmt args)))

;;;;;;;;;;;;;;;;
;;; デコード処理

;; ビットコードを読み込む
;;   # 8byte x 4のマジックナンバー: read-magic-numbers関数
;;   # ビットストリーム本体:        read-bit-stream関数
(defun read-bitcode (file &key show-progress)
  (let ((*depth* 0)
        (*show-progress* show-progress))
    (with-input-bit-stream (in file)
      `((:magic-numbers ,(read-magic-numbers in))
        ,@(read-bit-stream in 2 '() (make-hash-table))))))

;; ストリームの先頭のマジックナンバーを読み込む
;;   # 最初の二バイトは0x42('B')と0x43('C')に固定。
;;   # 次の二バイトはアプリケーション定義の値。LLVM-IRなら0xC0と0xDE。
(defun read-magic-numbers (in)
  (let ((nums (loop REPEAT 4 COLLECT (read-fixed-width-int in 8))))
    (progress "magic numbers: ~S" nums)
    nums))

;; ビットストリームを読み込む
;; - ビットストリームは大まかには'ブロック'と'レコード定義'、'レコードデータ'から成る(この三つのシーケンス)
;;
;; 読み込み手順:
;;  1] 次のデータの種類を判定するためのID(abbreviation-ID)を読み込む
;;  2-a] ID値が0〜3の場合は、特別な処理を行う
;;     - END_BLOCK(0): 現在のブロックを抜ける。
;;     - ENTER_SUBBLOCK(1): サブブロックに入る。
;;     - DEFINE_ABBREV(2): abbreviationを定義する。
;;                         個人的にはレコードの型定義と解釈。フィールド数と各フィールドの読み込み方法を指定する。
;;                         定義されたブロック内でのみ使用可能(BLOCK_INFOで定義されたものは別)
;;     - UNABBREV_RECORD(3): 未定義(非省略)のレコードを読み込む。
;;                           定義済みのものとは異なり、レコード識別子とフィールドの数を有し、各フィールドのデータは可変長整数としてエンコードされている
;;                           ※ 定義済みのものは、フィールドのデータのみが、その型に即した形式でエンコードされている
;;  2-b] ID値が4以上の場合は、そのIDに該当する定義済みのレコードを読み込む
;;       ※ レコード定義のIDは、定義された順に自動で、4から昇順の値が振られていく
(defun read-bit-stream (in abbr-width abbrs global-abbrs)
  (flet ((read-next (&optional new-abbr)
           (if (null new-abbr)
               (read-bit-stream in abbr-width abbrs global-abbrs)
             (read-bit-stream in abbr-width (cons new-abbr abbrs) global-abbrs))))
    (let ((abbr-id (read-fixed-width-int in abbr-width)))
      (case abbr-id
        (0 ; BUILT-IN: END_BLOCK
         (read-end-block in))
        (1 ; BUILT-IN: ENTER_SUBBLOCK
         `(,(read-enter-subblock in global-abbrs) ,@(read-next)))
        (2 ; BUILT-IN: DEFIN_ABBREV
         (let ((new-abbr (read-define-abbrev in)))
           `((:define :abbrev ,new-abbr) ,@(read-next new-abbr))))
        (3 ; BUILT-IN: UNABBREV_RECORD
         `((:record :unabbrev ,(read-unabbrev-record in)) ,@(read-next)))
        (otherwise ; Normal abbreviation ID (It is defined by the stream itself)
         `((:record :abbrev ,(read-abbrev-record in abbr-id abbrs)) ,@(read-next)))))))

;; END_BLOCK
;; ブロック終端: 32bit境界に揃える
(defun read-end-block (in)
  (progress "end-block")
  (read-fixed-width-int in 0 :align 32)
  '())

;; UNABBREV_RECORD
;; 未定義(非省略)レコードの読み込み
(defun read-unabbrev-record (in)
  (progress "unabbrev-record:")
  (let* ((code  (read-variable-width-int in 6))  ; レコードの種類を判別するための(?)識別子(コード値)
         (numop (read-variable-width-int in 6))  ; オペランドの数
         (operands (loop REPEAT numop COLLECT (read-variable-width-int in 6))))
    (progress "  CODE=~S, OPERANDS={~{~S~^ ~}}" code operands)
    `(:code ,code :operands ,operands)))

;; 補助関数:
;; BLOCKINFOブロックにて、あらかじめ定義されたblock-idブロック用のレコード定義群を取得する
;; ※ マクロにしているのは、単に汎変数(setf ... )の定義を省くため
(defmacro get-block-abbrevs (block-id global-abbrs)
  `(gethash ,block-id ,global-abbrs))

;; ENTER_SUBBLOCK
;; サブブロックに入る。
;;
;; 最初にサブブロックのIDを取得するが、
;; それが0〜7の場合はあらかじめ予約されたstandard blockとなり、
;; 特別に処理される(?)。
;; ※ 現在はID値が0のBLOCKINFOブロックのみが定義されている
;; 
;; 8以上はアプリケーション定義のブロックとなる。
(defun read-enter-subblock (in global-abbrs)
  (let ((block-id   (read-variable-width-int in 8))           ; ブロックID
        (abbr-width (read-variable-width-int in 4))           ; read-bit-stream関数でabbr-idを取得する際に用いるビット幅
        (block-size (read-fixed-width-int in 32 :align 32)))  ; ブロックのサイズ
    (progress "enter-subblock: ID=~S, SIZE=~S" block-id block-size)
    (let ((*depth* (1+ *depth*)))
      `(:block 
        (:id ,block-id :abbr-width ,abbr-width :block-size ,block-size)
        ,@(case block-id
            (0               ; standard block# BLOCKINFO block
             (progress "BLOCKINFO:")
             (read-blockinfo-block in abbr-width global-abbrs))
            ((1 2 3 4 5 6 7) ; standard blocks# reserved for future use
             (error "BLOCK-ID#~S is being reserved for standard blocks" block-id))
            (otherwise       ; application defined blocks#
             (read-bit-stream in abbr-width 
                                 (get-block-abbrevs block-id global-abbrs)
                                 global-abbrs)))))))

;; BLOCKINFOブロック
;; 
;; ストリームの読み込み方自体は、read-bit-stream関数と同様。
;; ただし、このブロックで読み込んだ情報は、他のブロックからも参照(利用)可能にする必要があるので、
;; その処理を行うために、専用の関数を用意している。
(defun read-blockinfo-block (in abbr-width global-abbrs &optional focused-block)
  (let ((abbr-id (read-fixed-width-int in abbr-width)))
    (ecase abbr-id
      (0 ; BUILD-IN: END_BLOCK
       (read-end-block in))
      (2 ; BUIlD-IN: DEFINE_ABBREV
       (assert (numberp focused-block))
       (let ((new-abbr (read-define-abbrev in)))
         ;; 他のブロックから参照可能なように、定義済みレコードをglobal-abbrsに保存する
         (push new-abbr (get-block-abbrevs focused-block global-abbrs))
         `((:define :abbrev ,new-abbr) 
           ,@(read-blockinfo-block in abbr-width global-abbrs focused-block))))
      (3 ; BUILD-IN: UNABBREV_RECORD
       (let ((rc (read-unabbrev-record in))) ; レコードを読み込む。BLOCKINFOではレコードはただのデータではなく特別な意味を持つ。
         (ecase (getf rc :code)
           (1 ; SETBID: 
              ;; 後続のシーケンスで対象となるブロックを設定する (どのブロック用のレコードを定義しているのか、などを知るため)
              ;; BLOCKINFOブロックでは、まず初めにこのレコードを読み込む必要がある
              ;; 一つのBLOCKINFOブロックに複数回出現しても良い
            (setf focused-block (first (getf rc :operands)))
            (progress "SWITCH FOCUS: BLOCK#~S" focused-block))
           (2 #| BLOCKNAME |#)       ; ブロックの名前を設定する。オプショナル。自分が試したビットコードでは不使用だったので省略。
           (3 #| SETRECORDNAME |#))  ; レコードの名前を設定する。オプショナル。自分が試したビットコードでは不使用だったので省略。
         `((:record :unabbrev ,rc)
           ,@(read-blockinfo-block in abbr-width global-abbrs focused-block)))))))

;; 補助関数:
;; 進捗表示に含まれるオペランド(フィールド)の型情報を若干見やすくする。
(defun format-types (operand-types)
  (mapcar (lambda (type)
            (if (eq (first type) :literal)
                (second type)
              (case (first #1=(second type))
                ((:fixed :vbr) `(,(first #1#) ,(third #1#)))
                (otherwise     (first #1#)))))
          operand-types))

;; DEFINE_ABBREV
;; レコード(の省略)定義を読み込む
(defun read-define-abbrev (in)
  (progress "define-abbrev:")
  (let* ((numops (read-variable-width-int in 5))  ; オペランドの数
         (operand-types (loop REPEAT numops COLLECT (read-operand-type in))))
    (progress "  operand-types: {~{~S~^ ~}}" (format-types operand-types))
    `(:operand-types ,operand-types)))

;; レコードのオペランドの型情報を読み込む
(defun read-operand-type (in)
  (if (= 1 (read-bit in))
      `(:literal ,(read-variable-width-int in 8)) ; リテラル(定数値)
    (let ((code (read-fixed-width-int in 3)))
      `(:enc
        ,(ecase code
           (1 `(:fixed :width ,(read-variable-width-int in 5))) ; fixed-width int
           (2 `(:vbr   :width ,(read-variable-width-int in 5))) ; variable-width int
           (3 `(:array))     ; array. この次には配列の要素の型定義が続く (:arrayはその次の型定義と二つで一セット)
           (4 `(:char6))     ; 6bit-character
           (5 `(:blob))))))) ; blob.

;; 定義済みレコードを読み込む
(defun read-abbrev-record (in abbr-id abbrevs)
  (let ((abbr (get-abbr abbr-id abbrevs)))             ; レコード定義を取得する
    (progress "abbrev-record: ABBREV-ID=~S" abbr-id)
    (let ((operands (read-operands in (second abbr)))) ; 型情報に従ってデータを読み込む
      (progress "  operands: {~{~S~^ ~}}" operands)
      `(:abbr-id ,abbr-id :operands ,operands))))

;; IDに対応するレコード定義を取得する
;; - レコードのIDは4から昇順に、定義された順番に振られる
;; - abbrevsにはレコード定義が逆順に格納されているので、後ろから(- id 4)番目が対応する定義となる
(defun get-abbr (id abbrevs &aux (len (length abbrevs)))
  (nth (- len 1 (- id 4)) abbrevs))

;; 各フィールドのデータを読み込む
(defun read-operands (in types)
  (when types
    (destructuring-bind ((flag datum) . rest) types
      `(,(if (eq flag :literal)
             ; リテラル値: ストリームからの読み込みは不要
             datum     
           ; エンコードされているデータ         
           (ecase (first datum)
             (:fixed (read-fixed-width-int in (third datum)))
             (:vbr   (read-variable-width-int in (third datum)))
             (:array (let ((ary-len (read-variable-width-int in 6))
                           (elem-type (list (first rest))))  ; 配列の場合、次の型情報が配列の要素の型を表す
                       (prog1 (loop REPEAT ary-len 
                                    APPEND (read-operands in elem-type)) ; サイズ分だけデータを読み込む
                         (setf rest (cdr rest)))))
             (:char6 (read-6bit-char in))
             (:blob (let ((len (read-variable-width-int in 6)))
                      (prog2 ;; XXX: 実際のデータで試していないので、この処理であっているかは不明
                          (read-fixed-width-int in 0 :align 32)
                          (loop REPEAT len COLLECT (read-fixed-width-int in 8))
                          (read-fixed-width-int in 0 :align 32))))))
        ,@(read-operands in rest)))))

