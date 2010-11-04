;;;; ファイル名: bit-stream.lisp
;;;;
;;;; - ビットストリームの読み込み方法には三種類ある
;;;;  1] ビット幅が固定の整数値の読み込み: read-fixed-width-int関数
;;;;  2] ビット幅が可変の整数値の読み込み: read-variable-width-int関数
;;;;     Nビットずつ読み込み、最上位ビットが1か0かで終端を判断する。0なら終端。
;;;;  3] 6bit文字の読み込み: read-6bit-char関数
;;;;
;;;; - ビットは下位のものから順番に読み込んでいく

;;;;;;;;;;
;;; 型定義
(deftype octet () '(unsigned-byte 8))                  ; 一バイト
(deftype octet-bit-position () '(integer 0 8))         ; 一バイトを表現するのに必要なビット数+1
(deftype bit-position () `(mod ,most-positive-fixnum)) ; ビットの位置   XXX: 上限はテキトウ

;;;;;;;;;;;;;;;;;;;;
;;; ビットストリーム
(defstruct (bit-stream (:constructor make-bit-stream (octet-stream)))
  (octet-stream nil :type stream)              ; 読み込み元となるバイトストリーム
  (octet          0 :type octet)               ; バッファ (1bitずつ読み込むことは出来ないので、まず一バイト取得し、そこから1bitずつ取り出していく)
  (pos            8 :type octet-bit-position)  ; octet内での現在位置
  (acc-pos        0 :type bit-position))       ; 読み込んだビット数 (ストリームの始点を基準とした現在位置)

(defmacro with-input-bit-stream ((in file) &body body)
  `(with-open-file (,in ,file :element-type 'octet)
     (let ((,in (make-bit-stream ,in)))
       ,@body)))

;; 1bit読み込む
(defun read-bit (in)
  (with-slots (octet-stream octet pos acc-pos) (the bit-stream in)
    (when (= pos 8)
      (setf octet (read-byte octet-stream nil 0)  ; ストリームの終端に達した場合は0を返す
            pos   0))
    (prog1 
        (ldb (byte 1 pos) octet)
      (incf acc-pos)
      (incf pos))))

;; ビット幅が固定の整数値を読み込む
(defun read-fixed-width-int (in width &key (align 1))
  (with-slots (acc-pos) (the bit-stream in)
    ;; 読み込み開始位置をalignビットの境界に合わせる   TODO: alignmentは別関数に分けた方が良い
    (loop WHILE (not (zerop (mod acc-pos align))) 
          DO (read-bit in))
    
    ;; 整数値読み込み
    (loop FOR i FROM 0 BELOW width
          SUM (ash (read-bit in) i))))

;; ビット幅が可変(不定)の整数値の読み込み
(defun read-variable-width-int (in width &aux (data-width (1- width)))
  (loop WITH int = 0
        FOR i FROM 0 
        FOR n = (read-fixed-width-int in data-width)  ; 下位の(1- width)ビットが整数値用のデータ
        FOR last? = (zerop (read-bit in))             ; 上位1bitはフラグビット
    DO
      (incf int (ash n (* data-width i)))
      (when last?
        (return int))))

;; 6bit文字の読み込み
;; - 英数字と'.'及び'_'が表現可能
(defun read-6bit-char (in)
  (let ((code (read-fixed-width-int in 6)))
    (cond ((<= 00 code 25) (char+ #\a code))
          ((<= 16 code 51) (char+ #\A code))
          ((<= 52 code 61) (char+ #\0 code))
          (t (case code
               (62 #\.)
               (63 #\_))))))

(defun char+ (base-char delta)
  (code-char (+ (char-code base-char) delta)))

