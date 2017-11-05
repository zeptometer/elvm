;;; Arighmetic Functions

;; Natural numbers are represented in little-endian 01 list
;; ex: 0  = ()
;;     4  = (0 0 1)
;;     13 = (1 0 1 1)

(define-syntax normalize!
  ;; remove trailing zeros
  ;; (normalize! '(1 0 0 0 )) => '(1)
  (syntax-rules (quote)
    ; optional argument
    ((_ s x)
     (ck s (normalize! x '())))
    ; main
    ((_ s (0 ...) x)
     (ck s x))
    ((_ s (i x ...) (y ...))
     (ck s (normalize! (x ...) (y ... i))))))

(define-syntax inc!
  ;; (inc! x y) => y ++ (x + 1)
  (syntax-rules (quote)
    ((_ s '() '(y ...))
     (ck s '(y ... 1)))
    ((_ s '(0 x ...) '(y ...))
     (ck s '(y ... 1 x ...)))
    ((_ s '(1 x ...) '(y ...))
     (ck s (inc! '(x ...) '(y ... 0))))))

(define-syntax dec!
  ;; (dec! x y) => y ++ (x - 1)
  (syntax-rules (quote)
    ((_ s '() _)
     (syntax-error "error: underflow"))
    ((_ s '(0 x ...) '(y ...))
     (ck s (dec! '(x ...) '(y ... 1))))
    ((_ s '(1 x ...) '(y ...))
     (ck s (normalize! '(y ... 0 x ...))))))

(define-syntax add!
  ;; (add! x y) => x + y
  (syntax-rules (quote)
    ;;; initialize
    ((_ s x y) (add! s x y '0 '()))
    ;;; corner case
    ((_ s '() '(y ...) '0 '(z ...))
     (ck s '(z ... y ...)))
    ((_ s '(x ...) '() '0 '(z ...))
     (ck s '(z ... x ...)))
    ((_ s '() 'ys '1 'zs)
     (ck s (inc! 'ys 'zs)))
    ((_ s 'xs '() '1 'zs)
     (ck s (inc! 'xs 'zs)))
    ;;; general case
    ((_ s '(0 . x) '(0 . y) '0 '(buf ...))
     (ck s (add! 'x 'y '0 '(buf ... 0))))
    ((_ s '(1 . x) '(0 . y) '0 '(buf ...))
     (ck s (add! 'x 'y '0 '(buf ... 1))))
    ((_ s '(0 . x) '(1 . y) '0 '(buf ...))
     (ck s (add! 'x 'y '0 '(buf ... 1))))
    ((_ s '(1 . x) '(1 . y) '0 '(buf ...))
     (ck s (add! 'x 'y '1 '(buf ... 0))))
    ((_ s '(0 . x) '(0 . y) '1 '(buf ...))
     (ck s (add! 'x 'y '0 '(buf ... 1))))
    ((_ s '(1 . x) '(0 . y) '1 '(buf ...))
     (ck s (add! 'x 'y '1 '(buf ... 0))))
    ((_ s '(0 . x) '(1 . y) '1 '(buf ...))
     (ck s (add! 'x 'y '1 '(buf ... 0))))
    ((_ s '(1 . x) '(1 . y) '1 '(buf ...))
     (ck s (add! 'x 'y '1 '(buf ... 1))))))

(define-syntax sub!
  ;; (add! x y) => x - y when x >= y
  ;;            |  error otherwise
  (syntax-rules (quote)
    ;; initialize
    ((_ s x y) (sub! s x y '0 '()))
    ;; corner case
    ((_ s '() '(y ...) _ _)
     (syntax-error "error: underflow"))
    ((_ s '(x ...) '() '0 '(z ...))
     (ck s '(z ... x ...)))
    ((_ s 'xs '() '1 'zs)
     (ck s (dec! 'xs 'zs)))
    ;; general case
    ((_ s '(0 . x) '(0 . y) '0 '(buf ...))
     (ck s (sub! 'x 'y '0 '(buf ... 0))))
    ((_ s '(1 . x) '(0 . y) '0 '(buf ...))
     (ck s (sub! 'x 'y '0 '(buf ... 1))))
    ((_ s '(0 . x) '(1 . y) '0 '(buf ...))
     (ck s (sub! 'x 'y '1 '(buf ... 1))))
    ((_ s '(1 . x) '(1 . y) '0 '(buf ...))
     (ck s (sub! 'x 'y '0 '(buf ... 0))))
    ((_ s '(0 . x) '(0 . y) '1 '(buf ...))
     (ck s (sub! 'x 'y '1 '(buf ... 1))))
    ((_ s '(1 . x) '(0 . y) '1 '(buf ...))
     (ck s (sub! 'x 'y '0 '(buf ... 0))))
    ((_ s '(0 . x) '(1 . y) '1 '(buf ...))
     (ck s (sub! 'x 'y '1 '(buf ... 0))))
    ((_ s '(1 . x) '(1 . y) '1 '(buf ...))
     (ck s (sub! 'x 'y '1 '(buf ... 1))))))

(define-syntax cmp%!
  ;; (cmp%! 'x 'y) => '((x <= y) (x < y))
  ;; assume that x and y are normalized
  (syntax-rules (quote)
    ;; optional arguemnt
    ((_ s x y)
     (ck s (cmp%! x y '"=")))
    ;; main
    ;; when x and y are of same width
    ((_ s '() '() '"=")
     (ck s '(1 0)))
    ((_ s '() '() '"<")
     (ck s '(1 1)))
    ((_ s '() '() '">")
     (ck s '(0 0)))
    ;; when different width
    ((_ s '() _ _)
     (ck s '(1 1)))
    ((_ s _ '() _)
     (ck s '( 0 0)))
    ;; recursion
    ((_ s '(0 x ...) '(0 y ...) c)
     (ck s (cmp%! '(x ...) '(y ...) c)))
    ((_ s '(1 x ...) '(1 y ...) c)
     (ck s (cmp%! '(x ...) '(y ...) c)))
    ((_ s '(0 x ...) '(1 y ...) _)
     (ck s (cmp%! '(x ...) '(y ...) '"<")))
    ((_ s '(1 x ...) '(0 y ...) _)
     (ck s (cmp%! '(x ...) '(y ...) '">")))))

(define-syntax cmp!
  ;; (cmp! op x y)
  ;; => 1 (= '(1)) when op x y satisfied
  ;;    0 (= '())  otherwise
  ;; op := "eq" "ne" "lt" "gt" "le" "ge"
  (syntax-rules (quote)
    ;; wrap
    ((_ s op x y)
     (ck s (cmp! op (cmp%! x y))))
    ;; main
    ((_ s '"eq" '(1 0)) (ck s '(1)))
    ((_ s '"ne" '(0 _)) (ck s '(1)))
    ((_ s '"ne" '(1 1)) (ck s '(1)))
    ((_ s '"lt" '(1 1)) (ck s '(1)))
    ((_ s '"le" '(1 0)) (ck s '(1)))
    ((_ s '"gt" '(0 0)) (ck s '(1)))
    ((_ s '"ge" '(_ 0)) (ck s '(1)))
    ((_ s _ _)          (ck s '()))))

(define-syntax if!
  ;; (if! bool then else)
  ;; => else when bool == 0
  ;;    then otherwise
  (syntax-rules (quote)
    ((_ s '() _ else) (ck s else))
    ((_ s _ then _)   (ck s then))))
