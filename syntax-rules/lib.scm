;;; CK machine
(define-syntax ck
  (syntax-rules (quote)
    ((_ () 'v) 'v)
    ((_ (((op ...) ea ...) . s) 'v)
     (ck s "arg" (op ... 'v) ea ...))
    ((_ s "arg" (op va ...))
     (op s va ...))
    ((_ s "arg" (op ...) 'v ea1 ...)
     (ck s "arg" (op ... 'v) ea1 ...))
    ((_ s "arg" (op ...) ea ea1 ...)
     (ck (((op ...) ea1 ...) . s) ea))
    ((_ s (op ea ...))
     (ck s "arg" (op) ea ...))))

;;; Binary Lookup Table
(define-syntax lookup!
  ;; (lookup! addr table) => table[addr]
  ;; note that $addr is little endian
  ((_ s '() d) (ck s d))
  ((_ s '(0 . r) '(d . _)) (ck s (lookup! 'r 'd)))
  ((_ s '(1 . r) '(_ . d)) (ck s (lookup! 'r 'd))))

;;; Arighmetic Functions
(define-syntax inc!
  ;; (inc! x) => x + 1
  (syntax-rules (quote)
    ((_ s '() '(y ...))
     (ck s '(y ... 1)))
    ((_ s '(0 x ...) '(y ...))
     (ck s '(y ... 1 x ...)))
    ((_ s '(1 x ...) '(y ...))
     (ck s (inc! '(x ...) '(y ... 0))))))

(define-syntax dec!
  ;; (inc! x) => x - 1
  (syntax-rules (quote)
    ((_ s '() _)
     (syntax-error "error: undeflow"))
    ((_ s '(0 x ...) '(y ...))
     (ck s (dec! '(x ...) '(y ... 1))))
    ((_ s '(1 x ...) '(y ...))
     (ck s '(x ... 0 x ...)))))

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
