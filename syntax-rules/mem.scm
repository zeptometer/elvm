;;; Word utilities
(define-syntax num-to-big-endian-word!
  ;; Coerce little-endian number to bid-endian word
  ;; (for num-to-addr and num-to-byte)
  (syntax-rules (quote)
    ((_ _ x '()) (ck s x))
    ((_ '(i x ...) '(y ...) '(0 z ...))
     (ck s (num-to-word! '(x ...) '(i y ...) '(z ...))))
    ((_ '() '(y ...) '(0 z ...))
     (ck s (num-to-word! '() '(0 y ...) '(z ...))))))

(define-syntax num-to-addr!
  (syntax-rules (quote)
    ((_ s x) (ck s (num-to-big-endian-word! x '()
                                            '(0 0 0 0 0 0 0 0
                                              0 0 0 0 0 0 0 0
                                              0 0 0 0 0 0 0 0))))))

(define-syntax num-to-byte!
  (syntax-rules (quote)
    ((_ s x) (ck s (num-to-big-endian-word! x '() '(0 0 0 0 0 0 0 0))))))

;;; Memory Utilities
;;
;; Memory is represented as a binary tree. The address space is 24-bit,
;; and an address is a 24-bit big-endian 01-list.
;; To reduce memory consumption, empty binary tree can be represented
;; just as an empty list.

;;; Binary Lookup Table
;; Constructor
(define-syntax cons!
  (syntax-rules (quote)
    ((_ s 'x 'y) (ck s '(x . y)))))

(define-syntax lookup!
  ;; (lookup! addr table) => table[addr]
  ;; note that $addr is little endian
  (syntax-rules (quote)
    ((_ s '() d) (ck s d))
    ((_ s '(0 . r) '(d . _)) (ck s (lookup! 'r 'd)))
    ((_ s '(1 . r) '(_ . d)) (ck s (lookup! 'r 'd)))
    ;; when memory is empty, return 0 (= '())
    ((_ s _ '()) (ck s '()))))

(define-syntax update!
  ;; (update! addr val table) => table[addr] := val
  (syntax-rules (quote)
    ((_ s '() val _) (ck s val))
    ((_ s '(0 . a) val '(r . l)) (ck s (cons! (update! 'a val r) l)))
    ((_ s '(1 . a) val '(r . l)) (ck s r (cons! (update! 'a val l))))
    ;; when memory is empty, create a binary tree 
    ((_ s '(0 . a) val '()) (ck s (cons! (update! 'a val '()) '())))
    ((_ s '(1 . a) val '()) (ck s '() (cons! (update! 'a val '()))))))
