;;; CK machine
;;; For detail, see http://okmij.org/ftp/Scheme/macros.html#ck-macros
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

;;; Arighmetic Functions

;; Natural numbers are represented in little-endian 01 list
;; 0 stands for an infinite list (0 0 ...)
;; and 1 represents an infinite list (1 1 ...)
;; -x is defined as ~x + 1
;;
;; ex: 0  = 0
;;     4  = (0 0 1 . 0)
;;     13 = (1 0 1 1 . 0)
;;     -1 = 1
;;     -2 = (0 . 1)

(define-syntax flip!
  (syntax-rules (quote)
    ;; optional
    ((_ s x)
     (flip! s x '()))
    ;; main
    ((_ s '0 '(y ...))
     (ck s '(y ... . 1)))
    ((_ s '1 '(y ...))
     (ck s '(y ... . 0)))
    ((_ s '(0 . xs) '(y ...))
     (flip! s 'xs '(y ... 1)))
    ((_ s '(1 . xs) '(y ...))
     (flip! s 'xs '(y ... 0)))))

(define-syntax inc!
  ;; (inc! x)   => x + 1
  ;; (inc! x y) => y ++ (x + 1)
  (syntax-rules (quote)
    ;; optional
    ((_ s x)
     (inc! s x '()))
    ;; main
    ((_ s '0 '(y ...))
     (ck s '(y ... 1 . 0)))
    ((_ s '1 '(y ...))
     (ck s '(y ... . 0)))
    ((_ s '(0 . xs) '(y ...))
     (ck s '(y ... 1 . xs)))
    ((_ s '(1 . xs) '(y ...))
     (inc! s 'xs '(y ... 0)))))

(define-syntax dec!
  ;; (dec! x)   => x - 1
  ;; (dec! x y) => y ++ (x - 1)
  (syntax-rules (quote)
    ;; optional
    ((_ s x)
     (dec! s x '()))
    ;; main
    ((_ s '0 '(y ...))
     (ck s '(y ... . 1)))
    ((_ s '1 '(y ...))
     (ck s '(y ... 0 . 1)))
    ((_ s '(0 . xs) '(y ...))
     (dec! s 'xs '(y ... 1)))
    ((_ s '(1 . xs) '(y ...))
     (ck  s '(y ... 0 . xs)))))

(define-syntax add!
  ;; (add! x y) => x + y
  ;; (add! x y carry rest) => rest ++ (x + y + carry?1:0)
  (syntax-rules (quote)
    ;;; initialize
    ((_ s x y) (add! s x y '#f '()))
    ;;; corner case
    ((_ s '0 '0 '#f '(z ...))
     (ck s '(z ... . 0)))
    ((_ s '0 '0 '#t '(z ...))
     (ck s '(z ... 1 . 0)))
    ((_ s '1 '1 '#f '(z ...))
     (ck s '(z ... 0 . 1)))
    ((_ s '1 '1 '#t '(z ...))
     (ck s '(z ... . 1)))
    ((_ s '0 '1 '#f '(z ...))
     (ck s '(z ... . 1)))
    ((_ s '1 '0 '#f '(z ...))
     (ck s '(z ... . 1)))
    ((_ s '0 '1 '#t '(z ...))
     (ck s '(z ... . 0)))
    ((_ s '1 '0 '#t '(z ...))
     (ck s '(z ... . 0)))
    ((_ s '0 'ys '#f '(z ...))
     (ck s '(z ... . ys)))
    ((_ s 'xs '0 '#f '(z ...))
     (ck s '(z ... . xs)))
    ((_ s '0 'ys '#t 'zs)
     (inc! s 'ys 'zs))
    ((_ s 'xs '0 't 'zs)
     (inc! s 'xs 'zs))
    ((_ s '1 'ys '#f 'zs)
     (dec! s 'ys 'zs))
    ((_ s 'xs '1 '#f 'zs)
     (dec! s 'xs 'zs))
    ((_ s '1 'ys '#t '(z ...))
     (ck s '(z ... . ys)))
    ((_ s 'xs '1 '#t '(z ...))
     (ck s '(z ... . xs)))
    ;;; general case
    ((_ s '(0 . x) '(0 . y) '#f '(buf ...))
     (add! s 'x 'y '#f '(buf ... 0)))
    ((_ s '(1 . x) '(0 . y) '#f '(buf ...))
     (add! s 'x 'y '#f '(buf ... 1)))
    ((_ s '(0 . x) '(1 . y) '#f '(buf ...))
     (add! s 'x 'y '#f '(buf ... 1)))
    ((_ s '(1 . x) '(1 . y) '#f '(buf ...))
     (add! s 'x 'y '#t '(buf ... 0)))
    ((_ s '(0 . x) '(0 . y) '#t '(buf ...))
     (add! s 'x 'y '#f '(buf ... 1)))
    ((_ s '(1 . x) '(0 . y) '#t '(buf ...))
     (add! s 'x 'y '#t '(buf ... 0)))
    ((_ s '(0 . x) '(1 . y) '#t '(buf ...))
     (add! s 'x 'y '#t '(buf ... 0)))
    ((_ s '(1 . x) '(1 . y) '#t '(buf ...))
     (add! s 'x 'y '#t '(buf ... 1)))))

(define-syntax sub!
  (syntax-rules (quote)
    ((_ s x y)
     (ck s (add! x (inc! (flip! y)))))))

(define-syntax cmp%!
  ;; (cmp%! 'x 'y) => '((x <= y) (x < y))
  ;; assume that x and y are normalized
  (syntax-rules (quote)
    ;; optional arguemnt
    ((_ s x y)
     (cmp%! s x y '"="))
    ;; main
    ;; when x and y are of same width
    ((_ s '0 '0 '"=")
     (ck s '(1 0)))
    ((_ s '0 '0 '"<")
     (ck s '(1 1)))
    ((_ s '0 '0 '">")
     (ck s '(0 0)))
    ((_ s '1 '1 '"=")
     (ck s '(1 0)))
    ((_ s '1 '1 '"<")
     (ck s '(1 1)))
    ((_ s '1 '1 '">")
     (ck s '(0 0)))
    ((_ s '0 '1 _)
     (ck s '(0 0)))
    ((_ s '1 '0 _)
     (ck s '(1 1)))
    ;; when different width
    ((_ s '0 y f)
     (cmp%! s '(0 . 0) y f))
    ((_ s x '0 f)
     (cmp%! s x '(0 . 0) f))
    ((_ s '1 y f)
     (cmp%! s '(1 . 1) y f))
    ((_ s x '1 f)
     (cmp%! s x '(1 . 1) f))
    ;; recursion
    ((_ s '(0 . xs) '(0 . ys) c)
     (cmp%! s 'xs 'ys c))
    ((_ s '(1 . xs) '(1 . ys) c)
     (cmp%! s 'xs 'ys c))
    ((_ s '(0 . xs) '(1 . ys) _)
     (cmp%! s 'xs 'ys '"<"))
    ((_ s '(1 . xs) '(0 . ys) _)
     (cmp%! s 'xs 'ys '">"))))

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
    ((_ s '"EQ" '(1 0)) (ck s '(1 . 0)))
    ((_ s '"NE" '(0 _)) (ck s '(1 . 0)))
    ((_ s '"NE" '(1 1)) (ck s '(1 . 0)))
    ((_ s '"LT" '(_ 1)) (ck s '(1 . 0)))
    ((_ s '"LE" '(1 _)) (ck s '(1 . 0)))
    ((_ s '"GT" '(0 _)) (ck s '(1 . 0)))
    ((_ s '"GE" '(_ 0)) (ck s '(1 . 0)))
    ((_ s _ __)         (ck s '0))))

(define-syntax if!
  ;; (if! bool then else)
  ;; => else when bool == 0
  ;;    then otherwise
  (syntax-rules (quote)
    ((_ s '0 'then 'else) (ck s else))
    ((_ s _   'then 'else) (ck s then))))

;;; Word utilities
(define-syntax num-to-word!
  ;; Coerce little-endian number to big-endian word
  ;; (for num-to-addr and num-to-byte)
  (syntax-rules (quote)
    ((_ s _ x '()) (ck s x))
    ((_ s '(i . xs) '(y ...) '(0 z ...))
     (num-to-word! s 'xs '(i y ...) '(z ...)))
    ((_ s '0 '(y ...) '(0 z ...))
     (num-to-word! s '0 '(0 y ...) '(z ...)))
    ((_ s '1 '(y ...) '(0 z ...))
     (num-to-word! s '1 '(1 y ...) '(z ...)))))

(define-syntax num-to-addr!
  (syntax-rules (quote)
    ((_ s x) (num-to-word! s x '()
			   '(0 0 0 0 0 0 0 0
			     0 0 0 0 0 0 0 0
			     0 0 0 0 0 0 0 0)))))

(define-syntax num-to-byte!
  (syntax-rules (quote)
    ((_ s x) (num-to-word! s x '() '(0 0 0 0 0 0 0 0)))))

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
  ;; note that $addr is big endian
  (syntax-rules (quote)
    ((_ s '() d) (ck s d))
    ((_ s '(0 . r) '(d . _)) (lookup! s 'r 'd))
    ((_ s '(1 . r) '(_ . d)) (lookup! s 'r 'd))
    ;; when memory is empty, return 0 (= '())
    ((_ s _ '()) (ck s '()))))

(define-syntax update!
  ;; (update! addr val table) => table[addr] := val
  (syntax-rules (quote)
    ((_ s '() val _) (ck s val))
    ((_ s '(0 . a) val '(r . l)) (ck s (cons! (update! 'a val 'r) 'l)))
    ((_ s '(1 . a) val '(r . l)) (ck s (cons! 'r (update! 'a val 'l))))
    ;; when memory is empty, create a binary tree 
    ((_ s '(0 . a) val '()) (ck s (cons! (update! 'a val '()) '())))
    ((_ s '(1 . a) val '()) (ck s (cons! '() (update! 'a val '()))))))

;;; VM consists of:
;;; * execution state := "load" | "exec"
;;; * PC : number
;;; * six registers A, B, C, D, SP, and BP : number
;;; * data memory : binary tree of number
;;; * instruction memory : binary tree of instructions
;;; * input stack : stack of bytes
;;; * output stack : stack of bytes

;;; Instruction format
;;; Inst := ("MOV" dst src)
;;;      |  ("ADD" dst src)
;;;      |  ("SUB" dst src)
;;;      |  ("LOAD" dst src)
;;;      |  ("STORE" src dst)
;;;      |  ("PUTC" src)
;;;      |  ("GETC" dst)
;;;      |  ("EXIT")
;;;      |  ("JCOND" op jmp dst src)
;;;      |  ("JMP" jmp)
;;;      |  ("CMP" op dst src)

;;; Virtual Machine
(define-syntax eval-r!
  (syntax-rules (quote)
    ((_ s '(a b c d sp bp)  '"A") (ck s 'a))
    ((_ s '(a b c d sp bp)  '"B") (ck s 'b))
    ((_ s '(a b c d sp bp)  '"C") (ck s 'c))
    ((_ s '(a b c d sp bp)  '"D") (ck s 'd))
    ((_ s '(a b c d sp bp) '"SP") (ck s 'sp))
    ((_ s '(a b c d sp bp) '"BP") (ck s 'bp))))

(define-syntax eval-ir!
  (syntax-rules (quote)
    ((_ s reg '("REG" r)) (eval-r! s reg 'r))
    ((_ s reg '("IMM" i)) (ck s 'i))))

(define-syntax update-reg!
  (syntax-rules (quote)
    ((_ s '(a b c d sp bp) '"A"  'v) (ck s '(v b c d sp bp)))
    ((_ s '(a b c d sp bp) '"B"  'v) (ck s '(a v c d sp bp)))
    ((_ s '(a b c d sp bp) '"C"  'v) (ck s '(a b v d sp bp)))
    ((_ s '(a b c d sp bp) '"D"  'v) (ck s '(a b c v sp bp)))
    ((_ s '(a b c d sp bp) '"SP" 'v) (ck s '(a b c d v  bp)))
    ((_ s '(a b c d sp bp) '"BP" 'v) (ck s '(a b c d sp v )))))

(define-syntax write!
  (syntax-rules (quote)
    ((_ s '(o ...) 'v) (ck s '(o ... v)))))

(define-syntax peek!
  (syntax-rules (quote)
    ((_ s '(x _ ...)) (ck s 'x))
    ((_ s '()) (ck s '0))))

(define-syntax pop!
  (syntax-rules (quote)
    ((_ s '(_ x ...)) (ck s '(x ...)))
    ((_ s '()) (ck s '()))))

(define-syntax run-vm!
  (syntax-rules (quote)
    ;; load instruction
    ((_ s '"load" pc reg dmem imem i o)
     (ck s (run-vm! '"exec" (lookup! (num-to-addr! pc) imem)
		    pc reg dmem imem i o)))

    ;; execute instruction
    ((_ s '"exec" '() pc reg dmem imem i o)
     (ck s (run-vm! '"load"  (inc! pc) reg dmem imem i o)))

    ((_ s '"exec" '(("MOV" dst src) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc (update-reg! reg 'dst (eval-ir! reg 'src))
		    dmem imem i o)))

    ((_ s '"exec" '(("ADD" dst src) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc
		    (update-reg! reg 'dst (add! (eval-r! reg 'dst)
						(eval-ir! reg 'src)))
		    dmem imem i o)))

    ((_ s '"exec" '(("SUB" dst src) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc
		    (update-reg! reg 'dst (sub! (eval-r! reg 'dst)
						(eval-ir! reg 'src)))
		    dmem imem i o)))

    ((_ s '"exec" '(("LOAD" dst src) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc
		    (update-reg! reg 'dst
				 (lookup! (num-to-addr! (eval-ir! reg 'src))
					  dmem))
		    dmem imem i o)))

    ((_ s '"exec" '(("STORE" src dst) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc reg
		    (update! (num-to-addr! (eval-ir! reg 'dst))
			     (eval-r! reg 'src) dmem)
		    imem i o)))

    ((_ s '"exec" '(("PUTC" src) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc reg dmem imem i
		    (write! o (num-to-byte! (eval-ir! reg 'src))))))

    ((_ s '"exec" '(("GETC" dst) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc
		    (update-reg! reg 'dst (peek! i)) dmem imem
		    (pop! i) o)))

    ((_ s '"exec" '(("EXIT") . _) pc reg dmem imem i o)
     ;; When the vm reach EXIT, it stops execution.
     ;; Therefore it does not call ck.
     (emit! o))

    ((_ s '"exec" '(("JCOND" op jmp dst src) . rest) pc reg dmem imem i o)
     (ck s (if! (cmp! 'op (eval-ir! reg 'src) (eval-r! reg 'dst))
		'(run-vm! '"load" (eval-ir! reg 'jmp) reg dmem imem i o)
		'(run-vm! '"exec" 'rest pc reg dmem imem i o))))

    ((_ s '"exec" '(("JMP" jmp) . _) pc reg dmem imem i o)
     (ck s (run-vm! '"load" (eval-ir! reg 'jmp) reg dmem imem i o)))

    ((_ s '"exec" '(("CMP" op dst src) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc
		    (update-reg! reg 'dst (cmp! 'op (eval-r! reg 'dst)
						(eval-ir! reg 'src)))
		    dmem imem i o)))))

;;; Emitter
(define (fold fn l i)
  (if (null? l)
      i
      (fold fn (cdr l) (fn (car l) i))))

(define (blist->num blist)
  (fold (lambda (x i) (+ x (* 2 i))) blist 0))

(define-syntax emit!
  (syntax-rules (quote)
    ((_ o)
     (for-each (lambda (blist) (write-char (integer->char
					    (blist->num blist))))
	       o))))

;; (display (ck () (add! '(1 1 . 0) '(0 0 1 . 0))))
;; (newline)
;; (display (ck () (add! '1 '(0 0 1 . 0))))
;; (newline)
;; (display (ck () (sub! '(1 1 . 0) '(0 0 1 . 0))))
;; (newline)

