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
    ((_ s '(v _ _ _ _ _)  '"A") (ck s 'v))
    ((_ s '(_ v _ _ _ _)  '"B") (ck s 'v))
    ((_ s '(_ _ v _ _ _)  '"C") (ck s 'v))
    ((_ s '(_ _ _ v _ _)  '"D") (ck s 'v))
    ((_ s '(_ _ _ _ v _) '"SP") (ck s 'v))
    ((_ s '(_ _ _ _ _ v) '"BP") (ck s 'v))))

(define-syntax eval-ir!
  (syntax-rules (quote)
    ((_ s reg '("REG" r)) (ck s (eval-r! reg 'r)))
    ((_ s reg '("IMM" i)) (ck s 'i))))

(define-syntax update-reg!
  (syntax-rules (quote)
    ((_ s '(_ b c d sp bp) '"A"  'v) (ck s '(v b c d sp bp)))
    ((_ s '(a _ c d sp bp) '"B"  'v) (ck s '(a v c d sp bp)))
    ((_ s '(a b _ d sp bp) '"C"  'v) (ck s '(a b v d sp bp)))
    ((_ s '(a b c _ sp bp) '"D"  'v) (ck s '(a b c v sp bp)))
    ((_ s '(a b c d _  bp) '"SP" 'v) (ck s '(a b c d v  bp)))
    ((_ s '(a b c d sp _ ) '"BP" 'v) (ck s '(a b c d sp v )))))

(define-syntax write!
  (syntax-rules (quote)
    ((_ s '(o ...) 'v) (ck s '(o ... v)))))

(define-syntax peek!
  (syntax-rules (quote)
    ((_ s '(x _ ...)) (ck s 'x))))

(define-syntax pop!
  (syntax-rules (quote)
    ((_ s '(_ x ...)) (ck s '(x ...)))))

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
		    (update! (num-to-addr (eval-ir! reg 'dst))
			     (eval-r! reg 'src) dmem)
		    imem i o)))

    ((_ s '"exec" '(("PUTC" src) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc reg dmem imem i
		    (write! o (eval-ir! reg 'src)))))

    ((_ s '"exec" '(("GETC" dst) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc
		    (update-reg! reg 'dst (peek! i)) dmem imem
		    (pop! i) o)))

    ((_ s '"exec" '(("EXIT" jmp) . _) _ _ _ _ _ o)
     ;; When the vm reach EXIT, it stops execution.
     ;; Therefore it does not call ck.
     (emit! o))

    ((_ s '"exec" '(("JCOND" op jmp dst src) . rest) pc reg dmem imem i o)
     (ck s (if! (cmp! 'op (eval-ir! reg 'src) (eval-r! reg 'dst))
		(run-vm! '"load" (eval-ir! reg 'jmp) reg dmem imem i o)
		(run-vm! '"exec" 'rest pc reg dmem imem i o))))

    ((_ s '"exec" '(("JMP" jmp) . _) pc reg dmem imem i o)
     (ck s (run-vm! '"load" (eval-ir! reg 'jmp) reg dmem imem i o)))

    ((_ s '"exec" '(("CMP" op dst src) . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest
		    pc
		    (update-reg! reg 'dst (cmp! 'op (eval-r! reg 'dst)
						(eval-ir! reg 'src)))
		    dmem imem i o)))

    ((_ s '"exec" '(("DUMP") . rest) pc reg dmem imem i o)
     (ck s (run-vm! '"exec" 'rest pc reg dmem imem i o)))))

;;; Emitter
(define fold (fn l i)
  (cond ((null? l) i)
	(else (fold fn (cdr l) (fn (car l) i)))))

(define blist->num (blist)
  (fold (lambda (x i) (+ x (* 2 i))) (reverse blist)))

(define-syntax emit!
  (syntax-rules (quote)
    ((_ _ o)
     (for-each (lambda (blist) (write (integer->char
				       (blist->num blist))))
	       o))))
