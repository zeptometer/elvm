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
