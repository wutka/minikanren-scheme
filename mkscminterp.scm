(load "mk.scm")

(define lookupo
  (lambda (x env out)
    (fresh (y val env^)
	   (== `((,y . ,val) . ,env^) env)
	   (conde
	    [(== x y) (== val out)]
	    [(=/= x y) (lookupo x env^ out)]))))

(define bind-let
  (lambda (bindings env envout)
    (conde
     [(== bindings '()) (== env envout)]
     [(fresh (a d aa ad newenv expr val)
	     (== (cons a d) bindings)
	     (== (cons aa ad) a)
	     (== (cons expr '()) ad)
	     (eval-expo expr env val)
	     (bind-let d env newenv)
	     (== `((,aa . ,val) . ,newenv) envout))]
     )))

(define bind-lambda
  (lambda (vars vals evalenv lambdaenv envout)
    (conde
     [(== vars '()) (== lambdaenv envout)]
     [(fresh (ra rd la ld expr val newenv)
	     (== (cons ra rd) vars)
	     (== (cons la ld) vals)
	     (eval-expo la evalenv val)
	     (bind-lambda rd ld evalenv lambdaenv newenv)
	     (== `((,ra . ,val) . ,newenv) envout))]
     )))

(define eval-expo
  (lambda (expr env out)
    (conde
     [(== expr '()) (== expr out)]
     [(numbero expr) (== expr out)]
     [(symbolo expr)
      (lookupo expr env out)]
     [(fresh (a d)
	     (== (cons a d) expr)
	     (numbero a)
	     (== out expr))]
     [(fresh (vars body env)
	     (== expr `(closure ,vars ,body ,env))
	     (== out expr))]
     [(fresh (x y xeval yeval)
	     (== `(eq? ,x ,y) expr)
	     (eval-expo x env xeval)
	     (eval-expo y env yeval)
	     (conde
	      [(== xeval yeval) (== out #t)]
	      [(=/= xeval yeval) (== out #f)])
	     )]
     [(fresh (c t f ceval)
	     (== `(if ,c ,t ,f) expr)
	     (eval-expo c env ceval)
	     (conde
	      [(== ceval #t) (eval-expo t env out)]
	      [(== ceval #f) (eval-expo f env out)]
	      ))]
     [(fresh (x y xeval yeval)
	     (== `(+ ,x ,y) expr)
	     (eval-expo x env xeval)
	     (eval-expo y env yeval)
	     (add xeval yeval out))]
     [(fresh (x y xeval yeval)
	     (== `(* ,x ,y) expr)
	     (eval-expo x env xeval)
	     (eval-expo y env yeval)
	     (multiply xeval yeval out))]
     [(fresh (x)
	     (== `(quote ,x) expr)
	     (== x out))]
     [(fresh (l a d lex)
	     (== `(car ,l) expr)
	     (eval-expo l env lex)
	     (== (cons a d) lex)
	     (== a out))]
     [(fresh (l a d lex)
	     (== `(cdr ,l) expr)
	     (eval-expo l env lex)
	     (== (cons a d) lex)
	     (== out d))]
     [(fresh (a d aex dex)
	     (== `(cons ,a ,d) expr)
	     (eval-expo a env aex)
	     (eval-expo d env dex)
	     (== (cons aex dex) out))]
     [(fresh (letbindings letexpr letenv)
	     (== `(let ,letbindings ,letexpr) expr)
	     (bind-let letbindings env letenv)
	     (eval-expo letexpr letenv out))]
     [(fresh (x body)
	     (== `(lambda ,x ,body) expr)
	     (== `(closure ,x ,body ,env) out)
	     )]
     [(fresh (e1 e2 vars val body lambda-env env^)
	     (== (cons e1 e2) expr)
	     (eval-expo e1 env `(closure ,vars ,body ,env^))
	     (bind-lambda vars e2 env env^ lambda-env)
	     (eval-expo body lambda-env out)
	     )]
     )))

(define digit-adder
  (lambda (x y out carry)
    (conde
     [(== x 0) (== carry 0) (== out y)]
     [(== y 0) (== carry 0) (== out x)]
     [(== x 1) (== y 1) (== out 2) (== carry 0)]
     [(conde [(== x 2) (== y 1)] [(== x 1) (== y 2)]) (== out 3) (== carry 0)]
     [(conde [(== x 3) (== y 1)] [(== x 2) (== y 2)] [(== x 1) (== y 3)]) (== out 4) (== carry 0)]
     [(conde [(== x 4) (== y 1)] [(== x 3) (== y 2)] [(== x 2) (== y 3)] [(== x 1) (== y 4)]) (== out 5) (== carry 0)]
     [(conde [(== x 5) (== y 1)] [(== x 4) (== y 2)] [(== x 3) (== y 3)] [(== x 2) (== y 4)] [(== x 1) (== y 5)]) (== out 6) (== carry 0)]
     [(conde [(== x 6) (== y 1)] [(== x 5) (== y 2)] [(== x 4) (== y 3)] [(== x 3) (== y 4)] [(== x 2) (== y 5)] [(== x 1) (== y 6)]) (== out 7) (== carry 0)]
     [(conde [(== x 7) (== y 1)] [(== x 6) (== y 2)] [(== x 5) (== y 3)] [(== x 4) (== y 4)] [(== x 3) (== y 5)] [(== x 2) (== y 6)] [(== x 1) (== y 7)]) (== out 8) (== carry 0)]
     [(conde [(== x 8) (== y 1)] [(== x 7) (== y 2)] [(== x 6) (== y 3)] [(== x 5) (== y 4)] [(== x 4) (== y 5)] [(== x 3) (== y 6)] [(== x 2) (== y 7)] [(== x 1) (== y 8)]) (== out 9) (== carry 0)]
     [(conde [(== x 9) (== y 1)] [(== x 8) (== y 2)] [(== x 7) (== y 3)] [(== x 6) (== y 4)] [(== x 5) (== y 5)] [(== x 4) (== y 6)] [(== x 3) (== y 7)] [(== x 2) (== y 8)] [(== x 1) (== y 9)]) (== out 0) (== carry 1)]
     [(conde [(== x 9) (== y 2)] [(== x 8) (== y 3)] [(== x 7) (== y 4)] [(== x 6) (== y 5)] [(== x 5) (== y 6)] [(== x 4) (== y 7)] [(== x 3) (== y 8)] [(== x 2) (== y 9)]) (== out 1) (== carry 1)]
     [(conde [(== x 9) (== y 3)] [(== x 8) (== y 4)] [(== x 7) (== y 5)] [(== x 6) (== y 6)] [(== x 5) (== y 7)] [(== x 4) (== y 8)] [(== x 3) (== y 9)]) (== out 2) (== carry 1)]
     [(conde [(== x 9) (== y 4)] [(== x 8) (== y 5)] [(== x 7) (== y 6)] [(== x 6) (== y 7)] [(== x 5) (== y 8)] [(== x 4) (== y 9)]) (== out 3) (== carry 1)]
     [(conde [(== x 9) (== y 5)] [(== x 8) (== y 6)] [(== x 7) (== y 7)] [(== x 6) (== y 8)] [(== x 5) (== y 9)]) (== out 4) (== carry 1)]
     [(conde [(== x 9) (== y 6)] [(== x 8) (== y 7)] [(== x 7) (== y 8)] [(== x 6) (== y 9)]) (== out 5) (== carry 1)]
     [(conde [(== x 9) (== y 7)] [(== x 8) (== y 8)] [(== x 7) (== y 9)]) (== out 6) (== carry 1)]
     [(conde [(== x 9) (== y 8)] [(== x 8) (== y 9)]) (== out 7) (== carry 1)]
     [(conde [(== x 9) (== y 9)]) (== out 8) (== carry 1)]
     )    
    ))

(define add-carry
  (lambda (x carry out carry-out)
    (conde
     [(== carry 0) (== out x) (== carry-out 0)]
     [(== carry 1) (== x 0) (== out 1) (== carry-out 0)]
     [(== carry 1) (== x 1) (== out 2) (== carry-out 0)]
     [(== carry 1) (== x 2) (== out 3) (== carry-out 0)]
     [(== carry 1) (== x 3) (== out 4) (== carry-out 0)]
     [(== carry 1) (== x 4) (== out 5) (== carry-out 0)]
     [(== carry 1) (== x 5) (== out 6) (== carry-out 0)]
     [(== carry 1) (== x 6) (== out 7) (== carry-out 0)]
     [(== carry 1) (== x 7) (== out 8) (== carry-out 0)]
     [(== carry 1) (== x 8) (== out 9) (== carry-out 0)]
     [(== carry 1) (== x 9) (== out 0) (== carry-out 1)]
     )))

(define add^
  (lambda (x y carry accum out)
    (fresh (xa xd ya yd newsum newcarry carriedsum ccarry)
	   (conde
	    [(== x '())
	     (conde
	      [(== y '()) (== carry 0) (== out accum)]
	      [(== y '()) (== carry 1) (== out (cons 1 accum))]
	      [(== (cons ya yd) y)
	       (add-carry ya carry carriedsum ccarry)
	       (add^ x yd ccarry (cons carriedsum accum) out)]
	      )]
	    [(== y '())
	     (conde
	      [(== x '()) (== carry 0) (== out accum)]
	      [(== x '()) (== carry 1) (== out (cons 1 accum))]
	      [(== (cons xa xd) x)
	       (add-carry xa carry carriedsum ccarry)
	       (add^ xd y ccarry (cons carriedsum accum) out)]
	      )]
	    [(== (cons xa xd) x) (== (cons ya yd) y)
	     (add-carry xa carry carriedsum ccarry)
	     (digit-adder carriedsum ya newsum newcarry)
	     (add^ xd yd newcarry (cons newsum accum) out)
	     ]
	    )
     )))
(define add
  (lambda (x y out)
    (fresh (xrev yrev addout)
	   (reverso x xrev)
	   (reverso y yrev)
	   (add^ xrev yrev 0 '() addout)
	   (strip-leading-zeros addout out)
	   )))
	   
(define digit-multiplier
  (lambda (x y out carry)
    (conde
     [(== x 0) (== out 0) (== carry 0)]
     [(== y 0) (== out 0) (== carry 0)]
     [(== x 1) (== out y) (== carry 0)]
     [(== y 1) (== out x) (== carry 0)]
     [(== x 2) (== y 2) (== out 4) (== carry 0)]
     [(conde [(== x 2) (== y 3)] [(== x 3) (== y 2)]) (== out 6) (== carry 0)]
     [(conde [(== x 2) (== y 4)] [(== x 4) (== y 2)]) (== out 8) (== carry 0)]
     [(conde [(== x 2) (== y 5)] [(== x 5) (== y 2)]) (== out 0) (== carry 1)]
     [(conde [(== x 2) (== y 6)] [(== x 3) (== y 4)] [(== x 4) (== y 3)] [(== x 6) (== y 2)]) (== out 2) (== carry 1)]
     [(conde [(== x 2) (== y 7)] [(== x 7) (== y 2)]) (== out 4) (== carry 1)]
     [(conde [(== x 2) (== y 8)] [(== x 4) (== y 4)] [(== x 8) (== y 2)]) (== out 6) (== carry 1)]
     [(conde [(== x 2) (== y 9)] [(== x 3) (== y 6)] [(== x 6) (== y 3)] [(== x 9) (== y 2)]) (== out 8) (== carry 1)]
     [(conde [(== x 3) (== y 3)]) (== out 9) (== carry 0)]
     [(conde [(== x 3) (== y 5)] [(== x 5) (== y 3)]) (== out 5) (== carry 1)]
     [(conde [(== x 3) (== y 7)] [(== x 7) (== y 3)]) (== out 1) (== carry 2)]
     [(conde [(== x 3) (== y 8)] [(== x 4) (== y 6)] [(== x 6) (== y 4)] [(== x 8) (== y 3)]) (== out 4) (== carry 2)]
     [(conde [(== x 3) (== y 9)] [(== x 9) (== y 3)]) (== out 7) (== carry 2)]
     [(conde [(== x 4) (== y 5)] [(== x 5) (== y 4)]) (== out 0) (== carry 2)]
     [(conde [(== x 4) (== y 7)] [(== x 7) (== y 4)]) (== out 8) (== carry 2)]
     [(conde [(== x 4) (== y 9)] [(== x 6) (== y 6)] [(== x 9) (== y 4)]) (== out 6) (== carry 3)]
     [(conde [(== x 5) (== y 5)]) (== out 5) (== carry 2)]
     [(conde [(== x 5) (== y 6)] [(== x 6) (== y 5)]) (== out 0) (== carry 3)]
     [(conde [(== x 5) (== y 7)] [(== x 7) (== y 5)]) (== out 5) (== carry 3)]
     [(conde [(== x 5) (== y 8)] [(== x 8) (== y 5)]) (== out 0) (== carry 4)]
     [(conde [(== x 5) (== y 9)] [(== x 9) (== y 5)]) (== out 5) (== carry 4)]
     [(conde [(== x 6) (== y 7)] [(== x 7) (== y 6)]) (== out 2) (== carry 4)]
     [(conde [(== x 6) (== y 8)] [(== x 8) (== y 6)]) (== out 8) (== carry 4)]
     [(conde [(== x 6) (== y 9)] [(== x 9) (== y 6)]) (== out 4) (== carry 5)]
     [(conde [(== x 7) (== y 7)]) (== out 9) (== carry 4)]     
     [(conde [(== x 7) (== y 8)] [(== x 8) (== y 7)]) (== out 6) (== carry 5)]
     [(conde [(== x 7) (== y 9)] [(== x 9) (== y 7)]) (== out 3) (== carry 6)]
     [(conde [(== x 8) (== y 8)]) (== out 4) (== carry 6)]
     [(conde [(== x 8) (== y 9)] [(== x 9) (== y 8)]) (== out 2) (== carry 7)]
     [(conde [(== x 9) (== y 9)]) (== out 1) (== carry 8)]
     )
    ))

(define multiply-row
  (lambda (x ydigit carry accum out)
    (conde
     [(== x '()) (== out (cons carry accum))]
     [(fresh (xa xd mulout mulcarry carryout ccarry nextcarry ignore)
	      (== (cons xa xd) x)
	      (digit-multiplier xa ydigit mulout mulcarry)
	      (digit-adder mulout carry carryout ccarry)
	      (digit-adder mulcarry ccarry nextcarry ignore)
	      (multiply-row xd ydigit nextcarry (cons carryout accum) out)
	      )])))

(define multiply^
  (lambda (x y zeros accum out)
    (conde
     [(== y '()) (== accum out)]
     [(fresh (ya yd rowmulout newaccum)
	     (== (cons ya yd) y)
	     (multiply-row x ya 0 zeros rowmulout)
	     (add accum rowmulout newaccum)
	     (multiply^ x yd (cons 0 zeros) newaccum out))])))

(define strip-leading-zeros
  (lambda (l out)
    (fresh (a d)
	   (== (cons a d) l)
	   (conde
	    [(== a 0) (strip-leading-zeros d out)]
	    [(=/= a 0) (== out l)]))))

(define multiply
  (lambda (x y out)
    (fresh (xrev yrev mulout)
	   (reverso x xrev)
	   (reverso y yrev)
	   (multiply^ xrev yrev '() '(0) mulout)
	   (strip-leading-zeros mulout out))))

(define reverso^
  (lambda (l build out)
    (conde
     [(== l '()) (== out build)]
     [(fresh (a d)
	     (== (cons a d) l)
	     (reverso^ d (cons a build) out))])))

(define reverso
  (lambda (l out)
    (reverso^ l '() out)))
