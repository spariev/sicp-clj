(ns sicp.chapterfour.interpreter
(defn evl [exp env]
    (cond (self-evluating? exp) exp
          (variable? exp) (lookup-variable-value exp env)
          (quoted? exp) (text-of-quotation exp)
          (assigment? exp) (evl-assignment exp env)
          (definition? exp) (evl-definition exp env)
          (if? exp) (evl-if exp env)
          (lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env)
          (begin? exp)
           (evl-sequence (begin-actions exp) env)
          (cond? exp) (evl (cond->if exp) env)
          (application? exp)
           (apply (evl (operator exp) env)
                  (list-of-values (operands exp) env))
          :else
            (error "Unknown expression type -- evl" exp)
      ))

(defn apply [procedure arguments]
    (cond (primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments)
          (compound-procedure? procedure)
           (evl-sequence
             (procedure-body procedure)
             (extend-environment
               (procedure-paramenters procedure)
               arguments
               (procedure-environment procedure)))
          :else
            (error "Unknown procedure type -- APPLY" procedure)))

(defn list-of-values [exps env]
    (if (no-operands? exps)
        '()
        (cons (evl (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

(defn evl-if [exp env]
    (if (true? (evl (if-predicate exp) env))
        (evl (if-consequent exp) env)
        (evl (if-alternative exp) env)))

(defn evl-sequence [exp env]
    (cond (last-exp? exps) (evl (first-exp exps) env)
          :else (do
                  (evl (first-exp exps) env)
                  (evl-sequence (rest-exps exps) env))))

(defn evl-assignment [exp env]
    (do
    (set-variable-value! (assignment-variable exp)
                         (evl (assignment-value exp) env)
                         env)
    :ok))

(defn evl-definition [exp env]
    (do
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env))
    :ok))

(defn self-evaluating? [exp]
    (cond (number? exp) true
          (string? exp) true
          :else false))

(defn variable? [exp]
    (symbol? exp))

(defn quoted? [exp]
    (tagged-list? exp 'quote))

(defn tagged-list? [exp tag]
    (if (pair? exp)
        (= (car exp) tag)
        false))

(defn assigment? [exp]
    (tagged-list? exp 'set!))

(defn definition? [exp]
    (tagged-list? exp 'define))

(defn definition-variable [exp]
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

(defn definition-value [exp]
    (if (symbol? (card exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))

(defn lambda? [exp]
    (tagged-list? exp 'lambda))

(defn lambda-parameters [exp]
    (cadr exp))

(defn lambda-body [exp]
    (cddr exp))


(defn make-lambda [parameters body]
    (cons 'lambda (cons parameters body)))

(defn if? [exp]
    (tagged-list exp 'if))

(defn if-predicate [exp]
    (cadr exp))

(defn if-consequent [exp]
    (caddr exp))


(defn if-alternative [exp]
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

(defn make-if [predicate consequent alternative]
    (list 'if predicate consequent alternative))

(defn begin? [exp]
    (tagged-list exp 'begin))

(defn begin-actions [exp]
    (cdr exp))

(defn last-exp? [seq]
    (null? (cdr seq)))

(defn first-exp [seq]
    (car seq))

(defn rest-exp [seq]
    (cdr seq))

(defn sequence->exp [seq]
    (cond (null? seq) seq
          (last-exp? seq) (first-exp seq)
          else (make-begin seq)))

(defn application? [exp]
    (pair? exp))

(defn operator [exp]
    (car exp))

(defn operands [exp]
    (cdr exp))

(defn no-operands? [ops]
    (null? ops))

(defn first-operand [ops]
    (car ops))

(defn rest-operands [ops]
    (cdr ops))



)
