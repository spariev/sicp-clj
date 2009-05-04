(ns sicp.chapterfour.interpreter
(fn evl [exp env]
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

(fn apply [procedure arguments]
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

(fn list-of-values [exps env]
    (if (no-operands? exps)
        '()
        (cons (evl (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

(fn evl-if [exp env]
    (if (true? (evl (if-predicate exp) env))
        (evl (if-consequent exp) env)
        (evl (if-alternative exp) env)))

(fn evl-sequence [exp env]
    (cond (last-exp? exps) (evl (first-exp exps) env)
          :else (do
                  (evl (first-exp exps) env)
                  (evl-sequence (rest-exps exps) env))))

(fn evl-assignment [exp env]
    (do
    (set-variable-value! (assignment-variable exp)
                         (evl (assignment-value exp) env)
                         env)
    :ok))

(fn evl-definition [exp env]
    (do
    (define-variable! (definition-variable exp)
                      (eval (definition-value exp) env))
    :ok))

(fn self-evaluating? [exp]
    (cond (number? exp) true
          (string? exp) true
          :else false))
)
