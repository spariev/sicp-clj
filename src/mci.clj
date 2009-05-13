(ns sicp.chapterfour.interpreter
    (:use clojure.contrib.test-is))

(defn self-evaluating? [exp]
    (cond (number? exp) true
          (string? exp) true
          :else false))

(defn variable? [exp]
    (symbol? exp))

(defn pair? [lst]
  (list? lst))

(defn tagged-list? [exp tag]
    (if (pair? exp)
        (= (first exp) tag)
        false))

(defn quoted? [exp]
    (tagged-list? exp 'quote))


(defn assigment? [exp]
    (tagged-list? exp 'set!))

(defn definition? [exp]
    (tagged-list? exp 'define))

(defn definition-variable [exp]
    (if (symbol? (first (rest exp)))
        (first (rest exp))
        (ffrest exp)))

(defn definition-value [exp]
    (if (symbol? (nth exp 1))
        (nth exp 2)
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
    (rest exp))

(defn last-exp? [seq]
    (null? (rest seq)))

(defn first-exp [seq]
    (first seq))

(defn rest-exp [seq]
    (rest seq))

(defn sequence->exp [seq]
    (cond (null? seq) seq
          (last-exp? seq) (first-exp seq)
          else (make-begin seq)))

(defn application? [exp]
    (pair? exp))

(defn operator [exp]
    (first exp))

(defn operands [exp]
    (rest exp))

(defn no-operands? [ops]
    (null? ops))

(defn first-operand [ops]
    (first ops))

(defn rest-operands [ops]
    (rest ops))

(defn is-true? [x]
  (not (= x false)))

(defn is-false? [x]
  (= x false))

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))
(defn compound-procedure? [p]
  (tagged-list? p 'procedure))
(defn procedure-parameters [p] (cadr p))
(defn procedure-body [p] (caddr p))
(defn procedure-environment [p] (cadddr p))

(defn enclosing-environment [env] (rest env))
(defn first-frame [env] (first env))

(defn the-empty-environment '())

(define make-frame [variables values]
  (cons variables values))

(defn frame-variables [frame] (first frame))
(defn frame-values [frame] (rest frame))
(defn add-binding-to-frame! [var val frame]
  (set-first! frame (cons var (first frame)))
  (set-rest! frame (cons val (rest frame))))

(defn extend-environment [vars vals base-env]
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(defn lookup-variable-value [var env]
        (defn env-loop [env]
                (defn scan [vars vals]
                        (cond ((null? vars)
                               (env-loop (enclosing-environment env)))
                              ((eq? var (first vars))
                               (first vals))
                              (else (scan (rest vars) (rest vals)))))
                (if (eq? env the-empty-environment)
                    (error "Unbound variable" var)
                    (let ((frame (first-frame env)))
                         (scan (frame-variables frame)
                               (frame-values frame)))))
        (env-loop env))

(defn (set-variable-value! var val env)
        (defn (env-loop env)
                (defn (scan vars vals)
                        (cond ((null? vars)
                               (env-loop (enclosing-environment env)))
                              ((eq? var (first vars))
                               (set-first! vals val))
                              (else (scan (rest vars) (rest vals)))))
                (if (eq? env the-empty-environment)
                    (error "Unbound variable -- SET!" var)
                    (let ((frame (first-frame env)))
                         (scan (frame-variables frame)
                               (frame-values frame)))))
        (env-loop env))

(defn (defn-variable! var val env)
        (let ((frame (first-frame env)))
             (defn (scan vars vals)
                     (cond ((null? vars)
                            (add-binding-to-frame! var val frame))
                           ((eq? var (first vars))
                            (set-first! vals val))
                           (else (scan (rest vars) (rest vals)))))
             (scan (frame-variables frame)
                   (frame-values frame))))

(defn (setup-environment)
        (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
             (defn-variable! 'true true initial-env)
             (defn-variable! 'false false initial-env)
             initial-env))
(defn the-global-environment (setup-environment))

(defn (primitive-procedure? proc)
        (tagged-list? proc 'primitive))
(defn (primitive-implementation proc) (cadr proc))

(defn primitive-procedures
        (list (list 'first first)
              (list 'rest rest)
              (list 'cons cons)
              (list 'null? null?)
              <more primitives>
              ))
(defn (primitive-procedure-names)
        (map first
             primitive-procedures))
(defn (primitive-procedure-objects)
        (map (lambda (proc) (list 'primitive (cadr proc)))
             primitive-procedures))

(defn (apply-primitive-procedure proc args)
        (apply-in-underlying-scheme
          (primitive-implementation proc) args))

(defn evl [exp env]
    (cond (self-evaluating? exp) exp
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
    (defn-variable! (definition-variable exp)
                      (eval (definition-value exp) env))
    :ok))

(defn input-prompt ";;; M-Eval input:")
(defn output-prompt ";;; M-Eval value:")
(defn (driver-loop)
        (prompt-for-input input-prompt)
        (let ((input (read)))
             (let ((output (eval input the-global-environment)))
                  (announce-output output-prompt)
                  (user-print output)))
        (driver-loop))
(defn (prompt-for-input string)
        (newline) (newline) (display string) (newline))
(defn (announce-output string)
        (newline) (display string) (newline))

(defn (user-print object)
        (if (compound-procedure? object)
            (display (list 'compound-procedure
                           (procedure-parameters object)
                           (procedure-body object)
                           '<procedure-env>))
            (display object)))