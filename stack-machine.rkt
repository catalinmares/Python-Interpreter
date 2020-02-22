#lang racket

(require "opcodes.rkt")
(require racket/list)
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) empty-stack)

(define push cons)
(define top car)
(define pop cdr)

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine)
  (second stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine)
  (third stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine)
  (fourth stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine)
  (fifth stack-machine))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine)
  (first stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine)
  (last stack-machine))

(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (index-of symbols symbol))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (list-set stack-machine (get-symbol-index symbol) item))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (list-set stack-machine 0 (push value (get-stack stack-machine))))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (list-set stack-machine 0 (pop (get-stack stack-machine))))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (let* ([co-code (get-code stack-machine)]
         [nrinstr (length co-code)])
    (let loop ([stack-mach stack-machine])
      (let ([IC (get-IC stack-mach)])
        (if (equal? IC nrinstr)
            stack-mach
            (let* ([instruction (list-ref co-code IC)]
                   [function (car instruction)]
                   [param (cdr instruction)])
              (case function
                ['POP_TOP (loop (pop-top (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['BINARY_MODULO (loop (binary-modulo (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['BINARY_ADD (loop (binary-add (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['BINARY_SUBTRACT (loop (binary-subtract (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['INPLACE_ADD (loop (inplace-add (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['INPLACE_SUBTRACT (loop (inplace-subtract (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['INPLACE_MODULO (loop (inplace-modulo (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['GET_ITER (loop (get-iter (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['RETURN_VALUE (loop (return-value (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['FOR_ITER (loop (for-iter param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['LOAD_CONST (loop (load-const param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['COMPARE_OP (loop (compare-op param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['JUMP_ABSOLUTE (loop (jump-absolute param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['POP_JUMP_IF_FALSE (loop (pop-jump-if-false param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['LOAD_GLOBAL (loop (load-global param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['LOAD_FAST (loop (load-fast param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['STORE_FAST (loop (store-fast param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['CALL_FUNCTION (loop (call-function param (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach)))]
                ['SETUP_LOOP (loop (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach))]
                ['POP_BLOCK (loop (update-stack-machine (add1 IC) 'INSTRUCTION-COUNTER stack-mach))])))))))

(define (pop-top stack-machine)
  (pop-exec-stack stack-machine))

(define (binary-modulo stack-machine)
  (let* ([stack (get-stack stack-machine)]
         [TOS (top stack)]
         [TOS1 (top (pop stack))])
    (push-exec-stack (modulo TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (binary-add stack-machine)
  (let* ([stack (get-stack stack-machine)]
         [TOS (top stack)]
         [TOS1 (top (pop stack))])
    (push-exec-stack (+ TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (binary-subtract stack-machine)
  (let* ([stack (get-stack stack-machine)]
         [TOS (top stack)]
         [TOS1 (top (pop stack))])
    (push-exec-stack (- TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))

(define inplace-add binary-add)

(define inplace-subtract binary-subtract)

(define inplace-modulo binary-modulo)

(define (get-iter stack-machine)
  stack-machine)

(define (return-value stack-machine)
  stack-machine)

(define (for-iter delta stack-machine)
  (let* ([stack (get-stack stack-machine)]
         [iterator (top stack)]
         [IC (get-IC stack-machine)])
    (if (null? iterator)
        (pop-exec-stack (update-stack-machine (+ IC (/ (+ delta 2) 2)) 'INSTRUCTION-COUNTER stack-machine))
        (push-exec-stack (car iterator) (push-exec-stack (cdr iterator) (pop-exec-stack stack-machine))))))

(define (load-const index stack-machine)
  (push-exec-stack (hash-ref (get-consts stack-machine) index) stack-machine))

(define (compare-op opindex stack-machine)
  (let* ([stack (get-stack stack-machine)]
         [TOS (top stack)]
         [TOS1 (top (pop stack))])
    (push-exec-stack ((get-cmpop opindex) TOS1 TOS) (pop-exec-stack (pop-exec-stack stack-machine)))))

(define (jump-absolute target stack-machine)
  (update-stack-machine (/ target 2) 'INSTRUCTION-COUNTER stack-machine))

(define (pop-jump-if-false target stack-machine)
  (let* ([stack (get-stack stack-machine)]
         [TOS (top stack)])
    (if (not TOS)
        (pop-exec-stack (update-stack-machine (/ target 2) 'INSTRUCTION-COUNTER stack-machine))
        (pop-exec-stack stack-machine))))

(define (load-global index stack-machine)
  (push-exec-stack (hash-ref (get-names stack-machine) index) stack-machine))

(define (load-fast index stack-machine)
  (push-exec-stack (hash-ref (get-varnames stack-machine) index) stack-machine))

(define (store-fast index stack-machine)
  (pop-exec-stack (update-stack-machine (hash-set (get-varnames stack-machine) index (top (get-stack stack-machine))) 'CO-VARNAMES stack-machine)))

(define (call-function argc stack-machine)
  (let loop ([stack-mach stack-machine]
             [argv '()]
             [count 0])
    (let ([stack (get-stack stack-mach)])
      (if (equal? count argc)
          (push-exec-stack (apply (get-function (top stack)) argv) (pop-exec-stack stack-mach))
          (let* ([new-stack-mach (pop-exec-stack stack-mach)])
            (loop new-stack-mach (cons (top stack) argv) (add1 count)))))))