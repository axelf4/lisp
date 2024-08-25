(set list (fn xs xs))

(set defmacro
     (list (fn (x . body)
             (list 'set (car x)
                   (list 'list (cons 'fn (cons (cdr x) body)))))))

(defmacro (progn . forms) (cons 'let (cons () forms)))

;; Quasiquotes STRUCTURE.
;;
;; See: BAWDEN, Alan, et al. Quasiquotation in Lisp. In: PEPM. 1999.
;;      p. 4-12.
(defmacro (quasiquote structure)
  (let (listify
        (fn (xs acc-fn tail)
          ;; TODO (append '(xs...) '(ys...) ...) -> (append '(xs... ys...) ...)
          (if xs (let (x (car xs)
                       acc-fn2 (if (= (car x) 'unquote-splicing) 'append (if tail 'list* 'list)) ; TODO cons
                       cont? (= (= acc-fn 'append) (= acc-fn2 'append))
                       tail2 (if (if cont? acc-fn) (progn (set acc-fn2 acc-fn) tail)
                               (if tail (list (if acc-fn (cons acc-fn tail) tail)))))
                   ;; Push elements in groups of same type
                   (listify (cdr xs) acc-fn2 (cons (cdr x) tail2)))
            (let (expr (if acc-fn (cons acc-fn tail) tail))
              (cons (if (= (car expr) 'quote) 'quote 'unquote) expr))))
        join-quote
        (fn (xs tail)
          (if (if (= (car (car xs)) 'quote) (= (car tail) 'quote))
              (join-quote (cdr xs)
                          (list 'quote (cons (car (cdr (cdr (car xs)))) (car (cdr tail)))))
            (listify xs nil (if (= tail ''()) () tail))))
        ;; Processes the body of a quasiquote.
        ;;
        ;; LEVEL is the nesting level. Returns (TAG . FORM) where TAG
        ;; is one of quote, unquote or unquote-splicing.
        f (fn (level x)
            (if (consp x)
                (if (member (car x) '(unquote unquote-splicing))
                    (if (= level 0) (cons (car x) (car (cdr x))) ; TODO ,@'(x) -> 'x
                      (join-quote (list (list 'quote 'quote (car x)))
                                  (cdr (f (+ level -1) (cdr x)))))
                  (if (= (car x) 'quasiquote)
                      (join-quote '((quote . 'quasiquote)) (cdr (f (+ level 1) (cdr x))))
                    ;; Otherwise, scan and translate this list level
                    (let (g (fn (acc xs)
                              (if (if (consp xs)
                                      (not (member (car xs) '(unquote quasiquote))))
                                  (g (cons (f level (car xs)) acc) (cdr xs))
                                (let (tail (cdr (f level xs)))
                                  (if (if (= tail ''()) (= (car (car acc)) 'unquote-splicing))
                                      (join-quote (cdr acc) (cdr (car acc)))
                                    (join-quote acc tail))))))
                      (g () x))))
              (list 'quote 'quote x))))
    (cdr (f 0 structure))))
(defmacro (unquote _) (throw ())) ; Invalid outside of quasiquote
(set unquote-splicing unquote)

(defmacro (def x . body)
  (cons 'set (if (consp x)
                 ;; `(,(car x) (let (,(car x) (fn ,(cdr x) . ,body)) ,(car x)))
                 (list (car x) (cons 'fn (cons (cdr x) body)))
               (cons x body))))

(def (null x) (if x nil t))
(set not null)
(def (member elt list)
  (if list (if (= (car list) elt) list (member elt (cdr list)))))

(def (list* x . xs)
  (let (f (fn (x xs) (if xs (cons x (f (car xs) (cdr xs))) x))) (f x xs)))
(def (append . xs)
  (let (f (fn (xs rest)
            (if xs (cons (car xs) (f (cdr xs) rest))
              (if rest (f (car rest) (cdr rest))))))
    (f (car xs) (cdr xs))))
