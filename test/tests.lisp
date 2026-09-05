(def (test name p)
  (print (cons (if p 'pass (set failed? 'fail)) name)))

(test "gensyms are unique" (not (= (gensym) (gensym))))

(test "man or boy"
      (let (a (fn (k x1 x2 x3 x4 x5)
                (let (b (fn () (set k (+ k -1)) (a k b x1 x2 x3 x4)))
                  (if (< k 1) (+ (x4) (x5)) (b)))))
        (= (a 10 (fn () 1) (fn () -1) (fn () -1) (fn () 1) (fn () 0)) -67)))

(def (fib n)
  (if (< n 2)
      n
    (+ (fib (+ n -1)) (fib (+ n -2)))))

(test "it computes 10th Fibonacci" (= (fib 10) 55))
(test "it computes 30th Fibonacci" (= (fib 30) 832040))

(def (ack m n)
  (if (< m 1)
      (+ n 1)
    (if (= n 0)
        (ack (+ m -1) 1)
      (ack (+ m -1) (ack m (+ n -1))))))

(test "it computes Ackermann" (= (ack 3 7) 1021))

(if failed? (fail))
