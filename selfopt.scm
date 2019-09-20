;; user functions

(define fib
  (lambda (n)
    (if (< n 2) 1
        (+ (fib (- n 1)) (fib (- n 2))))))

(define fib_iter
  (lambda (a b n)
    (if (< n 2) b
        (fib_iter b (+ a b) (- n 1)))))

(define f
  (lambda (n)
    (fib n)))

(define f_iter
  (lambda (n)
    (let loop ((i 0))
      (if (= i (* 1000 n)) 'done
          (begin
            (fib_iter 1 1 n)
            (loop (1+ i)))))))

(define make_batch
  (lambda (f)
    (lambda (ps)
      (for-each f ps))))

(define fs (list
            (cons 'f_iter f_iter) (cons 'f_iter_batch (make_batch f_iter))
            (cons 'f f) (cons 'f_batch (make_batch f))))

;; runtime

(define sk '(()))
(define st '(()))
(define summaries '())
(define sk_per '(()))
(define st_per '(()))

(define (sum m k v)
  (let ((kv (assoc k (car m))))
    (if kv
        (set-cdr! kv (+ v (cdr kv)))
        (set-car! m (cons (cons k v) (car m))))))

(define (sum_per n m k v)
  (let ((nv (assoc n (car m))))
    (if nv
        (sum (cdr nv) k v)
        (set-car! m (cons (cons n (list (list (cons k v)))) (car m))))))

(define (inc m k)
  (sum m k 1))

(define measure
  (lambda (thunk)
    (let ((s (real-time)))
      (thunk)
      (1+ (- (real-time) s)))))

(define is_batch?
  (lambda (k)
    (let* ((s (symbol->string k))
           (l (string-length s)))
      (and (>= l 6)
           (equal? "_batch" (substring s (- l 6) l))))))

(define sample_packet
  (lambda (n)
    (let loop ((i 0))
      (if (>= i n)
          '()
          (cons
           (random 40)
           (loop (1+ i)))))))

(define nth
  (lambda (i xs)
    (if (= i 0)
        (car xs)
        (nth (- i 1) (cdr xs)))))

(define nth-ratio
  (lambda (r sr)
    (if (< r (cdar sr))
        (caar sr)
        (nth-ratio r (map (lambda (kv) (cons (car kv) (+ (cdr kv) (cdar sr))))
                          (cdr sr))))))

(define missing
  (lambda (fs st)
    (if (null? fs)
        #f
        (let ((ex (assoc (caar fs) st)))
          (if ex
              (missing (cdr fs) st)
              (car fs))))))

(define sample
  (lambda (fs sk st)
    (let ((kv (missing fs (car st))))
      (if kv
          kv
          (let ((sr (inversely (over st sk))))
            (let ((t (total (map cdr sr))))
              (let ((r (random t)))
                (assoc (nth-ratio r sr) fs))))))))

(define over
  (lambda (st sk)
    (map (lambda (kv)
           (cons (car kv)
                 (/ (cdr kv) (* 1.0
                                (cdr (assoc (car kv) (car sk)))))))
         (car st))))

(define total
  (lambda (xs)
    (if (null? xs)
        0
        (+ (car xs) (total (cdr xs))))))

(define inversely
  (lambda (sr)
    (map (lambda (kv) (cons (car kv) (/ 1 (cdr kv)))) sr)))

(define summary
  (lambda (st sk)
    (let* ((table (inversely (over st sk)))
           (t (total (map cdr table))))
      (sort (lambda (x y) (> (cdr x) (cdr y)))
            (map (lambda (kv)
                   (cons (car kv) (/ (cdr kv) t)))
                 table)))))

(define summary_per
  (lambda ()
    (sort (lambda (x y) (< (car x) (car y)))
          (map
           (lambda (kv)
             (cons (car kv)
                   (summary (cdr (assoc (car kv) (car st_per))) (cdr kv))))
           (car sk_per)))))

(define print_summary_per
  (lambda ()
    (for-each (lambda (x)
                (format #t "(~2@a " (car x))
                (for-each (lambda (kv)
                            (format #t "(~6@a ~,3f) "
                                    (car kv) (cdr kv)))
                          (cdr x))
                (format #t ")\n")) (summary_per))))

(define pipeline
  (lambda (n)
    (let loop ((i 0))
      (if (>= i n)
          'done
          (let ((f_kv (sample fs sk st)))
            (let* ((k (car f_kv))
                   (b (if (is_batch? k) 10 1))
                   (arg (if (is_batch? k)
                            (sample_packet b)
                            (car (sample_packet b))))
                   (r (measure (lambda () ((cdr f_kv) arg)))))
              (sum sk k b)
              (sum st k r)
              (if (is_batch? k)
                  (for-each (lambda (a)
                             (sum_per a sk_per k 1)
                             (sum_per a st_per k (/ r 10.0)))
                           arg)
                  (begin
                    (sum_per arg sk_per k b)
                    (sum_per arg st_per k r)))
              (if (= 0 (modulo i 50))
                  (set! summaries (cons (summary st sk) summaries)))
              (loop (1+ i))))))))

(define pipeline-adaptive
  (lambda (m n)
    (let ((fs (filter (lambda (kv) (not (is_batch? (car kv)))) fs)))
      (let loop ((i 0))
        (if (>= i n)
            'done
            (let* ((arg (car (sample_packet 1)))
                   (sk_per_arg (assoc n (car sk_per)))
                   (st_per_arg (assoc n (car st_per)))
                   (f_kv (if (or (< i m) (not sk_per_arg) (not st_per_arg))
                             (sample fs sk st)
                             (sample fs (cdr sk_per_arg) (cdr st_per_arg))))
                   (r (measure (lambda () ((cdr f_kv) arg))))
                   (k (car f_kv)))
              (sum sk k 1)
              (sum st k r)
              (sum_per arg sk_per k 1)
              (sum_per arg st_per k r)
              (if (= 0 (modulo i 50))
                  (set! summaries (cons (summary st sk) summaries)))
              (loop (1+ i))))))))
