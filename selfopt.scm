(define-record-type packet (fields src dst))

(define m '(()))

(define (sum m k v)
  (let ((kv (assoc k (car m))))
    (if kv
        (set-cdr! kv (+ v (cdr kv)))
        (set-car! m (cons (cons k v) (car m))))))

(define (inc m k)
  (sum m k 1))

(define key cons)

(define f
  (lambda (p)
    (inc m (key (packet-src p) (packet-dst p)))))

(define f_batch
  (lambda (ps)
    (for-each (lambda (p) (inc m (key (packet-src p) (packet-dst p))))
              ps)))

(define measure
  (lambda (thunk)
    (let ((s (real-time)))
      (thunk)
      (1+ (- (real-time) s)))))

(define fs (list (cons 'f f) (cons 'f_batch f_batch)))

(define sk '(()))
(define st '(()))

(define is_batch
  (lambda (k)
    (eq? 'f_batch k)))

(define sample_packet
  (lambda (n)
    (let loop ((i 0))
      (if (>= i n)
          '()
          (cons
           (make-packet (random 100) (random 100))
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

(define pipeline
  (lambda (n)
    (let loop ((i 0))
      (if (>= i n)
          'done
          (let ((f_kv (sample fs sk st)))
            (let* ((b (if (is_batch (car f_kv)) 10 1))
                   (arg (if (is_batch (car f_kv))
                            (sample_packet b)
                            (car (sample_packet b)))))
              (sum sk (car f_kv) b)
              (sum st (car f_kv) (measure (lambda () ((cdr f_kv) arg))))
              (loop (1+ i))))))))
