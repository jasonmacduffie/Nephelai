
(import
  (scheme base)
  (scheme read)
  (scheme write)
  (scheme cxr))

(define kastian-names
  '("Barapi"
    "Gudin"
    "Gidi"
    "Lalen"
    "Nadi"
    "Nelin"
    "Padin"
    "Roding"
    "Sadip"
    "Usmita"
    "Bahuta"
    "Basidra"
    "Gadi"
    "Giha"
    "Goda"
    "Lila"
    "Pesh"
    "Sadipa"
    "Sasha"
    "Valedi"))

(define (convert-language s converter)
  (define l (string->list s))
  (let loop ((in l)
             (out '()))
    (if (null? in)
        (list->string (reverse out))
        (let ((next-char-set (converter (car in) (if (null? (cdr in)) #\X (cadr in)) (length in))))
          (define skip (car next-char-set))
          (define next-char (cadr next-char-set))
          (loop (cond
                 ((= skip 0) (cdr in))
                 ((= skip 1) (cddr in))
                 ((= skip 2) (cdddr in))
                 (else (cddddr in)))
                (append next-char out))))))

(define (kastian-char->walasian-char c c-look-ahead sz)
  (cond
   ((and (char=? c #\n) (char=? c-look-ahead #\g) (= sz 2))
    '(1 (#\k)))
   ((and (char=? c #\t) (char=? c-look-ahead #\a) (= sz 2))
    '(1 (#\t)))
   ((and (char=? c #\d) (char=? c-look-ahead #\r))
    '(1 (#\r #\t)))
   ((char=? c #\D)
    '(0 (#\h #\T)))
   ((char=? c #\d)
    '(0 (#\h #\t)))
   ((char=? c #\G)
    '(0 (#\K)))
   ((char=? c #\g)
    '(0 (#\k)))
   (else
    (list 0 (list c)))))

(define (kastian-char->maramian-char c c-look-ahead sz)
  (cond
   ((and (char=? c #\n) (char=? c-look-ahead #\g) (= sz 2))
    '(1 (#\k)))
   ((and (char=? c #\t) (char=? c-look-ahead #\a) (= sz 2))
    '(1 (#\t)))
   ((and (char=? c #\d) (char=? c-look-ahead #\r))
    '(1 (#\r #\t)))
   ((and (char=? c #\i) (= sz 1))
    '(0 (#\r #\a)))
   ((char=? c #\D)
    '(0 (#\h #\T)))
   ((char=? c #\d)
    '(0 (#\h #\t)))
   ((char=? c #\G)
    '(0 (#\K)))
   ((char=? c #\g)
    '(0 (#\g)))
   ((char=? c #\a)
    '(0 (#\i)))
   ((char=? c #\i)
    '(0 (#\a)))
   (else
    (list 0 (list c)))))


(define (kastian-char->apnian-char c c-look-ahead sz)
  (cond
   ((and (char=? c #\n) (char=? c-look-ahead #\g) (= sz 2))
    '(1 (#\k)))
   ((and (char=? c #\t) (char=? c-look-ahead #\a) (= sz 2))
    '(1 (#\t)))
   ((and (char=? c #\d) (char=? c-look-ahead #\r))
    '(1 (#\r #\t)))
   ((and (char=? c #\i) (= sz 1))
    '(0 (#\i)))
   ((char=? c #\o)
    '(0 (#\i)))
   ((char=? c #\i)
    '(0 (#\u)))
   ((char=? c #\u)
    '(0 (#\o)))
   ((char=? c #\O)
    '(0 (#\I)))
   ((char=? c #\I)
    '(0 (#\U)))
   ((char=? c #\U)
    '(0 (#\O)))
   ((char=? c #\G)
    '(0 (#\K)))
   ((char=? c #\g)
    '(0 (#\g)))
   ((char=? c #\D)
    '(0 (#\h #\T)))
   ((char=? c #\d)
    '(0 (#\h #\t)))
   (else
    (list 0 (list c)))))

(define (kastian->walasian s) (convert-language s kastian-char->walasian-char))
(define (kastian->maramian s) (convert-language s kastian-char->maramian-char))
(define (kastian->apnian s) (convert-language s kastian-char->apnian-char))

(write (map kastian->walasian kastian-names))
(newline)
(write kastian-names)
(newline)
(write (map kastian->maramian kastian-names))
(newline)
(write (map kastian->apnian kastian-names))
(newline)

