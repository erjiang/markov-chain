(module markov-chain (make-chain generate-sentence)
;; Generate Markov model from a list of sentences (plain-text)

(import chicken scheme)
(use srfi-1)
(use srfi-13)
(use srfi-69)
(use extras)
(use irregex)

;; blacklisted words will be ignored silently
(define blacklist '("nbsp" "ndash"))
(define (filter-blacklisted ls)
  (filter 
    (lambda (word)
      (if (member word blacklist) #f
        #t))
    ls))

(define (make-chain input-file)
  (with-input-from-file input-file
    (lambda ()
      (let ((chain (make-hash-table)))
        (let loop! ()
          (let ((line (read-line)))
            (if (eof-object? line)
              chain
              (let ((words (split-line line)))
                (begin
                  (chain-sentence! chain (filter-blacklisted words))
                  (loop!))))))))))

(define (chain-sentence! chain words)
  (cond
    ((null? words) #f)
    ((null? (cdr words)) #f)
    ((null? (cddr words))
     (chain-add! chain (car words) (cadr words) #f)) ;; end of sentence
    (else (begin
            (chain-add! chain (car words) (cadr words) (caddr words))
            (chain-sentence! chain (cdr words))))))

;;
;; chain-add takes a word and the next word and adds it to the chain.
;; The end of a sentence is marked by #f.
;;
(define chain-add!
  (lambda (chain word1 word2 next)
    (if (hash-table-exists? chain word1)
      (let ((word2-entry (hash-table-ref chain word1)))
        (if (hash-table-exists? word2-entry next)
          (hash-table-set! word2-entry word2 (cons next (hash-table-ref word2-entry next)))
          (hash-table-set! word2-entry word2 (list next))))
      (let ((word2-entry (make-hash-table)))
        (hash-table-set! word2-entry word2 (list next))
        (hash-table-set! chain word1 word2-entry)))))

(define split-regex
  (irregex '(or ","
                (seq (* (or #\'
                            #\.
                            alphanumeric))
                     alphanumeric))))
;;(define split-regex
;;  (irregex '(* (~ whitespace))))

(define (split-line line)
  (irregex-extract split-regex line))

(define (print-chain chain)
  (format #t "Hash table has ~a entries\n" (hash-table-size chain))
  (hash-table-map chain
    (lambda (word1 word2-entry)
      (format #t "~a\n" word1)
      (hash-table-map word2-entry
                      (lambda (word2 words-next)
                        (format #t "\t~a\n" word2)
                        (for-each 
                          (lambda (word)
                            (format #t "\t\t~a\n" word))
                          ;(sort word-entry string-compare)))))
                          words-next))))))

(define (generate-sentence chain)
  (let* ((keys (hash-table-keys chain))
         (count (hash-table-size chain))
         (first (choose-random (filter-capitalized keys)))
         (second (choose-random (hash-table-keys (hash-table-ref chain first)))))
    (fold (lambda (word sentence)
            (cond
              ((let ((first (string-ref word 0)))
                 (or (eq? first #\,) (eq? first #\.)))
               (string-append sentence word))
              (else
                (string-append sentence " " word))))
          (string-append first " " second)
          (let loop ((word1 first)
                     (word2 second))
            (format #t "Looking up ~a -> ~a\n" word1 word2)
            (let* ((word1-entry (hash-table-ref chain word1))
                   (word2-entry (hash-table-ref/default word1-entry word2 #f))
                   (choice (if word2-entry
                             (choose-random word2-entry)
                             (let ((word2-entry (hash-table-ref/default chain word2 #f)))
                               (if word2-entry
                                 (choose-random
                                   (hash-table-ref/default
                                     word2-entry
                                     (choose-random word2-entry) #f))
                                 #f)))))
              (if choice
                (cons choice (loop word2 choice))
                '()))))))

(define (choose-random ls)
  (cond
    ((hash-table? ls)
     (let* ((keys (hash-table-keys ls))
            (count (hash-table-size ls)))
       (list-ref keys (random count))))
    (else (let ((len (length ls)))
            (list-ref ls (random len))))))

;;
;; filter-capitalized only works on its first invocation
;;
(define filter-capitalized
  (let ((cached-lists (make-hash-table eq?)))
    (lambda (ls)
      (let ((cached-keys (hash-table-ref/default cached-lists ls #f)))
        (if (not cached-keys)
          (begin
            (let ((filtered-list 
                    (filter
                      (lambda (word)
                        (cond 
                          ((= (string-length word) 0) #f)
                          ((char-upper-case? (string-ref word 0)) #t)
                          (else #f)))
                      ls)))
              (hash-table-set! cached-lists ls filtered-list)
              filtered-list))
          cached-keys)))))

;; go!
)
