;; Generate Markov model from a list of sentences (plain-text)

(use srfi-69)
(use srfi-1)
(use extras)
(require-library regex)
(import regex)

(define (main)
  (if (= 0 (length (command-line-arguments)))
    (begin
      (display "No input corpus specified")
      (exit))
    (display (generate-sentence (make-chain (car (command-line-arguments)))))))

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
                  (chain-sentence! chain words)
                  (loop!))))))))))

(define (chain-sentence! chain words)
  (cond
    ((null? words) #f)
    ((null? (cdr words))
     (chain-add! chain (car words) #f)) ;; end of sentence
    (else (begin
            (chain-add! chain (car words) (cadr words))
            (chain-sentence! chain (cdr words))))))

;;
;; chain-add takes a word and the next word and adds it to the chain.
;; The end of a sentence is marked by #f.
;;
(define chain-add!
  (lambda (chain word next)
    (if (hash-table-exists? chain word)
      (let ((word-entry (hash-table-ref chain word)))
        (if (hash-table-exists? word-entry next)
          (hash-table-set! word-entry next
                           (+ 1 (hash-table-ref word-entry next)))
          (hash-table-set! word-entry next 1)))
      (let ((new-word-entry (make-hash-table)))
        (hash-table-set! new-word-entry next 1)
        (hash-table-set! chain word new-word-entry)))))

(define split-regex (regexp "(,|[\\w\\d'\\.]*[\\w\\d])"))
(define (split-line line)
  (string-split-fields split-regex line))

(define (print-chain chain)
  (format #t "Hash table has ~a entries\n" (hash-table-size chain))
  (hash-table-map chain
                  (lambda (word word-entry)
                    (format #t "~a\n" word)
                    (hash-table-map word-entry
                                    (lambda (next next-prob)
                                      (format #t "\t~a\t~a\n" next next-prob))))))

(define (generate-sentence chain)
  (let* ((keys (hash-table-keys chain))
         (count (hash-table-size chain))
         (first (choose-random (filter-capitalized keys))))
    (fold (lambda (word sentence)
            (cond
              ((or (equal? word ",") (equal? word "."))
               (string-append sentence word))
              (else
                (string-append sentence " " word))))
          first
          (let loop ((start first))
            (let* ((word-entry (hash-table-ref chain start))
                   (choice (choose-random word-entry)))
              (if choice
                (cons choice (loop choice))
                '()))))))

(define (choose-random ls)
  (cond
    ((hash-table? ls)
     (let* ((keys (hash-table-keys ls))
            (count (hash-table-size ls)))
       (list-ref keys (random count))))
    (else (let ((len (length ls)))
            (list-ref ls (random len))))))

(define (filter-capitalized ls)
  (filter 
    (lambda (word)
      (cond
        ((= (string-length word) 0) #f)
        ((char-upper-case? (string-ref word 0)) #t)
        (else #f)))
    ls))

;; go!
(main)
