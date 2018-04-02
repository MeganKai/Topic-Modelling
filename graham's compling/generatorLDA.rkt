#lang racket


(define (normalize params)
  (let ((sum (apply + params)))
    (map (lambda (x) (/ x sum)) params)))

(define (flip p)
  (if (< (random 100) (* p 100))
      #t
      #f))

(define (sample-categorical outcomes params)
  (if (flip (car params))
      (car outcomes)
      (sample-categorical (cdr outcomes) 
                          (normalize (cdr params)))))

(define (get-count obs observation-list count)
  (if (equal? observation-list '())
      count
      (if (equal? obs (car observation-list))
          (get-count obs (cdr observation-list) (+ 1 count))
          (get-count obs (cdr observation-list) count))))

(define (get-counts outcomes observation-list)
  (define (count-obs obs)
    (get-count obs observation-list 0))
  (map count-obs outcomes))

;LDA and GIBBS CODE STARTS HERE
(define (sample-document vocab topics theta-prior length)
  (if (equal? length 0)
      '()
      (cons (sample-categorical vocab (sample-categorical topics theta-prior))
            (sample-document vocab topics theta-prior (- length 1)))))

(define (sample-document-LDA vocab topics topics-distribution theta-priors length)
  (let ((theta-prior (sample-categorical theta-priors topics-distribution))) ;distribution over topics
    (sample-document vocab topics theta-prior length)))

(define my-corpus '((broccoli is good)
                    (green onion is good)
                    (meat and cheese is good)))

(define vocabulary '(broccoli is good green onion meat and cheese))


(define topic1 (list (/ 1 8 ) (/ 1 8 ) (/ 1 8 ) (/ 1 8 ) (/ 1 8 ) (/ 1 8 ) (/ 1 8 ) (/ 1 8 ))) ; distribution over the words
(define topic2 (list (/ 1 8 ) (/ 1 16) (/ 3 16 ) (/ 1 8 ) (/ 1 8 ) (/ 1 8 ) (/ 1 8 ) (/ 1 8 )))
(define topics (list topic1 topic2)) ;think of this as a vector which assigns prob 0 to all other word distributions

; distribution over topic distributions. One assigned per document.

(define theta-prior1 (list (/ 1 8) (/ 7 8))) ; assigned to topic1 and topic2 (and 0 to all other topics)
(define theta-prior2 (list (/ 1 2) (/ 1 2))) ; theta-prior1 and theta-prior2 are distributions over the topics

; allows us to pick which topics a document focuses on
; we parametrize this simplex by focusing on topics whose distributions are closer to a realistic topic
(define theta-priors (list theta-prior1 theta-prior2)) ;think of this as a simplex which puts weight on these two distributions over topics
(define topics-distribution (list (/ 1 4) (/ 3 4)))

(define document1 (sample-document-LDA vocabulary topics topics-distribution theta-priors 20))

(define (gibbs-sampler-single document vocab topics topics-distribution)
  (let* ((topic-assignments (map (lambda (x) (sample-categorical topics topics-distribution)) document)) ;randomly assign a topic to each word in the document
         (topic-counts (get-counts topics topic-assignments))) ; count the topics
    (normalize topic-counts))) ;normalize the topic counts and return this. this is the new distribution over the topics (originally inputted as the uniform). 

(define (gibbs-sampler-n document vocab topics topics-distribution count) ; repeat gibbs sample n times
  (if (= count 0)
      topics-distribution
      (gibbs-sampler-n document vocab topics (gibbs-sampler-single document vocab topics topics-distribution) (- count 1))))


