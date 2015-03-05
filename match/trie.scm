;; The merge procedure merges a list of sequences into a trie

(define (merge seqs)
  (cond ((null? seqs) '())
	((null? (car seqs)) (merge (cdr seqs)))
	(else (let* ((partition (partition-by-head (caar seqs) (cdr seqs)))
		     (tails (car partition))
		     (rest (cdr partition)))
		(cons `(branch ,(caar seqs) ,(merge (cons (cdar seqs) tails)))
		      (merge rest))))))

(define (partition-by-head head seqs)
  (cond ((null? seqs) '(() . ()))
	((null? (car seqs)) (partition-by-head head seqs))
	(else (let* ((partition (partition-by-head head (cdr seqs)))
		     (l (car partition))
		     (r (cdr partition)))
		(if (equal? head (caar seqs))
		    (cons (cons (cdar seqs) l) r)
		    (cons l (cons (car seqs) r)))))))

