(define-library (match match)

  (import (scheme base)
  	  (scheme cxr)
	  (sagittarius) ;; for er-macro-transformer
  	  (match trie)
	  (match compile-pattern)
	  (match interpret-tree))

  (export match
  	  compile-patterns)

  (include "match.scm"))
