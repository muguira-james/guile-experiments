(use-modules (srfi srfi-64))
(use-modules (muguira first))
;; See /usr/share/guile/2.0/srfi/srfi-64/testing.scm
;; Initialize and give a name to a simple testsuite.
(test-begin "examples")
(test-group "node creation"
  (create-node "james")
  (add-property "james" :lz "alpha")
  (add-property "james" :hq 2)
  (add-property "james" 'shoe 13)

  ;; Require that an expression evaluate to true.
  
  (test-assert "'james' contains a hash of attributes" (hash-table? (find-properties "james")))
  ;; Test that an expression is eqv? to some other expression.

  (test-eqv "'jo' should not be a node name" #f
            (member "jo" (nodes->list)))

  (test-eqv "'james' should be a node name" #t (list? (find-label "james")))
  
  (test-eqv "is the lz attrib 'alpha'"
    #t
    (equal? "alpha" (cdr (car (member '(#:lz . "alpha") (find-label "james"))))))

  (test-eqv "length of attributes = 3" 3 (number-of-properties "james"))

  )
(test-group "relations"
  (create-node "james")
  (create-node "julie")
  
  (add-relationship "connected" "james" "julie" (make-hash-table))
  
  (test-eqv "is there a relationship" #t (list? (relationships->list)))

  (test-eqv "is there a 'connected' relationship" #t (eq? "connected" (car (relationships->list))))

  (test-eqv "only 1 item in the list" 1 (length (relationships->list)))
  )
(test-end "examples")
