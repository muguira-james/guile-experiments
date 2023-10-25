;;
;;
;; for testing change "load-path" to point to local directory
;;  export GUILE_LOAD_PATH=".:..."
;;
;; also copy the current version of first.scm into muguira/
;;
;; Provide a set of funcs to handle a graph
;; make-graph - create a graph, which has a
;; a basic graph has a node-list, an edge-list, an adjacency-list
;; and adjacency-matrix. A graph is an srfi-9 record
;;
;; * node-list - the named nodes in the graph
;; ** a node in the list has:
;; *** a set of properties
;; *** an adjacency list or list of nodes connected to this node
;;
;; * edge-list - named, unnamed edges in the graph
;; ** each edge has a set of properties
;;
;; api:
;; create-node name, properties
;; create-edge name, properties, from / to
;; match node or edge
;;

;;
;; enable ":lz" type keyword syntax
(read-set! keywords 'prefix)


(define-module (muguira first)
  #:export (create-node
            add-property
            find-label
            find-properties
            number-of-properties
            node-list
            node->list
            add-relationship
            relationships->list
            relationship-list))

(use-modules (srfi srfi-9))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-13))
(use-modules (ice-9 hash-table))
(use-modules (ice-9 format)) 


;;
;; the node and edge lists
(define *node-list* (make-hash-table))
(define *edge-list* (make-hash-table))

(define-record-type <node>
  (make-node label properties adjacency-list)
  node?
  (label node-label set-node-label!)
  (properties node-properties set-node-properties!)
  (adjacency-list node-adjacency-list set-node-adjacency-list!))
;;
;; pretty print node attributes
(define (p-properties hsh-table)
  (hash-fold
   (lambda (k v prior)
     (cons (cons k v) prior))
   '() hsh-table))

;;
;; pretty print node insformation
(define (p-item k v)
  (begin
    (format #t "k: ~a ~a ~a ~a~%"
            k
            (node-label v)
            (p-properties (node-properties v))
            (node-adjacency-list v))))
            

;; print the current *node-list*
(define (node-list)
  (hash-for-each p-item *node-list*))


;;
;; return the node attributes
(define (find-properties label)
  (node-properties (hash-ref *node-list* label)))

;;
;; answer the number of attributes for node name
(define (number-of-properties label)
  (hash-count (const #t) (find-properties label)))


;;
;; answer a list of node names
(define (nodes->list)
  (hash-fold
   (lambda (key value prior)
     (cons key prior))
   '() *node-list*))

;;
;; create a node named name
(define (create-node label)
  (let* ((prop (make-hash-table))
         (adj-list '()))
  (hash-set! *node-list* label (make-node label prop adj-list))
  (format #f "name: ~a~%" label)))

;;
;; add attributes key, value to node named name
(define (add-property node-label key value)
  (if (member node-label (nodes->list))
      (hash-set! (find-properties node-label) key value)
      #f))
  
;; ------------------------- Edge code ------------------

(define-record-type <relation>
  (make-relation name from to properties)
  relation?
  (name relation-name set-relation-name!)
  (from relation-from set-relation-from!)
  (to   relation-to   set-relation-to!)
  (properties relation-properties set-relation-properties!))


(define (add-relationship name from to properties)
  (let ((rel (make-relation name from to properties))
        (from-adj (hash-ref *node-list* from))
        (to-adj (hash-ref *node-list* to)))
    (begin
      (hash-set! *edge-list* name rel)
      (set-node-adjacency-list!
       from-adj
       (append! (node-adjacency-list (hash-ref *node-list* from)) to))
      (set-node-adjacency-list!
       to-adj
       (append! (node-adjacency-list (hash-ref *node-list* to)) from)))))

(define (p-rel k v)
  (format #t "<~a> <~a>~%" k v))

(define (relationships-list)
  (hash-for-each p-rel *edge-list*))

(define (relationships->list)
  (hash-fold
   (lambda (k v prior)
     (cons k prior))
     '()
     *edge-list*))

;;
;; ------------------------ experiment code -----------
;(define-record-type <employee>
;  (make-employee name age salary)
;  employee?
;  (name employee-name)
;  (age employee-age set-employee-age!)
;  (salary employee-salary set-employee-salary!))


(define my-list '(1 2 3 4 5 6 7 8 9))

(define (mlast lst)
  (if (null? (cdr lst))
      (car lst)
      (mlast (cdr lst))))
;; tests
(create-node "james")
(add-property "james" :lz "alpha")
(add-property "james" :hq 2)
(add-property "james" 'shoe 13)
;; 
(create-node "julie")
(add-property "julie" :lz "bravo")
(add-property "julie" :hq 2)
(add-property "julie" 'age 2)
(node-list)
