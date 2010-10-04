;; namespace.scm
;; Simple namespace record/functions for Chicken.
;; (Lifted from the Delta project.)

(use srfi-1)
(use srfi-13) ;; for STRING<
(use srfi-69) ;; no longer auto-imported in Chicken 4.6.0

(define-record namespace
  parent   ;; parent namespace or #f
  names    ;; hash table
  )

(define-record-printer namespace
  (lambda (ns port)
    (fprintf port "#<namespace ~s>" (namespace-names ns))))

(define (create-namespace parent)
  (make-namespace parent (make-hash-table)))

;; find NAME in local namespace or parents; if found, set it to VALUE in
;; the namespace where it was found
(define (namespace-update! ns name value)
  (let ((result (namespace-get-3 ns name)))
    (if result
        (let ((target-ns (third result)))
          (namespace-define! target-ns name value))
        (error 'namespace-update! "Undefined value; cannot update"))))

;; set NAME to VALUE in the local namespace.
(define (namespace-define! ns name value)
  (assert (symbol? name) 'namespace-define! "name must be a symbol")
  (hash-table-set! (namespace-names ns) name value))

;; look up NAME in the local namespace (i.e. without looking in parents).
;; return (name value) if found, #f otherwise.
;; (we can use a default here because values should always be "objects",
;;  never #f.)
(define (namespace-get-local ns name)
  (and-let* ((value (hash-table-ref/default (namespace-names ns) name #f)))
            (list name value)))

;; look up NAME in the local namespace; if not found, try looking in parent
;; namespace(s). return #f if not found.
(define (namespace-get ns name)
  (or (namespace-get-local ns name)
      (and (namespace-parent ns)
           (namespace-get (namespace-parent ns) name))))

;; return (key value namespace) or #f
(define (namespace-get-3 ns name)
  (let ((result (namespace-get-local ns name)))
    (if result
        (list (first result) (second result) ns)
        (if (namespace-parent ns)
            (namespace-get-3 (namespace-parent ns) name)
            #f))))

(define (namespace-keys ns)
  (sort (hash-table-keys (namespace-names ns)) symbol<)) 

(define (namespace-all-keys ns)
  (define (get-all-keys-raw ns)
    (if (namespace-parent ns)
        (append (hash-table-keys (namespace-names ns))
                (namespace-all-keys (namespace-parent ns)))
        (hash-table-keys (namespace-names ns))))
  (sort (unique (get-all-keys-raw ns) member) symbol<)) 

;;; --- auxiliary functions ---

;; Compare symbols like strings.
(define (symbol< a b)
  (string< (symbol->string a) (symbol->string b)))
