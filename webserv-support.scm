; webserv-support.scm
; Support functions for the webserv.scm file
(use-modules (json))
(use-modules (ice-9 rdelim))

; vars-??? are functions around association lists

; vars-init - create empty list with no vars
(define (vars-init)
    '())

; vars-setvar - make a copy of the current vars and then set the var in it to val
(define (vars-setvar vars var val)
    (let ((nvars (copy-tree vars)))
        (assoc-set! nvars var val)))

; vars-getvar - just returns the specific value of var in the set of vars or #f
(define (vars-getvar vars var)
    (assoc-ref vars var))

; vars-addvar - add a value to the current set of values for var
(define (vars-addvar vars var val)
    (let ((tmp (vars-getvar vars var)))
        (if tmp
            (assoc-set! (copy-tree vars) var (cons val tmp))
            (assoc-set! (copy-tree vars) var (list val)) ) ) )

; vars-remvar - remove the specific var from the set
(define (vars-remvar vars var)
    (assoc-remove! vars var))

; tostring - do a flat conversion of everything to string with no added spaces
(define (tostring lst)
    (cond ((string? lst) lst)
          ((symbol? lst) (symbol->string lst))
          ((number? lst) (number->string lst))
          ((list? lst) (apply string-append (map tostring lst)))
          (else "")))

; scm->string - convert the values passed to a string but the way it would have looked if written to output
(define (scm->string . lst)
  (define (aux lst)
      (cond
          ((null? lst) "()")
          ((string? lst) (string-append "\"" lst "\""))
          ((symbol? lst) (symbol->string lst))
          ((number? lst) (number->string lst))
          ((char? lst) (string-append "#\\" (string lst)))
          ((list? lst) (apply string-append
                          (append
                              '("(")
                              (cons (aux (car lst))
                                    (map (lambda (a) (string-append " " (aux a)))
                                         (cdr lst)))
                              '(")"))))
          ((pair? lst) (string-append "(" (aux (car lst)) " . " (aux (cdr lst)) ")"))
          (else "")))
  (apply string-append (map aux lst)))

; alist? - checks a list to see if it is an association list
(define (alist? lst)
  (and (list? lst) (apply equal? (cons #t (map pair? lst))) (apply equal? (cons #t (map (lambda (a) (and (or (pair? a) (> (length a) 0)) (symbol? (car a)))) lst)))))

; esscm->json - converts a scmeme list to JSON representation
(define (esscm->json lst)
  (cond ((string? lst) (string-append "\"" lst "\""))
        ((symbol? lst) (string-append "\"" (symbol->string lst) "\""))
        ((boolean? lst) (if lst "true" "false"))
        ((alist? lst) (string-append "{" (string-join (map (lambda (a) (string-append (esscm->json (car a)) ": " (esscm->json (cdr a)))) lst) ", ") "}"))
        ((hash-table? lst) (string-append "{" (string-join (hash-map->list (lambda (a b) (string-append  (esscm->json a) ": " (esscm->json b))) lst) ", ") "}"))
        ((list? lst) (string-append "[" (string-join (map esscm->json lst) ", ") "]"))
        (else (tostring lst))))

; esjson-scm - will convert a JSON value to scheme by first using guile-json to do the initial conversion
; because I use association lists and not hash tables in the webserver esjson->scm basically just converts
; all hash tables to association lists
; The source for guile-json can be found here:
; http://download.savannah.gnu.org/releases/guile-json/guile-json-0.6.0.tar.gz
(define (esjson->scm str)
  (let ((tmp (json-string->scm str)))
      (define (aux val)
          (cond
              ((hash-table? val) (hash-map->list (lambda (a b) (cons (string->symbol a) (aux b))) val))
              ((list? val) (map aux val))
              ((pair? val) (cons (aux (car val)) (aux (cdr val))))
              (else val)))
      (aux tmp)))

; hash->alist - converts a hash table to an assocation list, but only at the top level
; it doesn't recurse to do any hash tables within the values
(define (hash->alist lst)
  (hash-map->list (lambda (a b) (cons (string->symbol a) b)) lst))

; string-startwith - checks if a str1 starts with what is in str2
(define (string-startwith str1 str2)
(if (or (not (string? str1)) (not (string? str2)) (> (string-length str2) (string-length str1)))
  #f
  (string=? (substring str1 0 (string-length str2)) str2)))

; string-endwith - check if str1 end with what is in str2
(define (string-endwith str1 str2)
  (if (or (not (string? str1)) (not (string? str2)) (> (string-length str2) (string-length str1)))
    #f
    (let ((l1 (string-length str1)) (l2 (string-length str2)))
      (string=? (substring str1 (- l1 l2) l1) str2))))

; getfile - read a file in as a string
(define (getfile fname)
    (if (file-exists? fname)
      (call-with-input-file fname (lambda (port) (read-string port)))
      #f))
