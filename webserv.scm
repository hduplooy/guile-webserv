; webserv.scm
; Web server functionality for Guile Scheme (min version 2.0)
(use-modules (web server))
(use-modules (web request))
(use-modules (web response))
(use-modules (web uri))
(use-modules (ice-9 iconv))
(load "webserv-support.scm")

; sessions are stored in a hash table
; each session has an associan list with the various vars saved in this association list
(define sessions (make-hash-table 100))
(define sesscnt 1) ; current session count
(define session-timeout (* 10 60)) ; timeout in seconds for a session

; session-get-keys - get all the session keys (for all the sessions that are open)
(define (session-get-keys)
    (hash-map->list (lambda (key val) key) sessions))

; session-check-sessions - go through the session and if any timedout then destroy them
(define (session-check-sessions)
    (do ((lst (session-get-keys) (cdr lst)))
        ((null? lst))
        (let ((tmp (session-getval (car lst) 'last-access)) (now (current-time)))
            (if (or (not tmp) (> (- now tmp) session-timeout))
                (session-destroy (car lst) #f)))))

; init-session - create a new session, save it in sessions but also put the session key to the cookies of the output
(define (init-session rh)
    (let ((key sesscnt))
        (set! sesscnt (1+ sesscnt))
        (if rh
            (response-header-set rh 'set-cookie (string-append "sesskey=" (number->string key))))
        (hash-set! sessions key (list (cons 'key key) (cons 'last-access (current-time))))
        key))

; session-touch - check if session is still valid, if not destroy it else update the last accessed time
(define (session-touch key rh)
    (let ((tmp (session-getval key 'last-access)) (now (current-time)))
        (if (or (not tmp) (> (- now tmp) session-timeout))
            (begin
                (session-destroy key rh)
                (response-header-set rh 'set-cookie "sesskey=-1") ; invalidate the cookie
                #f)
            (begin (session-setval key 'last-access now) #t))))

; session-destroy - just removes the session from the sessions global variable
(define (session-destroy key rh)
    (hash-remove! sessions key))

; session-check - check to see if session exists
(define (session-check key)
  (if (hash-ref sessions key) #t #f))

; session-getval - from the saved session get the variable
(define (session-getval key var)
    (let ((tmp (hash-ref sessions key)))
        (if tmp
            (assoc-ref tmp var)
            #f)))

; session-setval - get the session and set the variable to val
(define (session-setval key var val)
    (let ((tmp (hash-ref sessions key)))
        (if tmp
            (hash-set! sessions key (vars-setvar tmp var val))
            (hash-set! sessions key (vars-setvar (list (cons 'key key)) var val)))))

; session-clear - just clears the session of all variables
(define (session-clear key)
    (hash-set! sessions key (list (cons 'key key))))

; not-found - generate an error message when a resource is not fuond
(define (not-found request)
    (string-append "Resource not found: " (uri->string (request-uri request))))

; request-path - returns the uri-path from the request
(define (request-path request)
    (uri-path (request-uri request)))

; request-get-sessionid - from the cookies look for the sesskey which indicates the session id
(define (request-get-sessionid request)
    (let ((tmp (assoc-ref (request-headers request) 'cookie)))
        (if tmp
            (let ((tmp2 (assoc-ref
                          (map
                              (lambda (a)
                                  (string-split (string-trim-both a) #\=))
                          (string-split tmp #\;))
                        "sesskey")))
                (if tmp2
                   (string->number (car tmp2))
                    #f))
            #f)))

; request-query - parses the request path for any parameters and if the body is that from a post from a form those parameters as well
; these are returned as an association list
(define (request-query request body)
    (let* ((tmp (uri-query (request-uri request)))
           (conttp (assoc-ref (request-headers request) 'content-type))
           (prs (if (not tmp) '() (map (lambda (a)
                        (let ((tmp2 (string-split a #\=)))
                             (cons (string->symbol (car tmp2)) (regexp-substitute/global #f "%20" (cadr tmp2) 'pre " " 'post))))
                     (string-split tmp #\&)))) )
         (when (and conttp (equal? (car conttp) 'application/x-www-form-urlencoded))
            (let* ((tmp (bytevector->string body "utf-8"))
                   (prs2 (if (not tmp) '() (map (lambda (a)
                                 (let ((tmp2 (string-split a #\=)))
                                      (cons (string->symbol (car tmp2)) (regexp-substitute/global #f "\\+" (cadr tmp2) 'pre " " 'post))))
                              (string-split tmp #\&))) ))
                  (if (not (null? prs2))
                    (for-each (lambda (a)
                      (set! prs (vars-setvar prs (car a) (cdr a)))
                        )
                        prs2))))
           prs))

; vars->query - a support function that will take an association list and convert it to a get query form
(define (vars->query vars)
    (string-join
        (map
            (lambda (a) (string-append (tostring (car a))  "=" (tostring (cdr a))))
            vars)
        "&"))

; handlers are the various handlers to handle specific routes
(define handlers (make-hash-table))
; part handlers are similar to above, but they will match part of routes
(define parthandlers '())

; notfoundhandler - if a route cannot be matched to any handler then a not found error message is returned
; this can be overridden in custom code for a customized handler
(define notfoundhandler (lambda (request body rh) (not-found request)))

; handler-add add a handler to the hash table of handlers
(define (handler-add path handler)
    (hash-set! handlers path handler))

; handler-add-part - add a partial handler to the list of partial handlers
(define (handler-add-part path handler)
  (set! parthandlers (vars-setvar parthandlers path handler)))

; main-handler - the main routine to handle any request-get-sessionid
; It will check the session key and touch the session
; It will first check to see if there is a handler to handle the request
;    if there is that handler is passwed all the info
;    else it checks to see if there is a partial handler
;    else it will call notfoundhandler
(define (main-handler request body)
    (let* ((sesskey (request-get-sessionid request)) (pth (request-path request)) (tmp (hash-ref handlers pth)) (rh (response-header-init request)))
        (session-touch sesskey rh)
        (if tmp
            (let* ((response (tmp request body rh)))
                (values (response-header-generate rh)  response))
            (do ((lst parthandlers (cdr lst)))
                ((or (null? lst) (string-startwith pth (caar lst)))
                  (if (null? lst)
                    (let* ((response (notfoundhandler request body rh)))
                        (values (response-header-generate rh)  response))
                    (let* ((response ((cdar lst) request body rh)))
                         (values (response-header-generate rh)  response))))))))

; response-header-init - all headers in the response is in a hash tablem so this creates an empty hash table
(define (response-header-init request)
    (let ((tmp (make-hash-table)))
        (hash-set! tmp 'content-type '(text/html))
        tmp))

; response-header-set - set the key in the resp hash table to val
(define (response-header-set resp key val)
    (hash-set! resp key val))

; response-header-generate - resp is just a hash table and that is converted to an association list
(define (response-header-generate resp)
    (hash-map->list (lambda (key val) (cons key val)) resp))

; file-handler - a handler that will serve any of the files in base+path
(define (file-handler request path body base)
    (let* ((path (substring (request-path request) (string-length path))))
          (getfile (string-append base path))))

; serve-file - read file fname and return that as result to a web request
(define (serve-file request fname base)
    (let ((tmp (getfile (string-append base fname))))
        (if tmp
            tmp
            (not-found request))))

; session-manager - every minute check for stale sessions
(define (session-manager)
    (while #t
        (session-check-sessions)
        (sleep 60)))

; start-server - exactly what it says
; first start the thread to manage the sessions
; start the web server with main-handler as the function to handle incomming requests
(define (start-server port)
    (call-with-new-thread session-manager)
    (run-server main-handler 'http (list '#:port port)))
