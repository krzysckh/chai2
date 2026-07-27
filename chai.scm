(import
 (owl toplevel)
 (owl args)
 (owl readline)
 (robusta full)
 (prefix (owl sys) sys/)
 )

(define command-line-rules
  (cl-rules
   `((help    "-h" "--help")
     (rebuild "-r" "--rebuild"             comment "rebuild")
     (config  "-c" "--config-file" has-arg comment "set config file" default "chai.cfg")
     (edit    "-e" "--edit"                comment "enter interactive db edit mode")
     )))

(define-syntax define-config
  (syntax-rules (=>)
    ((_) empty)
    ((_ k => v . r)
     (put (_ . r) (quote k) v))))

(define *alphabet* (list->vector (string->bytes "abcdefghijklmnopqrstuvwxyz")))

(define (generate-id* config . rs)
  (lets ((len (get config 'id-length 8))
         (rs ns (random-numbers
                 (if (null? rs) (seed->rands (time-ns)) (car rs))
                 (vector-length *alphabet*)
                 len)))
    (values
     rs
     (string->symbol (bytes->string (map (H vector-ref *alphabet*) ns))))))

(define (generate-id config db)
  (let loop ((rs (seed->rands (time-ns))))
    (lets ((rs k (generate-id* config rs)))
      (if (get db k #f)
          (begin
            (format #f "key ~a is already in the db, re-rolling.~%" k)
            (loop rs))
          k))))

(define (load-database config)
  (list->ff
   (map
    (λ (x) (cons (car x) (list->ff (cdr x))))
    (fasl-load (get config 'database "chai.db") '()))))

(define (save-database config db)
  (fasl-save
   (map (λ (x) (cons (car x) (ff->list (cdr x)))) (ff->list db))
   (get config 'database "chai.db")))

(define (ensure-directory path)
  (if (sys/directory? path)
      path
      (begin
        (if (sys/mkdir path #o777)
            path
            (error "couldn't mkdir " path)))))

(define (www-root config)
  (if-lets ((path (get config 'www-root)))
    (ensure-directory path)
    (error "no www-root was specified in config " config)))

(define (in-directory* dir thunk)
  (let ((pwd (sys/getcwd)))
    (sys/chdir (ensure-directory dir))
    (let ((res (thunk)))
      (sys/chdir pwd)
      res)))

(define-syntax with-directory
  (syntax-rules ()
    ((_ dir exp ...)
     (in-directory* dir (λ () exp ...)))))

(define (minimize-image! config filename)
  (with-directory (www-root config)
    ((get config
          'minimize-image-function
          (λ (image)
            (lets ((new-name (str image "-small.jpg")))
              (system (list "convert" (str image) "-resize" "640" new-name))
              new-name))) filename)))

(define (write-rss! config items)
  (with-directory (www-root config)
    (list->file
     (string->bytes
      (rss/encode (get config 'name "images") (get config 'host "http://localhost") (get config 'rss-description "images")
                  (map
                   (λ (ob)
                     (let ((img (format #f "~a/~a" (get config 'host "http://localhost") (get ob 'filename)))
                           (img-small (format #f "~a/~a" (get config 'host "http://localhost") (get ob 'filename-minimized))))
                       (pipe empty
                         (put 'pubDate (get ob 'timestamp))
                         (put 'title (get ob 'filename))
                         (put 'description (html/encode*
                                            `((img (src . ,img)
                                                   (data-timestamp . ,(str (get ob 'timestamp)))
                                                   (data-thumb . ,img-small)
                                                   (alt . ,(str (get ob 'tags)))))
                                            '()))
                         (put 'link img))))
                   items)))
     (get config 'rss "/posts.rss"))))

(define (extension-of file)
  (last ((string->regex "c/\\./") file) "jpg"))

(define (render-tags tags class desc)
  `((div (id . ,class))
    ,desc ,@(map (λ (t)
                   `((span (class . "tag")) ((a (href . ,(format #f "/tags/~a/1.html" t))) ,t) " "))
                 tags)))

(define (render-image config ob)
  `(article
    ((a (href . ,(str "/" (get ob 'filename))))
     ((img (loading . "lazy") (src . ,(str "/" (get ob 'filename-minimized))))))
    ,(render-tags (get ob 'tags '()) "image-tags" "")
    ,((get config 'display-time-function str) (get ob 'timestamp 0))
    (hr)))

(define (get-all-tags db)
  (ff-fold (λ (a k v) (union a (get v 'tags #n))) #n db))

(define (split-every l n)
  (let loop ((l l))
    (if (< (len l) n)
        (if (null? l)
            ()
            (list l))
        (lets ((xs l (split-at l n)))
          (cons xs (loop l))))))

(define (write-paginated! config db root title items)
  (with-directory (www-root config)
    (ensure-directory "tags")
    (with-directory root
      (let* ((all-tags (get-all-tags db))
             (spl (split-every items (get config 'items-per-page 20)))
             (last-page (len spl)))
        (for-each
         (λ (i)
           (let ((items (lref spl (- i 1))))
             (list->file
              (string->bytes
               (html/encode
                `(html
                  (head
                   ((meta (charset . "utf-8")))
                   ((meta (name . "viewport") (content . "width=device-width, initial-scale=1.0")))
                   ((link (rel . "stylesheet") (href . ,(get config 'css "/style.css"))))
                   ((link (rel . "alternate") (type . "application/rss+xml") (href . ,(get config 'rss "/posts.rss"))))
                   (title ,title))
                  (body
                   ,((get config 'html-heading (λ (tag-name) `(h1 ,tag-name))) title)
                   (hr)
                   ,(render-tags all-tags "all-tags" "all tags :: ")
                   (hr)
                   ,@(map (H render-image config) items)
                   (nav
                    ,@(if (= i 1)
                          #n
                          `(((p (class . "nav-prev"))
                             ((a (href . ,(format #f "~a.html" (- i 1)))) "prev"))))
                    ,@(if (= i last-page)
                          #n
                          `(((p (class . "nav-next"))
                             ((a (href . ,(format #f "~a.html" (+ i 1)))) "next")))))
                   ))))
            (format #f "~a.html" i))))
         (iota 1 1 (+ 1 last-page)))
        (copy-file "1.html" "index.html")))))

(define (tag->items db tag)
  (ff-fold (λ (a k v)
             (if (has? (get v 'tags '()) tag)
                 (cons (cons k v) a)
                 a))
           #n db))

(define (write-tag-files! config db tag)
  (let ((images (map cdr (tag->items db tag))))
    (write-paginated! config db (format #f "tags/~a" tag) tag images)))

(define (rebuild config . db*)
  (lets ((db (if (null? db*) (load-database config) (car db*)))
         (items (sort
                 (λ (a b)
                   (> (get (cdr a) 'timestamp)
                      (get (cdr b) 'timestamp)))
                 (ff->list db)))
         (rss-location (get config 'rss "posts.rss"))
         (all-tags (get-all-tags db))
         )
    (write-rss! config (map cdr items))
    (for-each (λ (tag) (write-tag-files! config db tag)) all-tags)
    (write-paginated! config db "." (get config 'name "images") (map cdr items))))

(define (add-image config data)
  (when (null? data)
    (error "no image was provided" ()))
  (lets ((db (load-database config))
         (filename tags data)
         (tags (if (null? tags) (get config 'default-tags '(unknown)) (map string->symbol tags)))
         (id (generate-id config db))
         (new-filename (str id "." (extension-of filename)))
         (_ (copy-file filename (str (www-root config) "/" new-filename)))
         (new-db
          (put db
               id
               (ff
                'filename new-filename
                'filename-minimized (minimize-image! config new-filename)
                'timestamp (time)
                'tags tags))))
    (rebuild config new-db)
    (save-database config new-db)
    0))

(define (with-readline-handler prompt handler)
  (readline-result-stream
   '()
   (λ () (display prompt))
   (λ (str cont)
     (lets ((l ((string->regex "c/ /") str))
            (l* (if (null? l)
                    (tuple '_)
                    (list->tuple (cons (string->symbol (car l)) (cdr l))))))
       (print "")
       (handler l* cont)))
   (λ _ _ #f)
   readline-default-options))

(define (edit! id config db ob cont!)
  (with-readline-handler (str id "? ")
    (λ (l cont)
      (case (ref l 1)
        ('help
         (print '(help set-tags tags timestamp set-timestamp remove save quit))
         (cont))
        ('tags
         (print (get ob 'tags))
         (cont))
        ('timestamp
         (format stdout "~a (~a)~%" (get ob 'timestamp) (date-str (get ob 'timestamp)))
         (cont))
        ('set-timestamp
         (print "ok")
         (edit! id config db (put ob 'timestamp (string->number (ref l 2))) cont!))
        ('set-tags
         (print "ok")
         (edit! id config db (put ob 'tags (map string->symbol (cdr (tuple->list l)))) cont!))
        ('save
         (save-database config (put db id ob))
         (print "saved")
         (cont!))
        ('remove
         (with-readline-handler "are you sure? [y/n] "
           (λ (yn cont*)
             (tuple-case yn
               ((y)
                (save-database config (del db id))
                (print "saved")
                (cont!))
               ((n) (cont))
               (else (cont*))))))
        ('quit
         (print "did not save")
         (cont!))
        (else
         (print "bad command")
         (cont))))))

(define (edit/display-ob config id ob)
  (format stdout "[~a] ~a ~a/~a~%"
          id (get ob 'tags)
          (get config 'host) (get ob 'filename-minimized)))

(define (edit config)
  (let ((db (load-database config)))
    (with-readline-handler "? "
      (λ (l cont)
        (tuple-case l
          ((help)
           (print '(list tags tag pick rebuild quit)))
          ((rebuild)
           (rebuild config)
           (cont))
          ((list)
           (ff-fold (λ (_ k v)
                      (edit/display-ob config k v)) #n db)
           (cont))
          ((tags)
           (print (get-all-tags db))
           (cont))
          ((tag t)
           (let ((its (tag->items db (string->symbol t))))
             (for-each
              (λ (it*)
                (let ((k (car it*))
                      (it (cdr it*)))
                  (edit/display-ob config k it)))
              its)
             (cont)))
          ((pick id*)
           (if-lets ((id (string->symbol id*))
                     (ob (get db id)))
             (edit! id config db ob (λ () (edit config)))
             (begin
               (print "no such id")
               (cont))))
          ((quit) (print 'bye) #t)
          (else
           (print "bad command")
           (cont)))))))

(define (help args)
  (print "Usage: " (car args) " file tag ...")
  (print-rules command-line-rules)
  (halt 0))

(λ (args)
  (process-arguments
   (cdr args) command-line-rules "you lose"
   (λ (opt extra)
     (when (get opt 'help #f)
       (help args))

     (let ((config (eval (read (list->string (file->list (get opt 'config)))) *toplevel*)))
       (cond
        ((get opt 'rebuild) (rebuild config))
        ((get opt 'edit)    (edit config))
        (else
         (add-image config extra)))
       0))))
