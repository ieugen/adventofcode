;; Guile hoot tutorial - 0.2.0
;; https://spritely.institute/files/docs/guile-hoot/0.2.0/Tutorial.html
;;
(use-modules (hoot compile)
             (hoot reflect)
             (wasm parse)
             (wasm assemble)
             (ice-9 binary-ports))


(define the-answer (compile 42))
(define reflector (call-with-input-file "js-runtime/reflect.wasm"
                    parse-wasm))


(define instance (hoot-instantiate reflector the-answer))
(display (hoot-load instance))

(display (compile-value reflector '(list 1 2 3)))

(define hoot-factorial
  (compile-value reflector
                 '(let ()
                    (define (factorial x result)
                      (if (= x 1)
                          result
                          (factorial (- x 1)
                                     (* result x))))
                    factorial)))

(hoot-factorial 5 1)

(define hello (compile "Hello, world!"))
(define bin (assemble-wasm hello))

(call-with-output-file "hoot-tootorial/hello.wasm"
  (lambda (port)
    (put-bytevector port bin)))
