;;; -*- mode:lisp; coding:utf-8  -*-

(defparameter *debug* nil)

(defun subs (rxo)
    (rx:subscribe rxo
                  (lambda (x) (if *debug* (console "Ok" x))
                      (print (list 'Ok x)))
                  (lambda (x) (print (list 'Error x)))
                  (lambda () (print 'Complete )) ))

;;;(fset 'mkjso #'make-js-object)


;;; observable-while

(let ((*counter* 0))
    (subs (rx:observable-while
           (lambda () (< (incf *counter*) 5))
           (rx:observable-just 42)) ) )


;;; observable-case
(let ((sources (mkjso "foo" (rx:observable-just 42) "bar" (rx:observable-just 22)))
      (default  (rx:observable-empty)))
    (subs (rx:observable-case
           (lambda () "foo")
           sources
           default))

    (subs (rx:observable-case
           (lambda () "bar")
           sources
           default)) )


;;; observable-amb
;;;
(subs
 (rx:observable-amb
  (rx:select (rx:observable-timer 500) (lambda (x idx obs) "foo"))
  (rx:select (rx:observable-timer 200) (lambda (x idx obs) "bar")))
 )


;;; CONCAT

(subs
 (rx:observable-concat
  (rx:observable-just 11)
  (rx:observable-empty)
  (rx:observable-just 55)))



;;; generate-with-absolute-time
(subs (rx:time-interval
       (rx:observable-generate-with-absolute-time
        1
        (lambda (x) (< x 4))
        (lambda (x) (1+ x))
        (lambda (x) x)
        (lambda (x) (+ (#j:Date:now (* 100 x))))))
      )

;;; generate with relative time
(subs
 (rx:time-interval
  (rx:observable-generate-with-relative-time
   1
   (lambda (x) (< x 4))
   (lambda (x) (1+ x))
   (lambda (x) x)
   (lambda (x) (* 100 x)))))



;;; IF
(let ((*should-run* t))
    (subs
     (rx:observable-if
      (lambda () *should-run*)
      (rx:observable-just 111)))
    (setq *should-run* nil)
    (subs
     (rx:observable-if
      (lambda () *should-run*)
      (rx:observable-just 111)
      (rx:observable-just 999)))
    )


;;; PLUCK
(let ((values (vector (mkjso "value" 0) (mkjso "value" 1) (mkjso "value" 2))))
    (subs (rx:pluck (rx:observable-from values) "value" )))



;;; MERGE
(let ((*src0*
        (rx:pluck (rx:time-interval (rx:observable-interval 100)) "interval"))
      (*src1*
        (rx:pluck (rx:time-interval (rx:observable-interval 150)) "interval") ))

    (subs (rx:take (rx:observable-merge *src0* *src1*) 5)))


;;; RANGE
(subs (rx:observable-range 10 12) )


;;; FOR
(subs
 (rx:observable-for
  (vector 1 2 3)
  (lambda (val idx obj) (rx:observable-just val))))


;;; FORK-JOIN
;;; ERROR - too many arguments
;;; ERROR: ERROR: source.subscribe is not a function
(subs
 (rx:observable-fork-join
  (rx:observable-just 42)
  (rx:observable-range 0 10)))
;;;   => #(42 9)
;;;
(subs
 (rx:observable-fork-join
  #(1 2 3 4)
  (rx:observable-range 0 10)
  (lambda (x y z) (+ x y))))
;;;   => 13



;;; FROM

(rx:subscribe (rx:observable-from "abcd")
              (lambda (x) (princ x)))

;;; error!!!
(rx:subscribe (rx:observable-from 1 2 3 4)
              (lambda (x) (print x)))




;;; FROM-CALLBACK

(setf #j:Fs (require "fs"))
(fset 'exists (rx::observable-from-callback #j:Fs:exists))
(rx:subscribe
 (exists "file.txt")
 (lambda (ok)
     (if ok (format t  "proceed file.txt")
         (error "file.txt ?"))))


;;; FROM-EVENTS
(let ((event-emitter))
    (unless #j:EE
        (setf #j:EE(require "events")))
    (setq event-emitter (make_Instance #j:window "EE"))
(subs
 (rx:observable-from-event
  event-emitter
  "data"
  (lambda (foo bar) (mkjso "foo" foo "bar" bar))) )
(funcall ((oget event-emitter "emit" "bind") event-emitter "data" "baz" "quux")) )


;;; FROM-EVENTS-PATTERN
(let ((emitter))
    (unless #j:EE (require "events"))
    (setq emitter (make-new #j:EE))
    (setq src
          (rx:observable-from-event-pattern
           (lambda (handl) (funcall ((oget emitter "addListener" "bind") emitter "data" handl)))
           (lambda (handl) (funcall ((oget emitter "removeListener" "bind") emitter "data" handl)))
           (lambda (foo bar) (mkjso "foo" foo "bar" bar))))
    (subs src)
    (funcall ((oget emitter "emit" "bind") emitter "data" "Baaaz" "Qqqux")) )



;;; FROM-PROMISE
(let ((promise (lambda () (promise:resolve 42))))
    (subs (rx:observable-from-promise promise))
    (setq promise (promise:resolve "Agree"))
    (subs (rx:observable-from-promise promise))
    (setq promise (promise:reject "Unbound"))
    (subs (rx:observable-from-promise promise)) )


;;; FLAT_MAP
(subs
 (rx:flat-map
  (rx:observable-range 1 2)
  (lambda (x idx obs) (rx:observable-range x 2))) )

(subs
 (rx:flat-map (rx:observable-of 1 2 3 4)
              (lambda (x idx obs) (promise:resolve (+ x idx)))) )


(subs
 (rx:flat-map (rx:observable-of 2 3 5)
              (lambda (x idx obs)
                  (vector (* x x) (* x x x) (* x x x x)))
              (lambda (x y idx idy)
                  (mkjso "outer" x "inner" y "outerIdx" idx "innerIdx" idy))))

;;; COMBINE-LATEST
(let* ((src1 (rx:_map (rx:observable-interval 100) (lambda (x idx obs) (concat "First " x))))
       (src2 (rx:_map (rx:observable-interval 100) (lambda (x idx obs) (concat "Second " x))))
       (src (rx:take (rx:observable-combine-latest src1 src2) 2)))
    (subs src ))




;;; TIMER PLUCK

(fset 'pluk-interval
      (lambda (due period)
          (rx:pluck (rx:time-interval (rx:observable-timer due period)) "interval")))

(fset 'take (lambda (how from) (rx:take from how)))

(fset 'subscribe (lambda (source)
                     (rx:subscribe source
                                   (lambda (value) (push value someplace))
                                   (lambda (errmsg) (error "wtf ~a?" errmsg))
                                   (lambda ()  (rx:observable-start-async #'proc:proc-someplace)))))

(subscribe (take 3 (pluck-interval 200 100)))


;;; START
(let ((context (mkjso "value" 42)))
    (subs
     (rx:observable-start
      (lambda () (print (list 'Async-start (oget context "value")))))))


;;; START-ASYNC
(let ((async (lambda () (promise:resolve 42))))
    (subs (rx:observable-start-async async)))


;;; TO-ASYNC
(let* ((fn (lambda (x y) (+ x y)))
       (afn (rx:observable-to-async fn))
       (src))
    (setq src (funcall afn 11 22))
    (subs src))


;;; TAKE-LAST
(subs (rx:take-last (rx:observable-range 0 5) 3))


;;; TAKE-LAST-WITH-TIME
(subs
 (rx:take-last-with-time
  (rx:take (rx:observable-timer 0 1000) 10)
  5000))


;;; TAKE-UNTIL

(subs
 (rx:take-until
  (rx:observable-timer 0 1000)
  (rx:observable-timer 5000)))


;;; TAKE-WHILE
(let ((twp (lambda (val idx obs) (< val 3))))
    (subs
     (rx:take-while (rx:observable-range 1 5) twp) ))

;;; THROTTLE

(let* ((times
         (vector
          (mkjso "value" 0 "time" 100)
          (mkjso "value" 1 "time" 600)
          (mkjso "value" 2 "time" 400)
          (mkjso "value" 3 "time" 900)
          (mkjso "value" 4 "time" 200)))
       (src (rx:flat-map
             (rx:observable-from times)
             (lambda (x idx obs)
                 (rx:delay (rx:observable-of (oget x "value")) (oget x "time"))))))

    ;; => 0,2,3
    (subs (rx:throttle src 300)) )


;;; TIME-INTERVAL
;;; => "0-1000", "1-1000", "2-1000", "3-1000", "4-1000"
(subs
 (rx:take
  (rx:_map (rx:time-interval (rx:observable-timer 0 1000 ))
           (lambda (x idx obs) (concat (oget x "value") "-" (oget x "interval"))))
  5) )


;;; TIMESTAMP
;;; => 1378690776351, 1378690777313, 1378690778316,1378690779317,1378690780319

(subs
 (rx:take (rx:_map (rx:timestamp (rx:observable-timer 0 1000))
                   (lambda (x idx obs) (oget x "timestamp")))
          5))


;;; _FIRST
(subs
 (rx:_first (rx:observable-range 0 12)
            :predicate (lambda (x idx obs) (> x 10))
            :default 42))

(let ((pred (lambda (x idx obs) (if (oddp x) t nil)))
      (src (rx:_first (rx:observable-range 0 10))))
    ;; => 0
    (subs src))

(let ((pred (lambda (x idx obs) (if (oddp x) t nil))))
    ;; => 1
    (rx:subscribe
     (rx:_first (rx:observable-range 0 10) :predicate pred)
     (lambda (x) (print x))))







;;; EOF
