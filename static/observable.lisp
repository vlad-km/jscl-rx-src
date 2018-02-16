;;; -*- mode:lisp; coding:utf-8  -*-

;;; JSCL-RX is a lisp (JSCL) wrapper for RxJS
;;; This file is part of the JSCL-RX
;;; Copyright © 2018 Vladimir Mezentsev



(in-package :rx)

;;;
;;;              Observable static methods
;;;

;;; AMB
;;;
;;;  Propagates the observable sequence or Promise that reacts first. "amb" stands for ambiguous.
;;;
;;; Arguments
;;;
;;;       args (Array|arguments): Observable sources or Promises competing to react first either as an array or arguments.
;;;
;;; Returns
;;;
;;; (Observable): An observable sequence that surfaces any of the given sequences, whichever reacted first.
;;;
;;;
;;;
;;; (rx:observable-amb
;;;     (rx:select (rx:observable-timer 500) (lambda () "foo"))
;;;     (rx:select (rx:observable-timer 200) (lambda () "bar")))
;;;

(defun observable-amb (&rest sources)
    (apply #j:Rx:Observable:amb sources))

(export '(observable-amb))


;;; CASE
;;;
;;; Uses selector to determine which source in sources to use.
;;;
;;; Arguments
;;;
;;; selector (Function): The function which extracts the value for to test in a case statement.
;;;
;;; sources  (Object): An object which has keys which correspond to the case statement labels.
;;;
;;; else     (Observable): The observable sequence that will be run if the sources are not
;;;                        matched.
;;;
;;; Returns
;;;
;;; (Observable): An observable sequence which is determined by a case statement.
;;;
;;; (defparameter sources (mkjso "foo" (rx:observable-just 42) "bar" (rx:observable-just 22)))
;;; (defparameter default  (rx:observable-empty))
;;; (rx:observable-case
;;;      (lambda () "foo")
;;;       sources
;;;       default)

(defun observable-case (selector sources &optional else)
    (#j:Rx:Observable:case selector sources else ))

(export '(observable-case))


;;; CONCAT
;;;
;;; Concatenates all of the specified observable sequences, as long as the previous
;;; observable sequence terminated successfully.
;;;
;;; Arguments
;;;
;;;             args (Array | arguments): Observable sequences or Promises to concatenate.
;;;
;;; Returns
;;;
;;;    (Observable): An observable sequence that contains the elements of each given sequence,
;;;                  in sequential order.
;;;
;;;  (rx:observable-concat
;;;      (rx:observable-just 11)
;;;      (rx:observable-empty)
;;;      (rx:observable-just 55))
;;;

(defun observable-concat (&rest sources)
    (#j:Rx:Observable:concat (list-to-vector sources)))

(export '(observable-concat))


;;; CREATE
;;;
;;;  Creates an observable sequence from a specified subscribe method implementation.
;;;
;;; Arguments
;;;
;;; subscribe (Function): Implementation of the resulting observable sequence's subscribe method,
;;;                       optionally returning a function that will be wrapped in a disposable object.
;;;                       This could also be a disposable object.
;;;
;;; Returns
;;;
;;;           (Observable): The observable sequence with the specified implementation for the subscribe method.
;;;
;;;
;;;
;;; (rx:observable-create
;;;    (lambda (observer)
;;;       (rx:on-next-observer observer 42)
;;;       (rx:on-completed-observer observer)
;;;       (lambda () (format *console* "Disposed~%"))))
;;;
;;; or, if you require no cleanup
;;;
;;; (rx:observable-create
;;;    (lambda (observer)
;;;        (rx:on-next-observer observer 42)
;;;        (rx:on-completed-observer observer)
;;;        (values)))
;;;
;;; or
;;;
;;; (rx:observable-create
;;;    (lambda (observer)
;;;        (rx:on-next-observer observer 42)
;;;        (rx:on-completed-observer observer)
;;;        (rx:disposable-create (lambda () (format *console* "Disposable<br>")))))
;;;

(defun observable-create (observer)
    (#j:Rx:Observable:create observer))

(export '(observable-create))


;;; GENERATE
;;;
;;; Generates an observable sequence in a manner similar to a for loop, using
;;; an optional scheduler to enumerate the values.
;;;
;;; Arguments
;;;
;;;     initial (Any): Initial state.
;;;
;;;     condition (Function): Condition to terminate generation (upon returning false).
;;;
;;;     iterate (Function): Iteration step function.
;;;
;;;     selector (Function): Selector function for results produced in the sequence.
;;;
;;; Returns
;;;
;;;     (Observable): The generated sequence.


(defun observable-generate (initial condition iterate selector)
    (#j:Rx:Observable:generate initial condition iterate selector))

(export '(observable-generate))

;;; GENERATE-WITH-ABSOLUTE-TIME
;;;
;;; Generates an observable sequence by iterating a state from an initial state until the condition fails.
;;;
;;; Arguments
;;;
;;;         initialState (Any): Initial state.
;;;         condition (Function): Condition to terminate generation (upon returning false).
;;;         iterate (Function): Iteration step function.
;;;         resultSelector (Function): Selector function for results produced in the sequence.
;;;         timeSelector (Function): Time selector function to control the speed of values being
;;;                                  produced each iteration, returning Date values.
;;;
;;; Returns
;;;
;;;         (Observable): The generated sequence.
;;;
;;;
;;;  (rx:time-interval
;;;      (rx:observable-generate-with-absolute-time
;;;           1
;;;           (lambda (x) (< x 4))
;;;           (lambda (x) (1+ x))
;;;           (lambda (x) x)
;;;           (lambda (x) (+ (#j:Date:now (* 100 x))))))
;;;

(defun observable-generate-with-absolute-time (initial condition iter result time)
    (#j:Rx:Observable:generateWithAbsoluteTime initial condition iter result time))

(export '(observable-generate-with-absolute-time))


;;; GENERATE-WITH-RELATIVE-TIME
;;;
;;; Generates an observable sequence by iterating a state from an initial state until the condition fails.
;;;
;;; Arguments
;;;
;;;           initial (Any): Initial state.
;;;
;;;           condition (Function): Condition to terminate generation (upon returning false).
;;;
;;;           iterate (Function): Iteration step function.
;;;
;;;           result (Function): Selector function for results produced in the sequence.
;;;
;;;           time   (Function): Time selector function to control the speed of values being
;;;                                    produced each iteration, returning integer values denoting milliseconds.
;;; Returns
;;;
;;;           (Observable): The generated sequence.
;;;
;;;  (rx:time-interval
;;;      (rx:observable-generate-with-absolute-time
;;;           1
;;;           (lambda (x) (< x 4))
;;;           (lambda (x) (1+ x))
;;;           (lambda (x) x)
;;;           (lambda (x) (* 100 x))))
;;;

(defun observable-generate-with-relative-time (initial condition iter result time)
    (#j:Rx:Observable:generateWithRelativeTime initial condition iter result time))

(export '(observable-generate-with-relative-time))


;;; IF
;;;
;;;  Determines whether an observable collection contains values.
;;;
;;; Arguments
;;;
;;;            condition (Function): The condition which determines if the then
;;;                                  or else will be run.
;;;
;;;            then  (Observable): The observable sequence that will be
;;;                                run if the condition function returns true.
;;;
;;;            else  (Observable): The observable sequence that will
;;;                                be run if the condition function returns false.
;;;                                If this is not provided
;;;
;;; Returns
;;;
;;;            (Observable): The generated sequence.
;;;
;;;
;;;
;;;  (defparameter *should-run* t)
;;;
;;;  (rx:observable-if
;;;      (lambda () *should-run*)
;;;      (rx:observable-just 111))
;;;

(defun observable-if (condition then &optional else)
    (#j:Rx:Observable:if condition then else))

(export '(observable-if))


;;; MERGE
;;;
;;; Merges all the observable sequences and Promises into a single observable sequence.
;;;
;;; Arguments
;;;
;;;             args (Array|arguments): Observable sequences to merge into a single sequence.
;;; Returns
;;;
;;;            (Observable): An observable sequence that produces a value after each period.
;;;
;;;
;;; (defparameter *src0*
;;;       (rx:pluck (rx:time-interval (rx:observable-interval 100)) "interval"))
;;;
;;; (defparameter *src1*
;;;       (rx:take (rx:pluck (rx:time-interval (rx:observable-interval 150)) "interval") 5)
;;;
;;; (rx:observable-merge *src0* *src1*)
;;;

(defun observable-merge (&rest args)
    (#j:Rx:Observable:merge (list-to-vector args)))

(export '(observable-merge))




;;; RANGE
;;;
;;; Generates an observable sequence of integral numbers within a specified range,
;;;
;;;    start (Number): The value of the first integer in the sequence.
;;;    count (Number): The number of sequential integers to generate.
;;;
;;; (rx:observable-range 10 20)
;;;
(defun observable-range (start count)
    (#j:Rx:Observable:range start count))

(export '(observable-range))

;;; INTERVAL
;;;
;;; Returns an observable sequence that produces a value after each period.
;;;
;;;     period (Number): Period for producing the values in the resulting sequence
;;;                      (specified as an integer denoting milliseconds).
;;;
;;; (rx:take (rx:time-interval (rx:observable-interval 100)) 3)
;;;

(defun observable-interval (period)
    (#j:Rx:Observable:interval period))

(export '(observable-interval))


;;; REPEAT
;;;
;;; Generates an observable sequence that repeats the given element the specified
;;; number of times
;;;
;;;           value (Any): Element to repeat.
;;;           [repeatCount=-1] (Number):Number of times to repeat the element.
;;;                            If not specified, repeats indefinitely.
;;;
;;; (rx:take (rx:observable-repeat 42 30) 3)

(defun observable-repeat (value count)
    (#j:Rx:Observable:repeat value count))

(export '(observable-repeat))


;;; EMPTY
;;;
;;; Returns an empty observable sequence
;;;
;;;   (rx:observable-empty)
;;;
(defun observable-empty ()
    (#j:Rx:Observable:empty ))

(export '(observable-empty))


;;; JUST
;;; RETURN
;;;
;;; Returns an observable sequence that contains a single element,
;;;
;;;             value (Any): Single element in the resulting observable sequence.
;;;
(defun observable-just (value)
    (#j:Rx:Observable:just value))

(export '(observable-just))


;;; TIMER
;;;
;;; Returns an observable sequence that produces a value after dueTime has elapsed
;;; and then after each period.
;;;
;;;         due (Date|Number):      Absolute (specified as a Date object) or relative time
;;;                                 (specified as an integer denoting milliseconds)
;;;                                 at which to produce the first value.
;;;
;;;         period   (Number):      Period to produce subsequent values
;;;                                 (specified as an integer denoting milliseconds)
;;;                                 If not specified, the resulting timer is not recurring.
;;;
;;;
;;;
;;;   (fset 'take #'rx:take)
;;;   (fset 'pluck #'rx:pluk)
;;;   (fset 'timer0 (curry #'rx:observable-timer 200))
;;;   (fset 'timer1 (rcarry #'rx:observable-timer 100))
;;;   (fset 'from-now (lambda (value) (+ value (#j:Date:now))))
;;;   (take (pluck (rx:time-interval (timer1 (from-now 153))) "interval") 3)
;;;   (take (pluck (rx:time-interval (timer0 100)) "interval") 3)
;;;                 or
;;;
;;;   (fset 'pluk-interval
;;;        (lambda (due period)
;;;              (rx:pluck (rx:time-interval (rx:observable-timer due period)) "interval")))
;;;   (fset 'take (lambda (how from) (rx:take from how)))
;;;   (fset 'subscribe (lambda (source)
;;;                           (rx:subscribe source
;;;                                  (lambda (value) (push value someplace))
;;;                                  (lambda (errmsg) (error "wtf ~a?" errmsg))
;;;                                  (lambda ()  (rx:observable-start-async #'proc:proc-someplace)))))
;;;
;;;   (subscribe (take 3 (pluck-interval 200 100)))
;;;
;;;       or
;;;
;;;   (rx:take (rx:pluck (rx:time-interval (rx:observable-timer 200 100)) "interval") 3)
;;;

(defun observable-timer (due &optional period)
    (#j:Rx:Observable:timer due period))

(export '(observable-timer))



;;; WHILE
;;;
;;; condition (Function): The condition which determines if the source will be repeated.
;;; source (Observable): The observable sequence that will be run if the condition function returns true.
;;;
;;; (defparameter *counter* 0)
;;;
;;; (rx:subscribe (rx:observable-while
;;;                     (lambda () (< (incf *counter*) 5))
;;;                     (rx:observable-just 42))
;;;       (lambda (x) (print x)))
;;;
(defun observable-while (condition source)
    (#j:Rx:Observable:while condition source))

(export '(observable-while))



;;; FOR
;;;
;;; Concatenates the observable sequences or Promises obtained by running the specified
;;; result selector for each element in source.
;;;
;;; Arguments
;;;
;;;   sources (Array): An array of values to turn into an observable sequence.
;;;
;;;   resultSelector (Function): A function to apply to each item in the sources array to turn it
;;;      into an observable sequence. The resultSelector is called with the following information:
;;;
;;;         the value of the element
;;;         the index of the element
;;;         the Observable object being subscribed
;;;
;;; resultSelector must have !!! (lambda (value idx obj) )
;;;
;;; (rx:observable-for
;;;        (vector 1 2 3)
;;;        (lambda (val idx obj) (rx:observable-just val)))
;;;
(defun observable-for (array selector)
    (#j:Rx:Observable:for array selector))

(export '(observable-for))


;;; FORK-JOIN
;;;
;;; Runs all observable sequences in parallel and collect their last elements.
;;;
;;; Arguments
;;;
;;;    args (Arguments | Array): An array or arguments of Observable sequences
;;;                              or Promises to collect the last elements for.
;;;
;;;    resultSelector: Function - The result selector from all the values produced.
;;;                               If not specified, forkJoin will return the results as an array.
;;;
;;;
;;; (rx:observable-fork-join (rx:observable-just 42)
;;;                          (rx:observable-range 0 10))
;;;   => #(42 9)
;;;
;;; (rx:observable-fork-join    #(1 2 3 4)
;;;                             (rx:observable-range 0 10)
;;;                             (lambda (x y obj) (+ x y)))
;;;   => 13
;;;
(defun observable-fork-join (&rest args)
    (#j:Rx:Observable:forkJoin (list-to-vector args)))

(export '(observable-fork-join))


;;; OF
;;;
;;;  Converts arguments to an observable sequence.
;;;
;;;  Arguments
;;;
;;;        args (Arguments): A list of arguments to turn into an Observable sequence.
;;;
;;; Returns
;;;
;;; (Observable): The observable sequence whose elements are pulled from the given arguments.
;;;

(defun observable-of (&rest args)
    (apply #j:Rx:Observable:of args))

(export '(observable-of))


;;; FROM
;;;
;;; This method creates a new Observable sequence from an array-like or iterable object.
;;;
;;; Arguments
;;;
;;;      iterable (Array | Arguments | Iterable): An array-like or iterable object to convert to an Observable sequence.
;;;      [mapFn] (Function): Map function to call on every element of the array.
;;;
;;;
;;; (rx:subscribe (rx:observable-from "abcd")
;;;              (lambda (x) (princ x)))
;;;
;;;
;;; (rx:subscribe
;;;         (rx:observable-from
;;;               (vector 1 2 3 4 5))
;;;               (lambda (x idx) (+ x x))
;;;         (lambda (x) (print x)))
;;;
(defun observable-from (Iterable &optional other)
    (#j:Rx:Observable:from iterable other))

(export '(observable-from))


;;; FROM-CALLBACK
;;;
;;; Converts a callback function to an observable sequence.
;;;
;;; Arguments
;;;
;;;         func (Function): Function with a callback as the last parameter to convert to an Observable sequence.
;;;
;;; Returns
;;;
;;;         (Function): A function, when executed with the required parameters minus the callback,
;;;                     produces an Observable sequence with a single value of the arguments to the callback
;;;                     as an array if no selector given, else the object created by the selector function.
;;;
;;;
;;; (setf #j:Fs (require "fs"))
;;; (fset 'exists (rx:observable-from-callback #j:Fs:exists))
;;; (subscribe (exists "file.txt") (lambda (ok) (if ok (proceed-file "file.txt")
;;;                                                 (error "file.txt ?"))))
;;;
(defun observable-from-callback (fn)
    (#j:Rx:Observable:fromCallback fn))

(export '(observable-from-callback))


;;; FROM-EVENT
;;;
;;; Creates an observable sequence by adding an event listener to the matching DOMElement, jQuery element,
;;; Zepto Element, Angular element, Ember.js element or EventEmitter.
;;;
;;;
;;; Arguments
;;;
;;;  element (Any): The DOMElement, NodeList, jQuery element, Zepto Element,
;;;                 Angular element, Ember.js element or EventEmitter to attach a listener.
;;;                 For Backbone.Marionette this would be the application
;;;                 or an EventAggregator object.
;;;
;;;  eventName (String): The event name to attach the observable sequence.
;;;
;;;  [selector] (Function): A selector which takes the arguments from the event emitter so
;;;                         that you can return a single object.
;;;
;;;  [options] ( Object ) An object of event listener options.
;;;
;;;  Returns
;;;
;;;        (Observable): An observable sequence of events from the specified element
;;;                      and the specified event.
;;;
;;; from DOM
;;;
;;;           (rx:observable-from-event #j:window:document "mousemove")
;;;
;;;
;;;  from EventEmitter:
;;;
;;;           (setf #j:Eve (require "events"))
;;;           (setq event-emittter (make-new #j:Eve))
;;;           subscribe (rx:observable-from-event event-emitter
;;;                                  "data" (lambda (foo bar) (mkjso "foo" foo "bar" bar)))
;;;           (funcall ((oget event-emitter "emit" "bind") event-emitter "data" "baz" "quux"))
;;;
(defun observable-from-event (elt event &optional fn options)
    (#j:Rx:Observable:fromEvent elt event fn options))


(export '(observable-from-event))



;;; FROM-EVENT-PATTERN
;;;
;;; Creates an observable sequence by using the addHandler and removeHandler functions
;;; to add and remove the handlers, with an optional selector function to project
;;; the event arguments.
;;;
;;; Arguments
;;;
;;;             addHandler (Function): The DOMElement, NodeList or EventEmitter to attach a listener.
;;;
;;;             [removeHandler] (Function): The optional function to remove a handler from an emitter.
;;;
;;;             [selector] (Function): A selector which takes the arguments from the event handler
;;;                                    to produce a single item to yield on next.
;;;
;;; Returns
;;;
;;;    An observable sequence of events from the specified element and the specified event.
;;;
;;; (let ((emitter))
;;;    (unless #j:EventEmitter (require "events"))
;;;    (setq emitter (make-new #j:EventEmitter))
;;;    (setq src
;;;          (rx:observable-from-event-pattern
;;;           (lambda (handl) (funcall ((oget emitter "addListener" "bind") emitter "data" handl)))
;;;           (lambda (handl) (funcall ((oget emitter "removeListener" "bind") emitter "data" handl)))
;;;           (lambda (foo bar) (mkjso "foo" foo "bar" bar))))
;;;    (subs src)
;;;    (funcall ((oget emitter "emit" "bind") emitter "data" "Baaaz" "Qqqux")) )
;;;
;;;

(defun observable-from-event-pattern (add-handl &optional rem-handl selector)
    (#j:Rx:Observable:fromEventPattern add-handl rem-handl selector))

(export '(observable-from-event-pattern))



;;; FROM-PROMISE
;;;
;;; Converts a Promises/A+ spec compliant Promise and/or ES2015 compliant Promise
;;; or a factory function which returns said Promise to an Observable sequence.
;;;
;;; Arguments
;;;
;;;             promise|Function: Promise - Promises/A+ spec compliant Promise
;;;                                         to an Observable sequence or a function which
;;;                                         returns a Promise.
;;;
;;; Returns
;;;
;;;            Observable: An Observable sequence which wraps the existing
;;;                        promise success and failure.
;;;
;;;
;;;
;;; (let ((promise-fn (lambda () (promise:resolve 42))))
;;;    (subs (rx:observable-from-promise promise-fn))
;;;
;;;    (setq promise-fn (promise:reject "Unbound"))
;;;    (subs (rx:observable-from-promise promise-fn)) )

(defun observable-from-promise (promise)
    (#j:Rx:Observable:fromPromise promise))

(export '(observable-from-promise))



;;; COMBINE-LATEST
;;;
;;; Merges the specified observable sequences into one observable sequence by using the selector
;;; function whenever any of the observable sequences produces an element. This can be in the form
;;; of an argument list of observables or an array. If the result selector is omitted,
;;; a list with the elements will be yielded.
;;;
;;; Arguments
;;;
;;;       args (arguments): An arguments of Observable sequences.
;;;
;;;       [resultSelector] (Function): Function to invoke whenever either of the
;;;                                    sources produces an element.
;;;                                    If omitted, a list with the elements will be yielded.
;;;
;;; Returns
;;;
;;; (Observable): An observable sequence containing the result of combining elements of the
;;;               sources using the specified result selector function.
;;;
;;;
;;; (setq src1 (rx:select (rx:observable-interval 100) (lambda (x idx obs) (concat "First " x))))
;;; (setq src2 (rx:select (rx:observable-interval 100) (lambda (x idx obs) (concat "Second " x))))
;;; (setq src (rx:take (rx:observable-combine-latest src1 src2) 2))
;;; (rx:subscribe src (lambda (x) (print x))))
;;;

(defun observable-combine-latest (&rest args)
    (apply #j:Rx:Observable:combineLatest args))

(export '(observable-combine-latest))



;;; START
;;;
;;;  Invokes the specified function asynchronously
;;;  surfacing the result through an observable sequence.
;;;
;;; Arguments
;;;
;;;      func (Function): Function to run asynchronously.
;;;
;;; Returns
;;;
;;;      (Observable): An observable sequence exposing the function's result value, or an exception.
;;;
;;;
;;; (let ((context (mkjso "value" 42)))
;;;    (subs
;;;     (rx:observable-start
;;;      (lambda () (print (list 'Async-start (oget context "value")))))))

(defun observable-start (fn)
    (#j:Rx:Observable:start fn))

(export '(observable-start))


;;; START-ASYNC
;;;
;;; Invokes the asynchronous function, surfacing the result through an observable sequence.
;;;
;;; Arguments
;;;
;;;    functionAsync (Function): Asynchronous function which returns a Promise to run.
;;;
;;; Returns
;;;
;;;    (Observable): An observable sequence exposing the function's Promises's value or error.
;;;
;;;
;;; (let ((async (lambda () (promise:resolve 42))))
;;;    (subs (rx:observable-start-async async)))

(defun observable-start-async (fn)
    (#j:Rx:Observable:startAsync fn))

(export '(observable-start-async))


;;; TO-ASYNC
;;;
;;; Converts the function into an asynchronous function. Each invocation of the resulting
;;; asynchronous function causes an invocation of the original synchronous function
;;; on the specified scheduler.
;;;
;;; Arguments
;;;
;;;     func (Function): Function to convert to an asynchronous function.
;;;     [context] (Any): The context for the func parameter to be executed. If not specified, defaults to undefined.
;;;
;;; Returns
;;;
;;;     (Function): Asynchronous function.
;;;
;;;
;;; (let ((fn (lambda (x y) (+ x y)))
;;;      (afn (rx:observable-to-async fn))
;;;      (src))
;;;    (setq src (funcall afn 11 22))
;;;    (subs src))

(defun observable-to-async (fn)
    (#j:Rx:Observable:toAsync fn))

(export '(observable-to-async))




(in-package :cl-user)

;;; EOF
