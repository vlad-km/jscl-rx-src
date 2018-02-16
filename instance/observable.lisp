;;; -*- mode:lisp; coding:utf-8  -*-

;;; JSCL-RX is a lisp (JSCL) wrapper for RxJS
;;; This file is part of the JSCL-RX
;;; Copyright © 2018 Vladimir Mezentsev



(in-package :rx)

;;;
;;;              Observable Instance methods
;;;



;;; SUBSCRIBE
;;;
;;;
(defun subscribe (rxo on &optional error complite)
    (funcall ((oget rxo "subscribe" "bind") rxo on error complite)))


(export '(subscribe))


;;; TAKE
;;;
;;; Returns a specified number of contiguous elements from the start of an observable sequence,
;;; using the specified scheduler for the edge case of take(0).
;;;
;;; Arguments
;;;
;;;    count (Number): The number of elements to return.
;;;
;;; Returns
;;;
;;;    (Observable): An observable sequence that contains the elements before and
;;;                  including the specified index in the input sequence.

(defun take (rxo count)
    (funcall ((oget rxo "take" "bind") rxo count)))

(export '(take))



;;; TAKE-LAST
;;;
;;;    Returns a specified number of contiguous elements from the end of an observable sequence,
;;;    using an optional scheduler to drain the queue.
;;;    This operator accumulates a buffer with a length enough to store elements count elements.
;;;    Upon completion of the source sequence, this buffer is drained on the result sequence.
;;;    This causes the elements to be delayed.
;;;
;;; Arguments
;;;
;;;    count (Number): Number of elements to bypass at the end of the source sequence.
;;;
;;; Returns
;;;
;;;   (Observable): An observable sequence containing the source sequence elements
;;;   except for the bypassed ones at the end.
;;;
;;;

(defun take-last (rxo count)
    (funcall ((oget rxo "takeLast" "bind") rxo count)))

(export '(take-last))

;;; TAKE-LAST-WITH-TIME
;;;
;;; Returns elements within the specified duration from the end of the observable source sequence,
;;;
;;; Arguments
;;;
;;;           duration (Number): Duration for taking elements from the end of the sequence.
;;;
;;; Returns
;;;
;;;          (Observable): An observable sequence with the elements taken during the specified
;;;                        duration from the end of the source sequence.

(defun take-last-with-time (rxo duration)
    (funcall ((oget rxo "takeLastWithTime" "bind") rxo duration)))

(export '(take-last-with-time))


;;; TAKE-UNTIL
;;;
;;;  Returns the values from the source observable sequence until the other observable sequence
;;;  or Promise produces a value.
;;;
;;; Arguments
;;;
;;;          other (Observable | Promise): Observable sequence or Promise that terminates
;;;                                        propagation of elements of the source sequence.
;;; Returns
;;;
;;;       (Observable): An observable sequence containing the elements of the source sequence
;;;                     up to the point the other sequence or Promise interrupted further propagation.
;;;
;;;
;;; (rx:take-until (rx:observable-timer 0 1000) (rx:observable-timer 5000))
;;; => 0,1,2,3,4
;;;

(defun take-until (rxo other)
    (funcall ((oget rxo "takeUntil" "bind") rxo other)))


(export '(take-until))



;;; TAKE-WHILE
;;;
;;; Returns elements from an observable sequence as long as a specified condition is true.
;;;
;;; Arguments
;;;
;;;     predicate (Function): A function to test each source element for a condition.
;;;                           The callback is called with the following information:
;;;                                            the value of the element
;;;                                            the index of the element
;;;                                            the Observable object being subscribed
;;;
;;; Returns
;;;
;;;           (Observable): An observable sequence that contains the elements
;;;                         from the input sequence that occur before the element at which the test no longer passes.
;;;
;;; (setq twp (lambda (val idx obs) (< val 3)))
;;; (rx:take-while (rx:observable-range 1 5) twp)
;;; => 0,1,2
;;;

(defun take-while (rxo predicate)
    (funcall ((oget rxo "takeWhile" "bind") rxo predicate)))

(export '(take-while))




;;; SKIP
;;;
;;; Bypasses a specified number of elements in an observable sequence and then returns the remaining elements.
;;;
;;; Arguments
;;;
;;;              count (Number): The number of elements to skip before returning the remaining elements.
;;;
;;; Returns
;;;
;;;            (Observable): An observable sequence that contains the elements that
;;;                          occur after the specified index in the input sequence.
;;;
;;; (rx:skip (rx:0bservable-range 0 5) 3)
;;; => 3,4
;;;

(defun skip (rxo count)
    (funcall ((oget rxo "skip" "bind") rxo count)))


(export '(skip))


;;; SKIP-LAST
;;;
;;; Bypasses a specified number of elements at the end of an observable sequence.
;;; This operator accumulates a queue with a length enough to store the first count elements.
;;; As more elements are received, elements are taken from the front of the queue and produced
;;; on the result sequence. This causes elements to be delayed.
;;;
;;;
;;; Arguments
;;;
;;;            count (Number): Number of elements to bypass at the end of the source sequence.
;;;
;;; Returns
;;;
;;;             (Observable): An observable sequence containing the source sequence elements
;;;                           except for the bypassed ones at the end.
;;;
;;;
;;; (rx:skip-last (rx:observable-range 0 5) 3)
;;; => 0,1
;;;

(defun skip-last (rxo count)
    (funcall ((oget rxo "skipLast" "bind") rxo count)))

(export '(skip-last))


;;; SKIP-LAST-WITH-TIME
;;;
;;; Bypasses a specified number of elements at the end of an observable sequence.
;;; This operator accumulates a queue with a length enough to store the first count elements.
;;; As more elements are received, elements are taken from the front of the queue and produced
;;; on the result sequence. This causes elements to be delayed.
;;;
;;; Arguments
;;;
;;;         duration (Number): Duration for skipping elements from the end of the sequence.
;;;
;;; Returns
;;;
;;;        (Observable): An observable sequence with the elements skipped during
;;;                      the specified duration from the end of the source sequence.
;;;
;;;
;;; (rx:skip-last-with-time (rx:take (rx:observable-timer 0 1000) 10) 5000)
;;; => 0,1,2,3,4
;;;

(defun SKIP-LAST-WITH-TIME (rxo duration)
    (funcall ((oget rxo "skipLastWithTime" "bind") rxo duration)))

(export '(skip-last-with-time))


;;; SKIP-UNTIL
;;;
;;; Returns the values from the source observable sequence only after the other
;;; observable sequence produces a value.
;;;
;;; Arguments
;;;
;;;       other (Observable | Promise): The observable sequence or Promise that triggers
;;;                                     propagation of elements of the source sequence.
;;; Returns
;;;
;;;       (Observable): An observable sequence containing the elements of the source sequence
;;;                     starting from the point the other sequence triggered propagation.
;;;
;;;
;;; (rx:skip-until (rx:observable-timer 0 1000) (rx:observable-timer 5000))
;;; => 6,7,8
;;;

(defun skip-until (rxo other)
    (funcall ((oget rxo "skipUntil" "bind") rxo other)))

(export '(skip-until))


;;; SKIP-UNTIL-WITH-TIME
;;;
;;;  Skips elements from the observable source sequence until the specified
;;;  start time
;;;
;;;  Errors produced by the source sequence are always forwarded to the result sequence,
;;;  even if the error occurs before the start time.
;;;
;;; Arguments
;;;
;;;          start (Date | Number): Time to start taking elements from the source sequence.
;;;                                 If this value is less than or equal to current time, no elements will be skipped.
;;;
;;; Returns
;;;
;;;          (Observable): An observable sequence with the elements skipped until the specified start time.
;;;
;;;  (rx:skip-until-with-time (rx:observable-timer 0 1000) 5000)
;;;  => 6,7,8

(defun skip-until-with-time (rxo start)
    (funcall ((oget rxo "skipUntilWithTime" "bind") rxo start)))

(export '(skip-until-with-time))


;;; SKIp-WHILE
;;;
;;; Bypasses elements in an observable sequence as long as a specified condition is true
;;; and then returns the remaining elements.
;;;
;;; Arguments
;;;
;;;           predicate (Function): A function to test each source element for a condition.
;;;                                 The callback is called with the following information:
;;;                                       the value of the element
;;;                                       the index of the element
;;;                                       the Observable object being subscribed
;;;
;;; Returns
;;;
;;;          (Observable): An observable sequence that contains the elements
;;;                        from the input sequence starting at the first element in the linear series
;;;                        that does not pass the test specified by predicate.
;;;
;;; (rx:skip-while (rx:observable-range 1 5) (lambda (x idx obs) (< x 3)))
;;; => 3,4,5
;;;
;;;

(defun skip-while (rxo pred)
    (funcall ((oget rxo "skipWhile" "bind") rxo pred)))

(export '(skip-while))




;;; DELAY
;;;
;;; Time shifts the observable sequence by dueTime. The relative time intervals
;;; between the values are preserved.
;;;
;;; or
;;;
;;; Time shifts the observable sequence based on a subscription delay and a delay
;;; selector function for each element.
;;;
;;; Arguments
;;;
;;;
;;; Delay with an absolute or relative time:
;;;
;;;             time (Date | Number): Absolute (specified as a Date object)
;;;                                   or relative time (specified as an integer denoting milliseconds)
;;;                                   by which to shift the observable sequence.
;;;
;;; Delay with a delay selector function:
;;;
;;;             [delays]  (Observable): Sequence indicating the delay for the
;;;                                     subscription to the source.
;;;
;;;             duration (Function): Selector function to retrieve a sequence indicating
;;;                                  the delay for each given element.
;;;
;;;

(defun delay (rxo time &optional duration)
    (funcall ((oget rxo "delay" "bind") rxo time duration)))

(export '(delay))


;;; SELECT \
;;;         ==> RX:_MAP / RX:SELECT
;;; MAP    /
;;;
;;;    Projects each element of an observable sequence into a new form by incorporating the element's index.
;;;
;;;
;;; Arguments
;;;
;;;               selector (Function | Object): Transform function to apply to each
;;;                                             source element or an element to yield. If selector
;;;                                             is a function, it is called with the following information:
;;;                                                   the value of the element
;;;                                                   the index of the element
;;;                                                   the Observable object being subscribed
;;;
;;;  (rx:select (rx:observable-from-event "mousemove")
;;;           (lambda (x idx obs) (oget x "clientX")) )
;;;
;;;  (rx:_map (rx:observable-from-event "mousemove")
;;;           (lambda (x idx obs) (oget x "clientX")) )
;;;

(defun select (rxo fn)
    (funcall ((oget rxo "select" "bind") rxo fn)))

(fset '_map #'select)
(export '(select _map))



;;; SELECT-MANY
;;; FLAT-MAP
;;;
;;; One of the following:
;;;
;;;     i. Projects each element of an observable sequence to an observable sequence and merges
;;;        the resulting observable sequences or Promises or array/iterable into one observable sequence.
;;;
;;;    ii. Projects each element of an observable sequence or Promise to an observable sequence,
;;;        invokes the result selector for the source element and each of the corresponding
;;;        inner sequence's elements, and merges the results into one observable sequence.
;;;
;;;   iii. Projects each element of the source observable sequence to the other observable
;;;        sequence or Promise, or array/iterable and merges the resulting observable
;;;        sequences into one observable sequence.
;;;
;;;
;;; Arguments
;;;
;;;           selector (Function | Iterable | Promise): An Object to project to the sequence or
;;;                              a transform function to apply to each element or an observable sequence
;;;                              to project each element from the source sequence onto.
;;;                              The selector is called with the following information:
;;;
;;;                                        x: the value of the element
;;;                                        idx: the index of the element
;;;                                        obs: the Observable object being subscribed
;;;
;;;          [result] (Function): A transform function to apply to each element of the
;;;                              intermediate sequence. The resultSelector is called with the
;;;                              following information:
;;;
;;;                                        x: the value of the outer element
;;;                                        y: the value of the inner element
;;;                                        idx: the index of the outer element
;;;                                        idy: the index of the inner element
;;;
;;; Returns
;;;
;;; (Observable): An observable sequence whose elements are the result of invoking the one-to-many transform
;;; function collectionSelector on each element of the input sequence and then mapping each of those
;;; sequence elements and their corresponding source element to a result element.
;;;
;;;
;;;
;;; (rx:flat-map
;;;    (rx:observable-range 1 2)
;;;    (lambda (x idx obs) (rx:observable-range x 2)))
;;; => 1,2,2,3
;;;
;;;
;;; (rx:flat-map (rx:observable-of 1 2 3 4)
;;;             (lambda (x idx obs) (promise:resolve (+ x idx))))
;;; => 1,3,5,7
;;;
;;;
;;;
;;; (rx:flat-map (rx:observable-of 2 3 5)
;;;             (lambda (x idx obs)
;;;                 (vector (* x x) (* x x x) (* x x x x)))
;;;             (lambda (x y idx idy)
;;;                 (mkjso "outer" x "inner" y "outerIdx" idx "innerIdx" idy)))

(defun select-many (rxo selector &optional resulter)
    (funcall ((oget rxo "selectMany" "bind") rxo selector resulter)))

(export '(select-many))
(fset 'flat-map #'select-many)
(export '(flat-map))





;;; JOIN  => RX:_JOIN
;;;
;;; Correlates the elements of two sequences based on overlapping durations.
;;;
;;; Arguments
;;;
;;;       right (Observable): The right observable sequence to join elements for.
;;;
;;;       left  (Function): A function to select the duration (expressed as an
;;;                         observable sequence) of each element of the left observable
;;;                         sequence, used to determine overlap.
;;;
;;;       selector (Function): A function to select the duration
;;;                            (expressed as an observable sequence) of each
;;;                            element of the right observable sequence, used to determine overlap.
;;;
;;;       result (Any): A function invoked to compute a result element for any two
;;;                     overlapping elements of the left and right observable
;;;                     sequences. The parameters are as follows:
;;;                                   (Any) Element from the left source for which the overlap occurs.
;;;                                   (Any) Element from the right source for which the overlap occurs.
;;; Returns
;;;
;;;      (Observable): An observable sequence that contains result elements
;;;                    computed from source elements that have an overlapping duration.
;;;
;;; (setq xs (rx:_map (rx:observable-interval 100) (lambda (x) (concat "First-" x))))
;;; (setq ys (rx:_map (rx:observable-interval 100) (lambda (x) (concat "Second-" x))))
;;; (setq src (rx:_join xs
;;;                    ys
;;;                    (lambda () (rx:observable-timer 0))
;;;                    (lambda () (rx:observable-times 0))
;;;                    (lambda (x y) (concat x "-" y))))
;;;
;;; (rx:subscribe (rx:take src 5) (lambda (x) (print x)))
;;;

(defun _join (rxo right left  selector result)
    (funcall ((oget rxo rigth left selector result))))

(export '(_join))


;;; CONCAT => RX:_CONCAT
;;;
;;; Concatenates all the observable sequences. This takes in either an array or variable
;;; arguments to concatenate.
;;;
;;; Arguments
;;;
;;;       args (arguments | Array): An array or arguments of Observable sequences.
;;;
;;; Returns
;;;
;;;        (Observable): An observable sequence that contains the elements of each given sequence, in sequential order.
;;;
;;;
;;; (setq src (rx:_concat
;;;             (rx:observable-just 42)
;;;             (rx:observable-just 56)
;;;             (rx:observable-just 72)))

(defun _concat (rxo &rest args)
    (funcall ((oget rxo "concat" "bind") rxo (list-to-array args))))

(export '(_concat))



;;; FIRST => RX:_FIRST
;;;
;;; Returns the first element of an observable sequence that satisfies the condition in the predicate,
;;; or a default value if no such element exists. If no default value is given, then onError will be called.
;;;
;;; Arguments
;;;
;;; Rx.Observable.prototype.first([predicate], [thisArg], [defaultValue])
;;;
;;;     [predicate] (Function): A predicate function to evaluate for elements in the source sequence.
;;;                             The callback is called with the following information:
;;;                                       the value of the element
;;;                                       the index of the element
;;;                                       the Observable object being subscribed
;;;    [thisArg] (Any): Object to use as this when executing the predicate.
;;;    [defaultValue] (Any): Default value if no such element exists.
;;;

;;;
;;;
;;; without predicate
;;;
;;; (setq pred (lambda (x idx obs) (if (oddp x) t nil)))
;;; (setq src (rx:_first (rx:observable-range 0 10)))
;;; (rx:subscribe src (lambda (x) (print x)))
;;; => 0
;;;
;;; with predicate
;;;
;;; (rx:subscribe (rx:_first (rx:observable-range 0 10) :predicate pred) (lambda (x) (print x)))
;;; => 1
;;;
;;; with default value
;;;
;;; (rx:subscribe (rx:_first (rx:observable-range 0 10)
;;;                          :predicate (lambda (x idx obs) (> x 10))
;;;                          :default 42))
;;;     (lambda (x) (print x)))
;;; => 42
;;;                       or
;;;
;;; (rx:subscribe (rx:_first (rx:observable-range 0 10)
;;;                          :predicate (lambda (x idx obs) (> x 10))
;;;                          :default (lambda (x) 42))
;;;     (lambda (x) (print x)))
;;; => 42
;;;


(defun _first (rxo &key predicate default)
    (rx/fl_common rxo "first" predicate default))

(export '(_first))



;;; LAST => RX:_LAST
;;;
;;; Returns the last element of an observable sequence that satisfies the condition in the
;;; predicate if specified, else the last element. If no element was found and no default
;;; value is specified, onError is called with an error, however if a default value was specified,
;;; it will be yielded via an onNext call.
;;;
;;; Arguments
;;;
;;;Rx.Observable.prototype.last([predicate], [thisArg], [defaultValue])
;;;
;;;      [predicate] (Function): A predicate function to evaluate for elements in the source sequence.
;;;                              The callback is called with the following information:
;;;
;;;                                    the value of the element
;;;                                    the index of the element
;;;                                    the Observable object being subscribed
;;;      [defaultValue] (Any): Default value if no such element exists.
;;;


(defun _last (rxo &key predicate default)
    (rx/fl_common rxo "last" predicate default))

(export '(_last))

;;;
;;; first => (rx/fl_common rxo "first" predicate default)
;;; last  => (rx/fl_common rxo "last" predicate default)
;;;
(defun rx/fl_common (rxo opname predicate default)
    (if default
        (let ((defv (list "defaultValue" default)))
            (funcall ((oget rxo opname "bind")
                      rxo
                      (if predicate
                          (append defv (list "predicate" predicate))
                          defv))))
        (funcall ((oget rxo opname "bind") rxo predicate))))


;;; THROTTLE
;;;
;;; Returns an Observable that emits only the first item emitted by the source Observable
;;; during sequential time windows of a specified duration.
;;;
;;; Arguments
;;;
;;;     windowDuration (Number): Time to wait before emitting another item after emitting
;;;     the last item (specified as an integer denoting milliseconds).
;;;
;;;
;;; Returns
;;;
;;;     (Observable): An Observable that performs the throttle operation.
;;;
;;;
;;; (setq times
;;;      (vector
;;;       (mkjso "value" 0 "time" 100)
;;;       (mkjso "value" 1 "time" 600)
;;;       (mkjso "value" 2 "time" 400)
;;;       (mkjso "value" 3 "time" 900)
;;;       (mkjso "value" 4 "time" 200)))
;;;
;;; (setq src (rx:flat-map
;;;           (rx:observable-from times)
;;;           (lambda (x idx obs)
;;;               (rx:delay (rx:observable-of (oget x "value")) (oget x "time")))))
;;;
;;; (rx:throttle src 300)
;;; => 0,2,3
;;;

(defun throttle (rxo duration)
    (funcall ((oget rxo "throttle" "bind") rxo duration)))


(export '(throttle))


;;; TIME-INTERVAL
;;;
;;; Records the time interval between consecutive values in an observable sequence.
;;;
;;; Arguments
;;;
;;;              None
;;; Returns
;;;
;;;         (Observable): An observable sequence with time interval information on values.
;;;         -> (mkjso "value" seqn "interval" timeinterval)
;;;
;;; (rx:take
;;;  (rx:_map (rx:time-interval (rx:observable-timer 0 1000 ))
;;;          (lambda (x idx obs) (concat (oget x "value") "-" (oget x "interval"))))
;;;  5)
;;;
;;; => "0-1000", "1-1000", "2-1000", "3-1000", "4-1000"
;;;

(defun time-interval (rxo)
    (funcall ((oget rxo "timeInterval" "bind") rxo)))

(export '(time-interval))


;;; TIMEOUT
;;;
;;; Rx.Observable.prototype.timeout(dueTime, [other], [scheduler])
;;;
;;; Rx.Observable.prototype.timeout([firstTimeout], timeoutDurationSelector, [other])
;;;
;;; Returns the source observable sequence or the other observable sequence if dueTime elapses.
;;;
;;; --OR--
;;;
;;; Returns the source observable sequence, switching to the other observable sequence if a timeout is signaled.
;;;
;;; Arguments
;;;
;;; If using a relative or absolute time:
;;;
;;;      dueTime (Date | Number): Absolute (specified as a Date object) or relative time
;;;                               (specified as an integer denoting milliseconds) when a timeout occurs.
;;;
;;;      [other] (Observable | Promise | Error): Observable sequence or Promise to return in case
;;;                               of a timeout. If a string is specified, then an error will be
;;;                               thrown with the given error message.
;;;                               If not specified, a timeout error throwing sequence will be used.
;;;
;;;
;;; If using a timeout duration selector:
;;;
;;;       [firstTimeout] (Observable): Observable sequence that represents the timeout for the first element.
;;;                                    If not provided, this defaults to Rx.Observable.never().
;;;
;;;       timeoutDurationSelector (Function): Selector to retrieve an observable sequence
;;;                                    that represents the timeout between the current element and the next element.
;;;
;;;       [other] (Scheduler):Sequence to return in case of a timeout. If not provided, this is set to Observable.throw
;;;
;;; Returns
;;;
;;;       (Observable): An observable sequence with time interval information on values.
;;;
;;;

;;; (rx:timeout (rx:delay 5000 (rx:observable-just 42)) 200)
;;; => Error:TIMEOUT
;;;
;;; (setq src (rx:delay (rx:observable-just 42) 5000))
;;; (rx:timeout src 200 (rx:observable-empty))
;;; => "Completed"
;;;
;;;
;;; (setq src (rx:delay (rx:observable-just 42) 5000))
;;; (rx:timeout src (promise:resolve 42) 200)
;;; => 42
;;;
;;;
;;; (setq times (vector 200 300 3550 400))
;;; (setq src (rx:_map
;;;               (rx:observable-for times
;;;                        (lambda (x idx obs) (rx:observable-timer x) ))
;;;               (lambda (x idx obs) idx)))
;;;
;;; without a first timeout
;;;
;;; (rx:timeout src (lambda (x idx bs)
;;;                    (rx:observable-timer 400)))
;;; => 0,1,2

(defun timeout (rxo due other)
    (funcall ((oget rxo "timeout" "bind") rxo due other)))

(export '(timeout))



;;; TIMESTAMP
;;;
;;; Records the timestamp for each value in an observable sequence.
;;;
;;; Arguments
;;;
;;;
;;; Returns
;;;
;;;       (Observable): An observable sequence with timestamp information on values.
;;;
;;;
;;;
;;; (rx:take (rx:_map (rx:timestamp (rx:observable-timer 0 1000))
;;;                  (lambda (x idx obs) (oget x "timestamp")))
;;;         5)
;;; => 1378690776351, 1378690777313, 1378690778316,1378690779317,1378690780319

(defun timestamp (rxo)
    (funcall ((oget rxo "timestamp" "bind") rxo)))

(export '(timestamp))



;;; PLUCK
;;;
;;; Returns an Observable containing the value of a specified nested property from all
;;; elements in the Observable sequence. If a property can't be resolved, it will return
;;; undefined for that value.
;;;
;;; Arguments
;;;
;;;            property (String): The property or properties to pluck. pluck accepts
;;;                               an unlimited number of nested property parameters.
;;;
;;; Returns
;;;
;;;            (Observable): Returns a new Observable sequence of property values.
;;;
;;;
;;;
;;; (let ((values (vector (mkjso "value" 0) (mkjso "value" 1) (mkjso "value" 2))))
;;;      (subs (rx:pluck (rx:observable-from values) "value" )))
;;;

(defun pluck (rxo property)
    (funcall ((oget rxo "pluck" "bind") rxo property)))

(export '(pluck))


(in-package :cl-user)

;;; EOF
