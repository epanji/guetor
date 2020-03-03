;;;; guetor/condition.lisp

(defpackage :guetor/condition
  (:nicknames :guetor-cond)
  (:use :cl)
  (:export #:guetor-condition
           #:guetor-error
           #:guetor-warning))

(in-package :guetor/condition)

(define-condition guetor-condition (condition)
  ((message :initarg :message :reader condition-message))
  (:documentation "Base condition for guetor system.")
  (:report (lambda (condition stream)
             (format stream "~A~%" (condition-message condition)))))

(define-condition guetor-error (guetor-condition error)
  ()
  (:documentation "Base error for guetor system."))

(define-condition guetor-warning (guetor-condition warning)
  ()
  (:documentation "Base warning for guetor system."))
