;;;; guetor.asd

#-asdf3.1 (error "GUETOR requires ASDF 3.1 or later.")
(defsystem "guetor"
  :version "0.0.1"
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "GUETOR stands for guess selector."
  :license  ""
  :class :package-inferred-system
  :depends-on ("guetor/interface")
  :in-order-to ((test-op (load-op "guetor/tests")))
  :perform (test-op (o c) (symbol-call :guetor/tests :suite-tests)))
