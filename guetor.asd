;;;; guetor.asd

(asdf:defsystem "guetor"
  :version "0.0.1"
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "GUETOR stands for guess selector."
  :license  ""
  :class package-inferred-system
  :depends-on ("lquery")
  :components ((:file "guetor"))
  :in-order-to ((test-op (test-op "guetor/tests"))))

(asdf:defsystem "guetor/tests"
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "Test system for package GUETOR."
  :license  ""
  :class package-inferred-system
  :depends-on ("guetor" "fiveam")
  :components ((:file "guetor-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :guetor)))
