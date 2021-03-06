;;;; X12.asd

(asdf:defsystem #:x12
  :depends-on (
               #:flexi-streams
               #:rutils
               #:rutilsx
               )
  :serial t
  :description "Describe X12 here"
  :author "Fila Kolodny <fskolodny@gmail.com>"
  :license "BSD"
  :components ((:file "package")
               (:file "variables")
               (:file "classes")
               (:file "methods")
               (:file "functions")
               (:file "x12")))

