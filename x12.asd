;;;; X12.asd

(asdf:defsystem #:x12
  :depends-on (
               #:rutilsx
               )
  :serial t
  :description "Describe X12 here"
  :author "Fila Kolodny <fskolodny@gmail.com>"
  :license "BSD"
  :components ((:file "package")
               (:file "x12")))

