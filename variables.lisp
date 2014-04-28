(in-package #:x12)
(named-readtables:in-readtable rutils-readtable)

(defvar s (char-code #\^))
(defvar r (char-code #\~))
(defvar f (char-code #\*))
(defvar whitespace (coerce #v((char-code #\space) (char-code #\return)
                      (char-code #\newline) (char-code #\tab)
                      ) 'list))
