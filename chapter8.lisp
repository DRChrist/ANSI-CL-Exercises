(defpackage "CHAPTER8"
  (:use "COMMON-LISP" "TEXTGENERATION"))

(in-package chapter8)

;;;1.
;;Yes, if they are in different packages

;;;2.
;; "FOO" = three basic chars = 12 bytes?
;; foo = five properties = name (12 bytes?) + package (24 bytes?) + value (4-32 bytes?)
;;                         function (12 bytes?) + property list (0-xxx bytes?) = quite big

;;;3.
;;??

;;;4.
;;DONE

;;;5.
;;How the hell do I do this?
;;I can analyse the frequency of words.
;;I suppose I can try and compare the frequency of the words in the generated text with other texts.
;;So I can compare the result-text and the origin-text for wordfrequency?

;;;6.
;;See textgeneration.lisp
