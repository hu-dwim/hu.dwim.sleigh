(uiop:define-package :hu.dwim.sleigh.preprocessing
  (:use #:common-lisp
        #:alexandria
        #:anaphora
        #:esrap
        )
  (:export #:preprocess-sleigh-file))

(uiop:define-package :hu.dwim.sleigh
  (:use #:common-lisp
        #:alexandria
        #:anaphora
        #:esrap
        )
  (:export))
