(in-package :hu.dwim.sleigh)

;; ghidra/Ghidra/Framework/SoftwareModeling/src/main/antlr/ghidra/sleigh/grammar/

(defvar *sleigh-context*)

#+nil
(hu.dwim.defclass-star:defclass* sleigh-context ()
  ((preprocessor-definitions (make-hash-table :test 'equal))))

(defclass sleigh-context nil
  ((preprocessor-definitions :initform (make-hash-table :test 'equal)
                             :accessor preprocessor-definitions-of :initarg
                             :preprocessor-definitions)))

;;;; Whitespace & comments

(defrule whitespace-char
    (or #\Space #\Tab #\Newline #\Return)
  (:constant nil))

(defrule ws
  (+ whitespace-char)
  (:constant nil))

(defrule opt-ws
  (* (or whitespace-char
         comment))
  (:constant nil))

(defrule comment
  (or
   (and "//" (* (not (or #\Newline #\Return))) (+ (or #\Newline #\Return)))
   (and "#"  (* (not (or #\Newline #\Return))) (+ (or #\Newline #\Return)))
   (and "/*" (* (not "*/")) "*/"))
  (:constant nil))

;;;; Preprocessing

(defrule pp/@include-stmt
  (and "@include" ws string opt-ws)
  (:lambda (x)
    (list :include (third x))))

(defrule pp/@define-stmt
    (and "@define" ws ident ws string)
  (:lambda (x)
    (list :define :name (third x) :value (fifth x))))

(defrule pp/variable-substitution
    (and "$(" ident ")")
  (:lambda (x)
    (break "variable-substitution: ~S" x)
    nil))

(defrule pp/@ifdef-stmt
  (and "@ifdef" ws ident
       (* (and opt-ws pp/statement))
       opt-ws (? (and "@else" (* (and opt-ws pp/statement))))
       opt-ws "@endif")
  (:lambda (x)
    (break)
    (list* :ifdef :condition (nth 2 x) :body (mapcar 'second (nth 3 x))
           (awhen (nth 5 x)
             (list :else (mapcar 'second (second it)))))))

;;; consume exactly one char that is NOT a brace
(defrule non-brace-char
  (and (not (or "{" "}"))                 ; lookahead: next char is not '{' or '}'
       (character-ranges (#\u0000 #\uffff))) ; then consume one char
  (:lambda (char)                          ; return a one-char string
    (string char)))

(defrule brace-block
  (and "{"
       (* (or non-brace-char brace-block))
       "}")
  (:lambda (parts)
    ;; parts => ("{" content-items* "}")
    ;; we want to concatenate content-items (each either a string from non-brace-char
    ;; or a string returned by a nested brace-block)
    (let ((items (second parts)))
      (apply #'concatenate 'string items))))

(defrule pp/with-clause
    (and "with" ws ":" ws ident opt-ws "=" opt-ws number opt-ws
         brace-block)
  (:lambda (x)
    (list :with (nth 4 x)
          :value (nth 8 x)
          :body (first (nth 10 x)))))

(defrule pp/directive-lookahead
  (or "@include"
      "@define"
      "@ifdef"
      "@else"
      (and "with" ws)
      "$("
      ))

(defrule pp/everything-else
    (and (and (not pp/directive-lookahead)
              (not "}")
              )
         (+ (character-ranges (#\u0000 #\uffff))))
  (:lambda (x)
    (break)
    (list :inert (coerce (mapcar #'second x) 'string))))

(defrule pp/statement
    (and opt-ws
         (not "}")
         (or pp/@include-stmt
             pp/@define-stmt
             pp/@ifdef-stmt
             pp/with-clause
             pp/variable-substitution
             pp/everything-else
             ))
  (:lambda (x)
    (second x)))

(defrule pp/statement-list
    (* (and opt-ws pp/statement))
  (:lambda (x)
    (mapcar 'second x)))

(defrule pp/sleigh-file
    (and pp/statement-list
         opt-ws)
  (:lambda (x)
    (first x)))

;;;; Lexical rules

(defrule alpha
  (character-ranges (#\a #\z) (#\A #\Z)))

(defrule digit
  (character-ranges (#\0 #\9)))

(defrule ident
  (and (or alpha "_")
       (* (or alpha digit "_" "-")))
  (:lambda (elements)
    (coerce (list* (first elements) (second elements)) 'string)))

(defrule number
  (+ digit)
  (:lambda (digits) (parse-integer (coerce digits 'string))))

(defrule string
  (and "\"" (* (not "\"")) "\"")
  (:lambda (elements) (coerce (second elements) 'string)))

;;;; Values

(defrule value
  (or string number ident))

(defrule pattern-expr
  (or ident number string))

;;;; Statements

(defrule define-stmt
  (and "define" ws ident "=" value opt-ws ";")
  (:lambda (x) (list :define (nth 2 x) (nth 4 x))))

(defrule define-space-stmt
  (and "define" ws "space" ws ident ws  "type=" ident ws "size=" value opt-ws ";")
  (:lambda (x) (list :define (nth 2 x) (nth 4 x))))

(defrule token-stmt
  (and "token" ws ident opt-ws "{" opt-ws (* field-decl) opt-ws "}")
  (:lambda (x) (list :token (nth 2 x) (nth 6 x))))

(defrule field-decl
  (and ident opt-ws "=" opt-ws number opt-ws ":" opt-ws number opt-ws)
  (:lambda (x) (list :field (first x) :lo (nth 4 x) :hi (nth 8 x))))

(defrule space-stmt
  (and "space" ws ident ws number opt-ws)
  (:lambda (x) (list :space (nth 2 x) (nth 4 x))))

(defrule register-stmt
  (and "register" ws ident opt-ws "{" opt-ws (* reg-def) opt-ws "}")
  (:lambda (x) (list :register (nth 2 x) (nth 6 x))))

(defrule reg-def
  (and ident opt-ws "=" opt-ws number opt-ws)
  (:lambda (x) (list :reg (first x) (nth 4 x))))

(defrule attach-stmt
  (and "attach" ws ident ws ident opt-ws)
  (:lambda (x) (list :attach (nth 2 x) (nth 4 x))))

(defrule macro-stmt
  (and "macro" ws ident opt-ws "{" opt-ws (* macro-line) opt-ws "}")
  (:lambda (x) (list :macro (nth 2 x) (nth 6 x))))

(defrule macro-line
  (and (* (not (or "}" #\Newline #\Return))) (+ (or #\Newline #\Return)))
  (:lambda (x) (coerce (first x) 'string)))

(defrule constructor-stmt
  (and "constructor" ws ident opt-ws "{" opt-ws constructor-body opt-ws "}")
  (:lambda (x) (list :constructor (nth 2 x) (nth 6 x))))

(defrule constructor-body
  (* (or pattern-stmt action-stmt)))

(defrule pattern-stmt
  (and "pattern" ws ident opt-ws "=" opt-ws pattern-expr opt-ws)
  (:lambda (x) (list :pattern (nth 2 x) (nth 6 x))))

(defrule action-stmt
  (and "action" opt-ws "{" opt-ws (* action-line) opt-ws "}")
  (:lambda (x) (list :action (nth 4 x))))

(defrule action-line
  (and (* (not (or "}" #\Newline #\Return))) (+ (or #\Newline #\Return)))
  (:lambda (x) (coerce (first x) 'string)))

;;;; Top-level

(defrule statement
    (and opt-ws
         (or define-stmt
             token-stmt
             space-stmt
             register-stmt
             attach-stmt
             macro-stmt
             constructor-stmt)
         opt-ws))

(defrule statement-list
    (* (and opt-ws statement))
  (:lambda (x)
    (mapcar 'second (mapcar 'second x))))

(defrule sleigh-file
    (and statement-list
         opt-ws)
  (:lambda (x)
    (first x)))

(defun %parse-sleigh-file (file-name-or-string body-fn &key (base-directory (pathname-directory file-name-or-string)))
  (let ((*sleigh-context* (if (boundp '*sleigh-context*)
                              *sleigh-context*
                              (make-instance 'sleigh-context)))
        (input (if (pathnamep file-name-or-string)
                   (uiop:read-file-string file-name-or-string)
                   file-name-or-string)))
    (funcall body-fn input base-directory)
    *sleigh-context*))

(defun preprocess-sleigh-file (file-name-or-string &key (base-directory (pathname-directory file-name-or-string)))
  (%parse-sleigh-file
   file-name-or-string
   (lambda (input base-directory)
     (mapcar (lambda (stmt)
               (case (first stmt)
                 (:include
                  (let ((file-name (second stmt)))
                    (preprocess-sleigh-file
                     (make-pathname :directory base-directory
                                    :defaults (pathname file-name))
                     :base-directory base-directory)))
                 (:with
                  )))
             (print (esrap:parse 'pp/sleigh-file input))))
   :base-directory base-directory)

  *sleigh-context*)

(defun parse-sleigh-file (file-name-or-string
                          &key (base-directory (when (pathnamep file-name-or-string)
                                                 (pathname-directory file-name-or-string))))
  (%parse-sleigh-file
   file-name-or-string
   (lambda (input base-directory)
     (mapcar (lambda (stmt)
               (case (first stmt)
                 (:include
                  (let ((file-name (second stmt)))
                    (parse-sleigh-file
                     (make-pathname :directory base-directory
                                    :defaults (pathname file-name)))))
                 (:with
                  )))
             (print (esrap:parse 'sleigh-file input))))
   :base-directory base-directory)

  *sleigh-context*)

;; (parse-sleigh-file "/home/alendvai/common-lisp/maru/playground/ghidra/Ghidra/Processors/x86/data/languages/x86.slaspec")

(defun x1 ()
  (let ((input "@ifdef IA64
@define SIZE     \"8\"
@define STACKPTR \"RSP\"
@else
@define SIZE     \"4\"
@define STACKPTR \"ESP\"
@endif
"))
    (parse-sleigh-file input :base-directory nil)))

(defun x2 ()
  (preprocess-sleigh-file #p"/home/alendvai/common-lisp/maru/playground/ghidra/Ghidra/Processors/x86/data/languages/x86.slaspec"))

(defun x3 ()
  (let ((input "with : lockprefx=0 {
:AAA			is vexMode=0 & bit64=0 & byte=0x37		{ local car = ((AL & 0xf) > 9) | AF; AL = (AL+6*car)&0xf; AH=AH+car; CF=car; AF=car; }
}"))
    (preprocess-sleigh-file input)))
