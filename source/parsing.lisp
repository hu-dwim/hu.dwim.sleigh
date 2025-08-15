(in-package :hu.dwim.sleigh)

;; ghidra/Ghidra/Framework/SoftwareModeling/src/main/antlr/ghidra/sleigh/grammar/

(defvar *context*)

#+nil
(hu.dwim.defclass-star:defclass* sleigh-context ()
  ((definitions (make-hash-table :test 'equal))))

(defclass context nil
  ((definitions :initform (make-hash-table :test 'equal)
                :accessor definitions-of :initarg
                :definitions)))

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

;;;; Lexical rules

(defrule alpha
  (character-ranges (#\a #\z) (#\A #\Z)))

(defrule digit
  (character-ranges (#\0 #\9)))

(defrule ident
  (and (or alpha "_")
       (* (or alpha digit (character-ranges #\_ #\-))))
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

;;;; Define statements

(defrule define-token-stmt
  (and "define token" ws ident ws "(" number ")" (* (not ";")) ";")
  (:lambda (x)
    (list :define-token (nth 2 x) (nth 5 x) (nth 7 x))))

(defrule define-space-stmt
  (and "define space" ws ident ws  "type=" ident ws "size=" number opt-ws (? "default") opt-ws ";")
  (:lambda (x)
    (list :define-space (nth 2 x) (nth 4 x) (nth 7 x) (nth 9 x))))

(defrule define-register-stmt
  (and "define register" ws ident ws  "offset=" ident ws "size=" number opt-ws ";")
  (:lambda (x)
    (list :define-register (nth 2 x) (nth 4 x) (nth 7 x))))

(defrule define-value-stmt
  (and "define" ws ident "=" value opt-ws ";")
  (:lambda (x) (list :define (nth 2 x) (nth 4 x))))

;;;; Other statements

(defrule token-stmt
    (and "token" ws ident opt-ws "{" opt-ws (* field-decl) opt-ws "}")
  (:lambda (x) (list :token (nth 2 x) (nth 6 x))))

(defrule field-decl
  (and ident opt-ws "=" opt-ws number opt-ws ":" opt-ws number opt-ws)
  (:lambda (x) (list :field (first x) :lo (nth 4 x) :hi (nth 8 x))))

;; (defrule space-stmt
;;   (and "space" ws ident ws number opt-ws)
;;   (:lambda (x) (list :space (nth 2 x) (nth 4 x))))

;; (defrule register-stmt
;;   (and "register" ws ident opt-ws "{" opt-ws (* reg-def) opt-ws "}")
;;   (:lambda (x) (list :register (nth 2 x) (nth 6 x))))

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
         (or define-token-stmt
             define-space-stmt
             define-register-stmt
             define-stmt
             ;;token-stmt
             ;;space-stmt
             ;;register-stmt
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

;; (defun %parse-sleigh-file (file-name-or-string body-fn &key (base-directory (pathname-directory file-name-or-string)))
;;   (let ((*context* (if (boundp '*context*)
;;                        *context*
;;                        (make-instance 'context)))
;;         (input (if (pathnamep file-name-or-string)
;;                    (uiop:read-file-string file-name-or-string)
;;                    file-name-or-string)))
;;     (setf input (hu.dwim.sleigh.preprocessing:preprocess-sleigh-file input :base-directory base-directory))
;;     (funcall body-fn input base-directory)
;;     *context*))

(defun parse-sleigh-file (file-name-or-string
                          &key (base-directory (when (pathnamep file-name-or-string)
                                                 (pathname-directory file-name-or-string))))
  (let* ((*context* (if (boundp '*context*)
                        *context*
                      (make-instance 'context)))
         (input (if (pathnamep file-name-or-string)
                    (uiop:read-file-string file-name-or-string)
                  file-name-or-string))
         (preprocessed (hu.dwim.sleigh.preprocessing:preprocess-sleigh-file input :base-directory base-directory))
         (parsed (esrap:parse 'sleigh-file preprocessed)))
    (mapcar (lambda (stmt)
              (ecase (first stmt)
                (:with
                 )))
            parsed)))

;; (parse-sleigh-file #p"/home/alendvai/common-lisp/maru/playground/ghidra/Ghidra/Processors/x86/data/languages/x86.slaspec")
;; (parse-sleigh-file #p"/home/alendvai/common-lisp/maru/playground/ghidra/Ghidra/Processors/x86/data/languages/ia.sinc")

(defun x3 ()
  (let ((input "@ifdef IA64
@define SIZE     \"8\"
@define STACKPTR \"RSP\"
@else
@define SIZE     \"4\"
@define STACKPTR \"ESP\"
@endif
"))
    (parse-sleigh-file input)))

(defun x4 ()
  (let ((input "@ifdef IA64
:POPCNT Reg64, rm64      is $(LONGMODE_ON) & vexMode=0 & opsize=2 & $(PRE_F3) & $(REX_W) & byte=0x0F; byte=0xB8; Reg64 ... & rm64 { popcountflags(rm64); Reg64 = popcount(rm64); }
@endif

# The following
#@ifdef IA64
#:J^cc rel32     is
#@endif

postfix
"))
    (parse-sleigh-file input)))

(defun x5 ()
  (let ((input "prefix

macro ptr4(r,x) {
@ifdef IA64
  r = zext(x);
@else
  r = x;
@endif
}

postfix"))
    (parse-sleigh-file input)))

(defun test ()
  (let ((tests
          `(("

some inert, non-comment stuff

@ifdef IA64
@define SIZE     \"8\"
@else
@define SIZE     \"4\"
@endif
"
             ((:INERT "some inert, non-comment stuff

")
              (:IFDEF :CONDITION "IA64" :BODY ((:DEFINE :NAME "SIZE" :VALUE "8")) :ELSE
                      ((:DEFINE :NAME "SIZE" :VALUE "4")))))
            ("# SLA specification file for Intel x86
@ifdef IA64
@define SIZE     \"8\"
@else
@define SIZE     \"4\"
@endif

define endian=little;

# General purpose registers

@ifdef IA64
define register...
@else
define register...
@endif

zork
"
             ((:IFDEF :CONDITION "IA64"
                      :BODY ((:DEFINE :NAME "SIZE" :VALUE "8"))
                      :ELSE ((:DEFINE :NAME "SIZE" :VALUE "4")))
              (:INERT "define endian=little;

")
              (:IFDEF :CONDITION "IA64" :BODY
                      ((:INERT "define register...
"))
                      :ELSE
                      ((:INERT "define register...
")))
              (:INERT "zork
"))))))
    (mapcar (lambda (el)
              (destructuring-bind (input expected) el
                (multiple-value-bind (context ast)
                    (parse-sleigh-file input)
                  (declare (ignore context))
                  (if (equal ast expected)
                      t
                      (progn
                        (break "Test failed: ~S~%*** result:~%~S~%*** expected:~%~S"
                               input ast expected)
                        input)))))
            tests)))
