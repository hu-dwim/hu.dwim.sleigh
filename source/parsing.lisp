(in-package :hu.dwim.sleigh)

;; ghidra/Ghidra/Framework/SoftwareModeling/src/main/antlr/ghidra/sleigh/grammar/

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
       (* (or alpha digit "_" "-")))
  (:lambda (elements)
    (coerce (list* (first elements) (second elements)) 'string)))

(defrule number
  (+ digit)
  (:lambda (digits) (parse-integer (coerce digits 'string))))

(defrule string
  (and "\"" (* (not "\"")) "\"")
  (:lambda (elements) (coerce (second elements) 'string)))

;;;; Statements

(defrule define-stmt
  (and "define" ws ident "=" value opt-ws ";")
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

;;;; Values

(defrule value
  (or string number ident))

(defrule pattern-expr
  (or ident number string))

;;;; Top-level

(defrule statement
    (and opt-ws
         (or with-clause
             include-stmt
             define-stmt
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

(defrule with-clause
  (and "with" ws ":" ws ident opt-ws "=" opt-ws number opt-ws
       "{" (and statement-list opt-ws) "}")
  (:lambda (x)
    (list :with (nth 4 x)
          :value (nth 8 x)
          :body (first (nth 11 x)))))

(defrule include-stmt
  (and "@include" ws string opt-ws)
  (:lambda (x)
    (list :include (third x))))

(defrule sleigh-file
    (and statement-list
         opt-ws)
  (:lambda (x)
    (first x)))

(defun x ()
  (let ((input (uiop:read-file-string "/home/alendvai/common-lisp/maru/playground/ghidra/Ghidra/Processors/x86/data/languages/x86.slaspec")))
    (esrap:parse 'sleigh-file input)))

(defun x1 ()
  (let ((input "\"first \" \"second \" \"third \""))
    (esrap:parse 'sleigh-file input)))
