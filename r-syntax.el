;;; r-syntax.el --- Utils to work with R code
;; 
;; Copyright (C) 2016 Lionel Henry, Vitalie Spinu
;; 
;; Author: Lionel Henry <lionel.hry@gmail.com>
;; Created: 3 Jul 2016
;; 
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/
;; 
;;; Commentary:
;; 
;; API is not yet stable.
;; 
;;; Code:

(require 'regexp-opt)
(eval-when-compile
  (require 'cl-lib))

(defvar r-symbol-regexp "\\(\\sw\\|\\s_\\)"
  "The regular expression for matching an R symbol")


;;*;; Un-categorized yet

(defun r-containing-sexp-position ()
  (cadr (syntax-ppss)))

(defun r-line-end-position ()
  "Like (line-end-position) but stops at comments"
  (save-excursion
    (or (and (re-search-forward "#" (line-end-position) t)
             (match-beginning 0))
        (line-end-position))))

(defun r-blink-region (start end)
  (when ess-blink-region
    (move-overlay ess-current-region-overlay start end)
    (run-with-timer ess-blink-delay nil
                    (lambda ()
                      (delete-overlay ess-current-region-overlay)))))

(defun r-goto-line (line)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))))


;;*;; Utils

;; The three following wrappers return t if successful, nil on error
(defun r-backward-sexp (&optional N)
  (r-forward-sexp (- (or N 1))))

(defun r-forward-sexp (&optional N)
  (or N (setq N 1))
  (condition-case nil
      (prog1 t
        (goto-char (or (scan-sexps (point) N)
                       (buffer-end N))))
    (error nil)))

(defun r-up-list (&optional N)
  (condition-case nil
      (progn (up-list N) t)
    (error nil)))

(defun r-forward-char (&optional N)
  (unless (= (point) (point-max))
    (forward-char (or N 1))
    t))

(defun r-backward-char (&optional N)
  (unless (= (point) (point-min))
    (forward-char (- (or N 1)))
    t))

(defun r-goto-char (pos)
  "Go to `pos' if it is non-nil.
If `pos' is nil, return nil.  Otherwise return `pos' itself."
  (when pos
    (goto-char pos)))

(defun r-looking-at (regex &optional newlines)
  "Like `looking-at' but consumes blanks and comments first."
  (save-excursion
    (r-skip-blanks-forward newlines)
    (looking-at regex)))

(defun r-back-to-indentation ()
  "Move point to the first non-whitespace character on this line.
This non-interactive version of (back-to-indentation) should not
be advised"
  (beginning-of-line 1)
  (skip-syntax-forward " " (line-end-position))
  ;; Move back over chars that have whitespace syntax but have the p flag.
  (backward-prefix-chars))

(defmacro r-save-excursion-when-nil (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(let ((orig-point (point)))
     (cond ((progn ,@body))
           (t (prog1 nil
                (goto-char orig-point))))))

(defmacro r-while (test &rest body)
  "Like (while) but return `t' when body gets executed once."
  (declare (indent 1)
           (debug (&rest form)))
  `(let (executed)
     (while ,test
       (setq executed t)
       ,@body)
     executed))

(defmacro r-at-indent-point (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(save-excursion
     (goto-char indent-point)
     (r-back-to-indentation)
     (progn ,@body)))

(defmacro r-at-containing-sexp (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  '(when (not (bound-and-true-p containing-sexp))
     (error "Internal error: containing-sexp is nil or undefined"))
  `(save-excursion
     (goto-char containing-sexp)
     (progn ,@body)))

(defmacro r-any (&rest forms)
  "Evaluates all arguments and return non-nil if one of the
arguments is non-nil. This is useful to trigger
side-effects. FORMS follows the same syntax as arguments to
`(cond)'."
  (declare (indent 0) (debug nil))
  `(let ((forms (list ,@(mapcar (lambda (form) `(progn ,@form)) forms))))
     (cl-some 'identity (mapcar 'eval forms))))

(defun r-char-syntax (string)
  (char-to-string (char-syntax (string-to-char string))))


;;*;; Tokenisation

(defun r-token-type (token) (car (nth 0 token)))
(defun r-token-value (token) (cdr (nth 0 token)))
(defun r-token-start (token) (car (nth 1 token)))
(defun r-token-end (token) (cdr (nth 1 token)))

(defun r-token-refined-type (token)
  (r-token-type (r-refine-token token)))

(defun r-token-after (&optional token)
  "Returns next token.
Cons cell containing the token type and string representation."
  (save-excursion
    (when token
      (goto-char (r-token-end token)))
    (r-jump-token)))

(defun r-token-before (&optional token)
  "Returns previous token.
Cons cell containing the token type and string representation."
  (save-excursion
    (when token
      (goto-char (r-token-start token)))
    (r-climb-token)))

(defun r-climb-token (&optional type string)
  (r-save-excursion-when-nil
    (r-escape-comment)
    (r-skip-blanks-backward t)
    (let ((token (or (r-climb-token--back)
                     (r-climb-token--back-and-forth)
                     (error "Internal error: Backward tokenization failed:\n%s"
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))))))
      (if (or type string)
          (when (r-token= token type string)
            token)
        token))))

(defun r-token--cons (type value)
  (if (eq type 'self)
      (cons value nil)
    (cons type value)))

(defun r-climb-token--back ()
  (let* ((token-end (point))
         (token-type (if (= (point) (point-min))
                         "buffer-start"
                       (r-climb-token--operator)))
         (token-value (buffer-substring-no-properties (point) token-end)))
    (unless (null token-type)
      (list (r-token--cons token-type token-value)
            (cons (point) token-end)))))

;; Difficult to use regexps here because we want to match greedily
;; backward
(defun r-climb-token--operator ()
  (when (pcase (char-before)
          ((or ?+ ?/ ?^ ?~ ?? ?!)
           (r-backward-char))
          (`?=
           (prog1 (r-backward-char)
             (or (r-climb-token--char ?=)
                 (r-climb-token--char ?!)
                 (r-climb-token--char ?:)
                 (r-climb-token--char ?>)
                 (r-climb-token--char ?<))))
          ((or ?& ?| ?* ?@ ?$)
           (prog1 (r-backward-char)
             (r-climb-token--char (char-after))))
          (`?<
           (r-backward-char))
          (`?>
           (prog1 (r-backward-char)
             (or (r-climb-token--char ?-)
                 (and (looking-back "->" (- (point) 2))
                      (goto-char (- (point) 2))))))
          (`?-
           (prog1 (r-backward-char)
             (r-climb-token--char ?< ?<)))
          (`?:
           (prog1 (r-backward-char)
             (r-climb-token--char ?: ?:))))
    'self))

(defsubst r-climb-token--char (&rest chars)
  (r-while (and chars
                (eq (char-before) (car chars))
                (r-backward-char))
    (setq chars (cdr chars))))

(defun r-climb-token--back-and-forth ()
  (let ((limit (point)))
    (when (r-skip-token-backward)
      (save-restriction
        (narrow-to-region (point) limit)
        (r-token-after)))))

(defun r-skip-token-backward ()
  (r-save-excursion-when-nil
    (cond
     ;; Punctuation
     ((memq (char-before) '(?, ?\;))
      (r-backward-char))
     ;; Quoting delimiters
     ((memq (char-syntax (char-before)) '(?\" ?$))
      (r-backward-sexp))
     ;; Syntaxic delimiters
     ((memq (char-syntax (char-before)) '(?\( ?\)))
      (prog1 (r-backward-char)
        ;; Also skip double brackets
        (r-save-excursion-when-nil
          (when (let ((current-delim (char-after)))
                  (r-skip-blanks-backward)
                  (and (memq (char-before) '(?\[ ?\]))
                       (eq current-delim (char-before))))
            (r-backward-char)))))
     ;; Identifiers and numbers
     ((/= (skip-syntax-backward "w_") 0)))))

(defun r-jump-token (&optional type string)
  "Consume a token forward.
Returns a cons cell containing the token type and the token
string content. Returns nil when the end of the buffer is
reached."
  (r-save-excursion-when-nil
    (r-skip-blanks-forward t)
    (let* ((token-start (point))
           (token-type (or (r-jump-token--regexps)
                           (r-jump-token--literal)
                           (r-jump-token--infix-op)
                           (r-jump-token--punctuation)
                           (error "Internal error: Forward tokenization failed:\n%s"
                                  (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))
           (token-value (buffer-substring-no-properties token-start (point))))
      (let ((token (list (r-token--cons token-type token-value)
                         (cons token-start (point)))))
        (if (or type string)
            (when (r-token= token type string)
              token)
          token)))))

(defun r-jump-token--literal ()
  (cond
   ;; Simply assume anything starting with a digit is a number. May be
   ;; too liberal but takes care of fractional numbers, integers such
   ;; as 10L, etc. False positives are not valid R code anyway.
   ((looking-at "[0-9]")
    (r-forward-sexp)
    "number")
   ((or (looking-at "\\sw\\|\\s_")
        (eq (char-after) ?`))
    (r-forward-sexp)
    "identifier")
   ((memq (char-after) '(?\" ?\'))
    (r-forward-sexp)
    "string")))

(defun r-jump-token--punctuation ()
  (or (when (= (point) (point-max))
        "buffer-end")
      (pcase (char-after)
        (`?\;
         (forward-char)
         'self)
        (`?,
         (forward-char)
         ;; Treat blanks after comma as part of an argument
         (r-skip-blanks-forward t)
         ","))))

(defvar r-prefix-keywords-list
  '("if" "for" "while" "function"))

(defvar r-keywords-list
  (append r-prefix-keywords-list '("else")))

(defvar r-delimiters-list
  '("(" ")" "{" "}" "[" "]" "[[" "]]"))

(defvar r-operators-list
  '("+" "-" "*" "/" "%%" "**" "^"
    "&" "&&" "|" "||" "!" "?" "~"
    "==" "!=" "<" "<=" ">=" ">"
    "=" "<-" "<<-" "->" "->>"
    "$" "@" ":" "::" ":::" ":="))

(defvar r-keywords-re
  (concat (regexp-opt r-keywords-list) "\\_>"))

(defvar r-delimiters-re
  (regexp-opt r-delimiters-list))

(defvar r-operators-re
  (regexp-opt r-operators-list))

(defun r-jump-token--regexps ()
  (when (or (looking-at r-keywords-re)
            (looking-at r-delimiters-re)
            (looking-at r-operators-re))
    (goto-char (match-end 0))
    'self))

(defun r-jump-token--infix-op ()
  (or (when (looking-at r-operators-re)
        (goto-char (match-end 0))
        'self)
      (when (eq (char-after) ?%)
        (r-forward-sexp)
        "%infix%")))

(defun r-escape-token ()
  (r-escape-comment)
  (r-skip-blanks-forward)
  (or (r-escape-string)
      (when (r-token-delimiter-p (r-token-after))
        (prog1 t
          (mapc (lambda (delims)
                  (while (and (r-token-after= nil delims)
                              (eq (char-before) (string-to-char
                                                 (car delim))))
                    (r-backward-char)))
                '(("[" "[[") ("]" "]]")))))
      (r-token-after= '("," ";"))
      (and (r-token-after= "identifier")
           (not (memq (char-syntax (char-before)) '(?w ?_))))
      (progn (/= (skip-syntax-backward ".") 0)
             (r-token-operator-p (r-token-after)))
      (/= (skip-syntax-backward "w_") 0)))

(defun r-refine-token (token)
  (let ((refined-type
         (pcase (r-token-type token)
           ;; Parameter assignment
           ("="
            (save-excursion
              (goto-char (r-token-start token))
              (let ((containing-sexp (r-containing-sexp-position)))
                (when (and containing-sexp
                           (r-at-containing-sexp
                             (and (r-token-after= "(")
                                  (r-token-before= '("identifier" "string"))))
                           (save-excursion
                             (and (r-climb-token)
                                  (r-token-before= '("," "(")))))
                  "param-assign"))))
           ;; Quoted identifiers
           ("string"
            (when (or
                   ;; Quoted parameter names
                   (r-refined-token= (r-token-after) "param-assign")
                   ;; Quoted call names
                   (r-token-after= "("))
              "identifier"))
           ((or "(" ")")
            (or (save-excursion
                  (if (r-token-close-delimiter-p token)
                      (r-climb-paired-delims nil token)
                    (goto-char (r-token-start token)))
                  (when (r-token-keyword-p (r-token-before))
                    "prefixed-expr-delimiter"))
                ;; Fixme: probably too crude. Better handled in parser
                (when (r-token= token ")")
                  (save-excursion
                    (r-climb-paired-delims ")" token)
                    (when (r-token-before= '("identifier" "string" ")" "]" "]]" "}"))
                      "argslist-delimiter")))))
           ((or "{" "}")
            (save-excursion
              (unless (r-climb-paired-delims "}" token)
                (goto-char (r-token-start token)))
              (when (r-refined-token= (r-token-before) "prefixed-expr-delimiter")
                "prefixed-expr-delimiter"))))))
    (if refined-type
        (list (cons refined-type (r-token-value token))
              (nth 1 token))
      token)))

(defun r-token-balancing-delim (token)
  (pcase (r-token-type token)
    (`"(" ")")
    (`")" "(")
    (`"[" "]")
    (`"]" "[")
    (`"[[" "]]")
    (`"]]" "[[")))


;;;*;;; Token predicates

(defun r-token= (token &optional type string)
  (when (and (null type)
             (null string))
    (error "No condition supplied"))
  (let ((type (if (stringp type) (list type) type))
        (string (if (stringp string) (list string) string)))
    (and (if type (member (r-token-type token) type) t)
         (if string (member (r-token-value token) string) t))))

(defun r-refined-token= (token type &optional string)
  (r-token= (r-refine-token token) type string))

(defun r-token-after= (type &optional string)
  (r-token= (r-token-after) type string))

(defun r-token-before= (type &optional string)
  (r-token= (r-token-before) type string))

(defun r-token-open-delimiter-p (token)
  (string= (r-char-syntax (r-token-type token)) "("))

(defun r-token-close-delimiter-p (token)
  (string= (r-char-syntax (r-token-type token)) ")"))

(defun r-token-delimiter-p (token)
  (or (r-token-open-delimiter-p token)
      (r-token-close-delimiter-p token)))

(defun r-token-operator-p (token &optional strict)
  (and (or (member (r-token-type token) r-operators-list)
           (string= (r-token-type token) "%infix%"))
       (or (null strict)
           (not (r-refined-token= token "param-assign")))))

(defun r-token-keyword-p (token)
  (member (r-token-type token) r-keywords-list))


;;;*;;; Tokens properties and accessors

(defun r-token-make-hash (&rest specs)
  (let ((table (make-hash-table :test #'equal)))
    (mapc (lambda (spec)
            ;; alist
            (if (listp (cdr spec))
                (mapc (lambda (cell)
                        (puthash (car cell) (cdr cell) table))
                      spec)
              ;; Cons cell
              (mapc (lambda (token)
                      (puthash token (cdr spec) table))
                    (car spec))))
          specs)
    table))

(defvar r-token-r-powers-delimiters
  '(("("  . 100)
    ("["  . 100)
    ("[[" . 100)))

(defvar r-token-r-powers-operator
  '(("?"       .  5)
    ("else"    .  8)
    ("<-"      . 10)
    ("<<-"     . 10)
    ("="       . 15)
    ("->"      . 20)
    ("->>"     . 20)
    ("~"       . 25)
    ("|"       . 30)
    ("||"      . 30)
    ("&"       . 35)
    ("&&"      . 35)
    ("!"       . 40)
    ("<"       . 45)
    (">"       . 45)
    ("<="      . 45)
    (">="      . 45)
    ("=="      . 45)
    ("+"       . 50)
    ("-"       . 50)
    ("*"       . 55)
    ("/"       . 55)
    ("%infix%" . 60)
    (":"       . 65)
    ("^"       . 70)
    ("$"       . 75)
    ("@"       . 75)
    ("::"      . 80)
    (":::"     . 80)))

(defvar r-token-r-power-table
  (r-token-make-hash r-token-r-powers-operator
                     r-token-r-powers-delimiters))

(defvar r-token-r-right-powers-operator
  '((")"  . 1)))

(defvar r-token-r-right-power-table
  (r-token-make-hash r-token-r-powers-operator
                     r-token-r-right-powers-operator))

(defvar r-token-r-nud-table
  (r-token-make-hash
   '(("identifier" . identity)
     ("literal" . identity)
     ("number" . identity)
     ("function" . identity)
     ("if" . identity)
     ("while" . identity)
     ("for" . identity))
   '(("(" . r-parser-nud-block)
     ("{" . r-parser-nud-block))))

(defvar r-token-r-rnud-table
  (r-token-make-hash
   '(("identifier" . identity)
     ("literal" . identity)
     ("number" . identity))
   '((")" . r-parser-rnud-paren)
     ("}" . r-parser-nud-block))))

(defvar r-token-r-leds-operator
  (let ((operators-list (append '("%infix%" "else") r-operators-list)))
    (cons operators-list #'r-parser-led-lassoc)))

(defvar r-token-r-leds-delimiter
  '(("(" . r-parser-led-funcall)
    ("[" . r-parser-led-funcall)
    ("[[" . r-parser-led-funcall)))

(defvar r-token-r-led-table
  (r-token-make-hash r-token-r-leds-operator
                     r-token-r-leds-delimiter))

(defvar r-token-r-rid-table
  (r-token-make-hash
   '((")" . r-parser-rid-expr-prefix))))


;;;*;;; Nud, led and rid functions

(defun r-parser-nud-block (prefix-token)
  (let ((right (list (cons "TODO" nil))))
    (r-parser-advance-pair nil prefix-token)
    (r-node (cons "block" nil)
            (cons (r-token-start prefix-token) (point))
            (list prefix-token right))))

(defun r-parser-led-lassoc (start infix-token)
  (let* ((power (r-parser-power infix-token))
         (end (r-parse-expression power)))
    (r-node (cons "binary-op" nil)
            (cons (r-parser-token-start start) (point))
            (list start infix-token end))))

(defun r-parser-led-funcall (left infix-token)
  (when (r-token= left (append '("identifier" "string" "node")
                               r-prefix-keywords-list))
    (let* ((power (r-parser-power infix-token))
           (right (r-parse-arglist power infix-token))
           (type (if (r-token= left r-prefix-keywords-list)
                     "prefixed-expr"
                   "funcall")))
      (when (string= type "prefixed-expr")
        (setq right (list right (r-parse-expression 0))))
      (r-node (cons type nil)
              (cons (r-parser-token-start left) (point))
              (list left right)))))

(defun r-parser-rid-expr-prefix (right suffix-token)
  (when (r-refined-token= suffix-token "prefixed-expr-delimiter")
    (r-parser-rnud-paren suffix-token right)))

(defun r-parser-rnud-paren (suffix-token &optional prefixed-expr)
  (let* ((infix-token (save-excursion
                        (r-parser-advance-pair nil suffix-token)))
         (power (r-parser-power infix-token))
         (args (r-parse-arglist power suffix-token))
         (left (if prefixed-expr
                   (r-parser-advance)
                 (r-parse-expression power)))
         (type (cond (prefixed-expr "prefixed-expr")
                     (left "funcall")
                     (t "enclosed-expr"))))
    (when prefixed-expr
      (setcdr (car prefixed-expr) (list infix-token suffix-token)))
    (r-node (cons type nil)
            (cons (r-parser-token-start suffix-token) (point))
            (if prefixed-expr
                (list prefixed-expr args left)
              (list args left)))))


;;;*;;; Parsing

(defun r-parser-advance (&optional type value)
  (if (bound-and-true-p r-parser--backward)
      (r-climb-token type value)
    (r-jump-token type value)))

(defun r-parser-advance-pair (&optional type token)
  (if (bound-and-true-p r-parser--backward)
      (r-climb-paired-delims type token)
    (r-jump-paired-delims type token)))

(defun r-parser-next-token ()
  (if (bound-and-true-p r-parser--backward)
      (r-token-before)
    (r-token-after)))

(defun r-parser-token-start (token)
  (if (bound-and-true-p r-parser--backward)
      (r-token-end token)
    (r-token-start token)))

(defun r-parser-power (token)
  (or (if (bound-and-true-p r-parser--backward)
          (gethash (r-token-type token) r-token-r-right-power-table)
        (gethash (r-token-type token) r-token-r-power-table))
      0))

(defun r-node (type pos contents)
  (let ((pos (if (bound-and-true-p r-parser--backward)
                 (cons (cdr pos) (car pos))
               pos))
        (contents (if (bound-and-true-p r-parser--backward)
                      (nreverse contents)
                    contents)))
    (list type pos contents)))

(defalias 'r-node-start #'r-token-start)
(defalias 'r-node-end #'r-token-end)

(defun r-parse-start-token (token)
  (let* ((table (if (bound-and-true-p r-parser--backward)
                    r-token-r-rnud-table
                  r-token-r-nud-table))
         (nud (gethash (r-token-type token) table)))
    (when (fboundp nud)
      (funcall nud token))))

(defun r-parse-infix-token (infix-token left)
  (let ((infix-power (r-parser-power infix-token))
        (led (or (when (bound-and-true-p r-parser--backward)
                   (gethash (r-token-type infix-token) r-token-r-rid-table))
                 (gethash (r-token-type infix-token) r-token-r-led-table))))
    (funcall led left infix-token)))

(defun r-parse-expression (&optional power)
  (let ((current (r-parse-start-token (r-parser-advance)))
        (power (or power 0))
        (next (r-parser-next-token))
        (last-sucessful-pos (point))
        last-success)
    (setq last-success current)
    (while (and current (< power (r-parser-power next)))
      (r-parser-advance)
      (when (setq current (r-parse-infix-token next current))
        (setq last-sucessful-pos (point))
        (setq last-success current))
      (setq next (r-parser-next-token)))
    (goto-char last-sucessful-pos)
    last-success))

(defun r-parse-arglist (power start-token)
  (let ((start-pos (point))
        (arg-start-pos (point))
        (arglist (list start-token))
        (closing-delim (r-token-balancing-delim start-token))
        expr)
    (while (and (setq expr (r-parse-expression))
                (push (r-node (cons "arg" nil)
                              (cons arg-start-pos (point))
                              (list expr))
                      arglist)
                (r-parser-advance ","))
      (setq arg-start-pos (point)))
    (push (r-parser-advance closing-delim) arglist)
    (r-node (cons "arglist" nil)
            (cons start-pos (1- (point)))
            (nreverse arglist))))

(defun forward-r-expr ()
  (interactive)
  (r-save-excursion-when-nil
    (r-escape-token)
    (r-parse-expression)))

(defun forward-r-sexp ()
  (interactive)
  (r-save-excursion-when-nil
    (r-escape-token)
    (let* ((orig-token (r-token-after))
           (tree (r-parse-expression))
           (sexp-node (r-parser-tree-assoc orig-token tree)))
      (when sexp-node
        (goto-char (r-token-end sexp-node))
        sexp-node))))

(defun backward-r-expr ()
  (interactive)
  (let ((r-parser--backward t))
    (r-parse-expression)))

(defun backward-r-sexp ()
  (interactive)
  (error "todo"))

(defun r-parser-tree-assoc (key tree)
  (let ((next tree)
        stack last-node result)
    (while (and next (null result))
      (cond ((eq next 'node-end)
             (pop last-node))
            ((nth 2 next)
             (push 'node-end stack)
             (dolist (node (nth 2 next))
               (push node stack))
             (push next last-node))
            ((equal next key)
             (setq result (car last-node))))
      (setq next (pop stack)))
    result))


;;*;; Point predicates

(defun r-within-call-p (&optional call)
  "Is point in a function or indexing call?"
  (let ((containing-sexp (or (bound-and-true-p containing-sexp)
                             (r-containing-sexp-position))))
    (save-excursion
      (and (prog1 (r-goto-char containing-sexp)
             (r-climb-chained-delims))
           (save-excursion
             (forward-char)
             (r-up-list))
           (or (r-behind-call-opening "(")
               (looking-at "\\["))
           (r-within-call-name-p call)))))

(defun r-within-continuation-p ()
  (unless (or (looking-at ",")
              (r-behind-call-opening "[[(]"))
    (or (save-excursion
          (r-jump-object)
          (and (not (r-ahead-param-assign-p))
               (r-behind-operator-p)))
        (save-excursion
          (r-climb-object)
          (r-climb-operator)
          (and (r-behind-operator-p)
               (not (r-ahead-param-assign-p)))))))

(defun r-within-call-name-p (&optional call)
  (save-excursion
    (r-climb-call-name call)))

(defun r-within-prefixed-block-p (&optional call)
  "Return t if point in a prefixed block.
Prefixed blocks refer to the blocks following function
declarations, control flow statements, etc.

If CALL is non nil, check if the prefix corresponds to a CALL. If
nil, return the prefix."
  (save-excursion
    (r-escape-prefixed-block call)))

(defun r-within-comment-p (&optional state)
  (let ((state (or state (syntax-ppss))))
    (eq (syntax-ppss-context state) 'comment)))

(defun r-within-string-p (&optional state)
  (let ((state (or state (syntax-ppss))))
    (eq (syntax-ppss-context state) 'string)))


;;*;; Syntactic Travellers and Predicates

;;;*;;; Blanks, Characters, Comments and Delimiters

(defun r-skip-blanks-backward (&optional newlines)
  "Skip blanks and newlines backward, taking end-of-line comments
into account."
  (r-any ((r-skip-blanks-backward-1))
         ((when newlines
            (r-while (and (/= (point) (point-min))
                          (= (point) (line-beginning-position)))
              (forward-line -1)
              (goto-char (r-line-end-position))
              (r-skip-blanks-backward-1))))))

(defun r-skip-blanks-backward-1 ()
  (and (/= (point) (point-min))
       (/= 0 (skip-syntax-backward " "))))

(defun r-skip-blanks-forward (&optional newlines)
  "Skip blanks and newlines forward, taking end-of-line comments
into account."
  (r-any ((/= 0 (skip-syntax-forward " ")))
         ((r-while (and newlines
                        (= (point) (r-line-end-position))
                        (when (r-save-excursion-when-nil
                                ;; Handles corner cases such as point being on last line
                                (let ((orig-point (point)))
                                  (forward-line)
                                  (r-back-to-indentation)
                                  (> (point) orig-point)))
                          (skip-chars-forward " \t")
                          t))))))

(defun r-jump-char (char)
  (r-save-excursion-when-nil
    (r-skip-blanks-forward t)
    (when (looking-at char)
      (goto-char (match-end 0)))))

(defun r-escape-comment ()
  (when (r-within-comment-p)
    (prog1 (comment-beginning)
      (skip-chars-backward "#+[ \t]*"))))

(defun r-ahead-closing-p ()
  (memq (char-before) '(?\] ?\} ?\))))

(defun r-ahead-boundary-p ()
  (looking-back "[][ \t\n(){},]" (1- (point))))

(defun r-escape-string ()
  (and (nth 3 (syntax-ppss))
       (r-goto-char (nth 8 (syntax-ppss)))))

(defun r-climb-paired-delims (&optional type token)
  (r-save-excursion-when-nil
    (let ((token (or token (r-token-before))))
      (goto-char (r-token-end token))
      (when (if type
                (r-token= token type)
              (r-token-delimiter-p token))
        (and (r-backward-sexp)
             (r-token-after))))))

(defun r-jump-paired-delims (&optional type token)
  (r-save-excursion-when-nil
    (let ((token (or token (r-token-after))))
      (goto-char (r-token-start token))
      (when (if type
                (r-token= token type)
              (r-token-delimiter-p token))
        (and (r-forward-sexp)
             (r-token-before))))))


;;;*;;; Blocks

(defun r-block-opening-p ()
  (save-excursion
    (cond
     ((looking-at "{"))
     ;; Opening parenthesis not attached to a function opens up a
     ;; block too. Only pick up those that are last on their line
     ((r-behind-block-paren-p)))))

(defun r-block-closing-p ()
  (save-excursion
    (cond
     ((looking-at "}"))
     ((looking-at ")")
      (forward-char)
      (backward-sexp)
      (not (looking-back
            (concat r-name-pattern "[[:blank:]]*")
            (line-beginning-position)))))))

(defun r-block-p ()
  (or (save-excursion
        (when containing-sexp
          (goto-char containing-sexp)
          (r-block-opening-p)))
      (r-unbraced-block-p)))

;; Parenthesised expressions
(defun r-behind-block-paren-p ()
  (and (looking-at "(")
       (not (r-ahead-attached-name-p))))

(defun r-climb-block (&optional ignore-ifelse)
  (r-save-excursion-when-nil
    (cond
     ((and (not ignore-ifelse)
           (r-climb-if-else 'to-start)))
     ((and (eq (char-before) ?\})
           (prog2
               (forward-char -1)
               (r-up-list -1)
             (r-climb-block-prefix)))))))

(defvar r-prefixed-block-patterns
  (mapcar (lambda (fun) (concat fun "[ \t\n]*("))
          '("function" "if" "for" "while")))

(defun r-behind-prefixed-block-p (&optional call)
  (if call
      (looking-at (concat call "[ \t]*("))
    (cl-some 'looking-at r-prefixed-block-patterns)))

(defun r-unbraced-block-p (&optional ignore-ifelse)
  "This indicates whether point is in front of an unbraced
prefixed block following a control flow statement. Returns
position of the control flow function (if, for, while, etc)."
  (save-excursion
    (and (r-backward-sexp)
         (or (and (looking-at "else\\b")
                  (not ignore-ifelse))
             (and (looking-at "(")
                  (r-backward-sexp)
                  (cl-some 'looking-at r-prefixed-block-patterns)
                  (if ignore-ifelse
                      (not (looking-at "if\\b"))
                    t)))
         (point))))

(defun r-climb-block-prefix (&optional call ignore-ifelse)
  "Climb the prefix of a prefixed block. Prefixed blocks refer to
the blocks following function declarations, control flow
statements, etc.

Should be called either in front of a naked block or in front
of the curly brackets of a braced block.

If CALL not nil, check if the prefix corresponds to CALL. If nil,
return the prefix."
  (r-save-excursion-when-nil
    (or (and (not ignore-ifelse)
             (prog1 (and (r-climb-if-else-call)
                         (or (null call)
                             (looking-at call)))
               (when (r-token-after= "else")
                 (r-climb-token "}"))))
        (let ((pos (r-unbraced-block-p ignore-ifelse)))
          (and (r-goto-char pos)
               (if call
                   (looking-at call)
                 (cond ((looking-at "function")
                        "function")
                       ((looking-at "for")
                        "for")
                       ((looking-at "if")
                        "if")
                       ((looking-at "else")
                        "else"))))))))

(defun r-escape-prefixed-block (&optional call)
  "Climb outside of a prefixed block."
  (let ((containing-sexp (or (bound-and-true-p containing-sexp)
                             (r-containing-sexp-position))))
    (or (r-save-excursion-when-nil
          (and (r-goto-char containing-sexp)
               (looking-at "{")
               (r-climb-block-prefix call)))
        (r-escape-unbraced-block call))))

(defun r-escape-unbraced-block (&optional call)
  (r-save-excursion-when-nil
    (while (and (not (r-unbraced-block-p))
                (or (r-escape-continuations)
                    (r-escape-call))))
    (r-climb-block-prefix call)))

(defun r-jump-block ()
  (cond
   ;; if-else blocks
   ((r-jump-if-else))
   ;; Prefixed blocks such as `function() {}'
   ((r-behind-prefixed-block-p)
    (r-jump-prefixed-block))
   ;; Naked blocks
   ((and (or (looking-at "{")
             (r-behind-block-paren-p))
         (r-forward-sexp)))))

(defun r-jump-prefixed-block (&optional call)
  (r-save-excursion-when-nil
    (when (r-behind-prefixed-block-p call)
      (r-forward-sexp 2)
      (r-skip-blanks-forward t)
      (if (looking-at "{")
          (r-forward-sexp)
        (prog1 (r-jump-expression)
          (r-jump-continuations))))))


;;;*;;; Calls

(defun r-call-closing-p ()
  (save-excursion
    (when (cond ((looking-at ")")
                 (r-up-list -1))
                ((looking-at "]")
                 (when (r-up-list -1)
                   (prog1 t (r-climb-chained-delims)))))
      (r-ahead-attached-name-p))))

(defun r-behind-call-opening (pattern)
  (and (looking-at pattern)
       (r-ahead-attached-name-p)))

;; Should be called just before the opening brace
(defun r-ahead-attached-name-p ()
  (save-excursion
    (r-climb-object)))

(defun r-ahead-param-assign-p ()
  "Are we looking at a function argument? To be called just
before the `=' sign."
  (r-refined-token= (r-token-before) "param-assign"))

(defun r-behind-arg-p ()
  (save-excursion
    (r-jump-arg)))

(defun r-behind-parameter-p ()
  (save-excursion
    (r-jump-parameter)))

(defun r-jump-parameter ()
  (r-save-excursion-when-nil
    (and (r-jump-name)
         (when (looking-at "[ \t]*=\\([^=]\\)")
           (goto-char (match-beginning 1))
           (r-skip-blanks-forward)
           t))))

(defun r-jump-arg ()
  (r-save-excursion-when-nil
    (r-skip-blanks-forward t)
    (r-any ((r-jump-parameter))
           ((r-jump-expression))
           ((r-jump-continuations)))))

(defun r-arg-bounds ()
  "Should be called in front of the argument."
  (save-excursion
    (let ((beg (point)))
      (and (r-jump-arg)
           (list beg (point))))))

(defun r-climb-call (&optional call)
  "Climb functions (e.g. ggplot) and parenthesised expressions."
  (or (r-while (r-save-excursion-when-nil
                 (r-climb-name)
                 (and (r-climb-chained-delims ?\])
                      ;; (r-climb-expression)
                      (if (eq (char-before) ?\))
                          (r-climb-call)
                        (r-climb-name))
                      )))
      (r-save-excursion-when-nil
        (when (and (memq (char-before) '(?\] ?\) ?\}))
                   (r-backward-sexp))
          (if call
              (and (r-climb-name)
                   (looking-at call)))
          (prog1 t
            (r-climb-name))))))

(defun r-climb-call-name (&optional call)
  (r-save-excursion-when-nil
    (r-jump-name)
    (r-skip-blanks-forward)
    (and (r-behind-call-opening "[[(]")
         (r-climb-name)
         (or (null call)
             (looking-at call)))))

(defun r-step-to-first-arg ()
  (let ((containing-sexp (r-containing-sexp-position)))
    (cond ((r-within-call-p)
           (goto-char containing-sexp)
           (forward-char)
           t)
          ((r-within-call-name-p)
           (r-jump-name)
           (r-skip-blanks-forward)
           (forward-char)
           t))))

(defun r-jump-to-next-arg ()
  (and (r-jump-arg)
       (prog1 (r-jump-char ",")
         (r-skip-blanks-forward t))))

(defun r-jump-call ()
  (r-save-excursion-when-nil
    (or (and (r-jump-object)
             (cond ((eq (char-before) ?\)))
                   ((looking-at "\\[")
                    (r-jump-chained-brackets))
                   ((looking-at "(")
                    (r-forward-sexp))))
        (and (looking-at "[ \t]*(")
             (r-forward-sexp)))))

(defun r-behind-call-p ()
  (save-excursion
    (r-jump-object)
    (r-skip-blanks-forward)
    (looking-at "[[(]")))

(defun r-climb-chained-delims (&optional delim)
  "Should be called with point between delims, e.g. `]|['."
  (setq delim (if delim
                  (list delim)
                '(?\] ?\))))
  (r-while (r-save-excursion-when-nil
             (when (memq (char-before) delim)
               (r-backward-sexp)))))

(defun r-jump-chained-brackets ()
  (r-while (r-save-excursion-when-nil
             (when (eq (char-after) ?\[)
               (r-forward-sexp)))))

(defun r-escape-call (&optional call)
  (let ((containing-sexp (r-containing-sexp-position)))
    (if (r-within-call-p)
        (r-save-excursion-when-nil
          (goto-char containing-sexp)
          (r-climb-chained-delims)
          (and (r-climb-name)
               (or (null call)
                   (looking-at call))))
      ;; At top level or inside a block, check if point is on the
      ;; function name.
      (r-save-excursion-when-nil
        (let ((orig-pos (point)))
          (and (r-jump-name)
               (looking-at "[[(]")
               (r-climb-name)
               (or (null call)
                   (looking-at call))
               (/= (point) orig-pos)))))))

(defun r-escape-calls ()
  (r-while (r-escape-call)))

(defun r-jump-inside-call ()
  (r-save-excursion-when-nil
    (when (r-jump-name)
      (r-skip-blanks-forward)
      (when (looking-at "(")
        (forward-char)
        t))))

(defun r-args-bounds (&optional marker)
  (let ((containing-sexp (r-containing-sexp-position)))
    (when (r-within-call-p)
      (save-excursion
        (let ((beg (1+ containing-sexp))
              (call-beg (r-at-containing-sexp
                          (r-climb-name)
                          (point))))
          ;; (r-up-list) can't find its way when point is on a
          ;; backquoted name, so start from `beg'.
          (and (goto-char beg)
               (r-up-list)
               (prog1 t
                 (forward-char -1))
               (let ((end (if marker
                              (point-marker)
                            (point))))
                 (list beg end call-beg))))))))

(defun r-args-alist ()
  "Return all arguments as an alist with cars set to argument
names and cdrs set to the expressions given as argument. Both
cars and cdrs are returned as strings."
  (save-excursion
    (when (r-step-to-first-arg)
      (let (args current-arg)
        (while (and (setq current-arg (r-cons-arg))
                    (setq args (nconc args (list current-arg)))
                    (r-jump-to-next-arg)))
        args))))

(defun r-cons-arg ()
  "Return a cons cell of the current argument with car set to the
parameter name (nil if not specified) and cdr set to the argument
expression."
  (save-excursion
    (r-skip-blanks-forward t)
    (let ((param (when (r-behind-parameter-p)
                   (buffer-substring-no-properties
                    (point)
                    (prog2
                        (r-jump-name)
                        (point)
                      (r-jump-char "=")
                      (r-skip-blanks-forward)))))
          (arg (buffer-substring-no-properties
                (point)
                (progn
                  (r-jump-arg)
                  (point)))))
      (cons param arg))))


;;;*;;; Statements

(defun r-behind-operator-p (&optional strict)
  (r-token-operator-p (r-token-after) strict))

(defun r-ahead-operator-p (&optional strict)
  (r-token-operator-p (r-token-before) strict))

(defun r-climb-lhs (&optional no-fun-arg climb-line)
  (r-save-excursion-when-nil
    (let ((start-line (line-number-at-pos)))
      (r-climb-operator)
      (when (and (or climb-line (equal (line-number-at-pos) start-line))
                 (r-behind-definition-op-p no-fun-arg))
        (prog1 t
          (r-climb-expression))))))

(defun r-jump-lhs ()
  (r-save-excursion-when-nil
    (and (r-jump-name)
         (r-behind-definition-op-p)
         (r-jump-operator))))

(defun r-climb-operator ()
  (when (r-token-operator-p (r-token-before))
    (prog1 (r-climb-token)
      (r-skip-blanks-backward))))

;; Currently doesn't check that the operator is not binary
(defun r-climb-unary-operator ()
  (r-save-excursion-when-nil
    (let ((token (r-climb-token)))
      (member (r-token-type token) '("+" "-" "!" "?" "~")))))

;; Currently returns t if we climbed lines, nil otherwise.
(defun r-climb-continuations (&optional cascade ignore-ifelse)
  (let ((start-line (line-number-at-pos))
        (moved 0)
        (last-pos (point))
        last-line prev-point def-op expr)
    (setq last-line start-line)
    (when (r-while (and (<= moved 1)
                        (or (r-save-excursion-when-nil
                              (and (r-climb-operator)
                                   (r-climb-continuations--update-state 'op)
                                   (r-climb-expression ignore-ifelse)))
                            (r-climb-unary-operator))
                        (/= last-pos (point)))
            (r-climb-continuations--update-state)
            (setq last-pos (point)))
      (when (and prev-point
                 (or (= moved 3)
                     (not expr)))
        (goto-char prev-point))
      (if def-op 'def-op (< (line-number-at-pos) start-line)))))

(defun r-climb-continuations--update-state (&optional op)
  ;; Climbing multi-line expressions should not count as moving up
  (when op
    (setq expr (r-ahead-closing-p)))
  (let ((cur-line (line-number-at-pos)))
    (when (and last-line
               (< cur-line last-line)
               (or cascade (not expr)))
      (setq moved (1+ moved))
      (setq last-line cur-line)))
  ;; Don't update counter after climbing operator or climbing too high
  (when (and (not op)
             (<= moved 1))
    (setq prev-point (point)))
  (when (and (r-behind-definition-op-p)
             (<= moved 1))
    (setq def-op t))
  t)

(defun r-jump-operator ()
  (when (r-behind-operator-p)
    (r-jump-token)
    (r-skip-blanks-forward t)
    t))

(defun r-jump-continuation ()
  (and (r-jump-operator)
       (r-jump-expression)))

(defun r-jump-continuations ()
  (let (last-pos)
    (when (r-while (and (or (null last-pos)
                            (/= (point) last-pos))
                        (setq last-pos (point))
                        (r-jump-continuation)))
      ;; In calls, operators can start on newlines
      (let ((start-line (line-number-at-pos)))
        (when (r-save-excursion-when-nil
                (and (r-within-call-p)
                     (r-skip-blanks-forward t)
                     (/= (line-number-at-pos) start-line)
                     (r-behind-operator-p)))
          (r-jump-continuations)))
      t)))

(defun r-ahead-continuation-p (&optional or-parameter)
  (or (r-token-operator-p (r-token-before) (not or-parameter))
      (save-excursion
        (r-climb-block-prefix))
      (r-token-after= "else")
      (save-excursion
        (r-climb-if-else-call))))

(defun r-token-definition-op-p (token strict)
  (and (r-token= token '("<-" "<<-" ":=" "~" "="))
       (if strict
           (not (r-refined-token= token "param-assign"))
         t)))

(defun r-behind-definition-op-p (&optional strict)
  (r-token-definition-op-p (r-token-after) strict))

(defun r-ahead-definition-op-p (&optional strict)
  (r-token-definition-op-p (r-token-before) strict))

(defun r-behind-assignment-op-p ()
  (let ((token (r-token-after)))
    (and (r-token= token '("<-" "="))
         (not (r-refined-token= token "param-assign")))))

(defun r-escape-continuations ()
  (r-any ((unless (r-ahead-boundary-p)
            (r-climb-expression)))
         ((r-while (r-climb-continuations)))))

(defun r-continuations-bounds (&optional marker)
  (save-excursion
    (let ((orig-point (point))
          (beg (progn
                 (r-escape-continuations)
                 (point))))
      (when beg
        (r-jump-expression)
        (r-jump-continuations)
        (let ((end (if marker
                       (point-marker)
                     (point))))
          (list beg end))))))

(defun r-climb-to-top-level ()
  (while (r-goto-char (r-containing-sexp-position)))
  (r-escape-continuations))


;;;*;;; Statements: Control Flow

(defun r-climb-if-else-call (&optional multi-line)
  "Climb if, else, and if else calls."
  (r-save-excursion-when-nil
    (cond ((r-climb-paired-delims ")")
           (when (r-climb-token "if")
             ;; Check for `else if'
             (prog1 t
               (r-save-excursion-when-nil
                 (let ((orig-line (line-number-at-pos)))
                   (and (r-climb-token "else")
                        (or multi-line
                            (eq orig-line (line-number-at-pos)))))))))
          ((r-climb-token "else")))))


(defun r-climb-if-else-body (&optional from-else)
  (cond
   ;; Climb braced body
   ((r-save-excursion-when-nil
      (and (when (progn (r-skip-blanks-backward t)
                        (eq (char-before) ?\}))
             (prog1 t (forward-char -1)))
           (r-up-list -1))))
   ;; Climb unbraced body
   ((when from-else
      (r-save-excursion-when-nil
        (r-skip-blanks-backward t)
        (prog1 (r-climb-expression 'ignore-ifelse)
          (or (r-climb-continuations nil 'ignore-ifelse)
              (r-climb-block-prefix nil 'ignore-ifelse))))))))

(defun r-climb-if-else (&optional to-start)
  "Climb horizontal as well as vertical if-else chains, with or
without curly braces."
  ;; Don't climb if we're atop the current chain of if-else
  (unless (r-token-after= "if")
    (r-save-excursion-when-nil
      (let ((from-else (r-token-after= "else")))
        (when (and (r-climb-if-else-body from-else)
                   (r-climb-if-else-call to-start))
          ;; If we start from a final else and climb to another else, we
          ;; are in the wrong chain of if-else. In that case,
          ;; climb-recurse to the top of the current chain and climb
          ;; again to step in the outer chain.
          (when (save-excursion (and from-else
                                     (r-jump-token "else")
                                     (not (r-jump-token "if"))))
            (r-climb-if-else 'to-start)
            (r-climb-continuations)
            (r-climb-block-prefix nil 'ignore-ifelse)
            (r-climb-if-else-call nil))
          (r-maybe-climb-broken-else)
          (when to-start
            (r-climb-if-else to-start))
          t)))))

;; Broken else: if \n else
(defun r-maybe-climb-broken-else (&optional same-line)
  (r-save-excursion-when-nil
    ;; Don't record current line if not needed (expensive operation)
    (let ((cur-line (when same-line (line-number-at-pos))))
      (and (r-climb-token "else")
           (if same-line
               (= cur-line (line-number-at-pos))
             t)))))

(defun r-skip-curly-backward ()
  (re-search-backward "}[ \t]*" (line-beginning-position) t))

(defun r-jump-if-else ()
  (let (from)
    (r-while (r-save-excursion-when-nil
               (r-skip-blanks-forward t)
               (cond
                ((and (not (eq from 'if))
                      (r-jump-if)
                      (setq from 'if)))
                ((looking-at "else")
                 (r-forward-sexp)
                 (or (r-jump-if)
                     (progn
                       (r-skip-blanks-forward t)
                       (r-jump-expression)))
                 (setq from 'else))
                (t
                 nil))))))

(defun r-jump-if ()
  (r-save-excursion-when-nil
    (r-skip-blanks-forward t)
    (and (looking-at "if[ \t\n]*(")
         (r-forward-sexp 2)
         (progn
           (r-skip-blanks-forward t)
           (r-jump-expression)))))


;;;*;;; Function Declarations

(defun r-behind-defun-p ()
  (or (looking-at "function[ \t]*(")
      (r-behind-enclosed-defun-p)))

(defun r-behind-enclosed-defun-p ()
  (save-excursion
    (and (r-behind-call-p)
         (r-jump-inside-call)
         (cl-some (lambda (arg)
                    (string-match "^function\\b"
                                  (cdr arg)))
                  (r-args-alist)))))


;;;*;;; Names / Objects / Expressions

;; Should  climb any names, including backquoted ones or those
;; containing `@' or `$'. Difficult to achieve with regexps, but
;; skipping chars is faster anyway.
(defun r-climb-object ()
  (r-save-excursion-when-nil
    (let (climbed)
      (r-skip-blanks-backward)
      ;; Backquoted names can contain any character
      (if (and (memq (char-before) '(?` ?\" ?\'))
               (r-backward-sexp))
          (setq climbed t)
        (while (cl-some (apply-partially '/= 0)
                        `(,(skip-syntax-backward "w_")
                          ,(skip-chars-backward "\"'")))
          (setq climbed t)))
      ;; Recurse if we find an indexing char
      (when (memq (char-before) '(?$ ?@))
        (forward-char -1)
        (r-climb-object))
      climbed)))

;; Todo: split name and object climbing
(defun r-climb-name ()
  (r-climb-object))

;; This jumps both object names and atomic objects like strings or
;; numbers.
(defun r-jump-object ()
  (cond
   ;; Jump over object names
   ((r-jump-name))
   ;; Jump over strings))
   ((r-save-excursion-when-nil
      (skip-chars-forward " \t")
      (memq (char-after) '(?\" ?\')))
    (r-forward-sexp))))

(defun r-jump-name ()
  (r-save-excursion-when-nil
    (let (climbed quote-char)
      (skip-chars-forward " \t")
      ;; Jump over backquoted names
      (cond ((and (eq (char-after) ?`)
                  (looking-back r-symbol-regexp
                                (1- (point))))
             (forward-char)
             (setq climbed t))
            ((eq (char-after) ?`)
             (forward-char)
             (when (r-while (not (memq (char-after) '(?` ?\C-J)))
                     (forward-char))
               (setq climbed t)
               (forward-char)))
            ;; Jump over regular names
            ((when (/= 0 (skip-syntax-forward "w_"))
               ;; Maybe point was inside backticks
               (when (eq (char-after) ?`)
                 (forward-char))
               (setq climbed t))))
      climbed)))

(defun r-climb-expression (&optional ignore-ifelse)
  (r-save-excursion-when-nil
    (or (r-climb-block ignore-ifelse)
        (r-climb-call)
        (r-climb-object))))

(defun r-jump-expression ()
  (or (r-jump-block)
      (r-jump-call)
      (r-jump-object)))

(provide 'r-syntax)
;;; r-syntax.el ends here
