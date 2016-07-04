;;; r-indent.el --- Indentation engine for r-mode
;; 
;; Copyright (C) 2016 Lionel Henry, Vitalie Spinu
;; 
;; Author: Lionel Henry <lionel.hry@gmail.com>,
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

(require 'r-syntax)

(defgroup r-indent nil
  "r-mode indentation."
  :group 'r
  :prefix "r-indent-"
  :prefix "r-offset-")

(defcustom r-indent-offset 2
  "Main indentation offset that is commonly inherited by other offsets.
See `r-style-alist' for all available offsets."
  :group 'r-indent
  :type 'integer)

(defcustom r-offset-arguments 'open-delim
  "Indent for arguments of function calls or indexing brackets.
This variables has an effect only when the ( or [ are not
directly followed by a new line. See
`r-offset-arguments-newline' for indentation after closing
newline.

When set to `open-delim', arguments are indented relative to the
opening parenthesis of the closest function call:

  object <- call(argument, other_call(argument,
                                      other_argument))


When set to `prev-call', arguments are indented relative to the
closest function call:

  object <- call(argument, other_call(argument,
                               other_argument))


When set to `prev-line', arguments are indented relative to the
preceding line:

  object <- call(argument, other_call(argument,
      other_argument))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`r-indent-offset' is used as a default. See `r-style-alist'
for other offsets controlling indentation."
  :group 'r-indent
  :type '(choice (const open-delim)
                 (const prev-call)
                 (const prev-line)))

(defcustom r-offset-arguments-newline 'prev-call
  "Indent of arguments when ( or [ is followed by a new line.

When set to `open-delim', arguments on a new line are indented
relative to the opening parenthesis of the closest function call:

  object <- call(argument, other_call(
                                      argument,
                                      other_argument
                                      ))


When set to `prev-call', arguments on a new line are indented relative to
the closest function call:

  object <- call(argument, other_call(
                               argument,
                               other_argument
                           ))

You can control the details of indentation at `prev-call' with
`r-indent-from-lhs' and `r-indent-from-chain-start'.


When set to `prev-line', arguments on a new line are indented
relative to the preceding line:

  object <- call(argument, other_call(
      argument,
      other_argument
  ))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`r-indent-offset' is used as a default. See `r-style-alist'
for other offsets controlling indentation."
  :group 'r-indent
  :type '(choice (const open-delim)
                 (const prev-call)
                 (const prev-line)))

(defcustom r-offset-block 'prev-line
  "Indentation for blocks. A block is usually declared with
braces but a statement wrapped in anonymous parentheses is also
considered a block. This offset can be either `prev-call',
`prev-line' or `open-delim'.

When set to `open-delim', blocks are indented relative to the
opening parenthesis of the closest function call:

  call(argument, other_call(parameter = {
                                stuff
                            }, {
                                stuff
                            }))

  call(argument, lapply(data, function(x) {
                            body
                        }))


When set to `prev-call', blocks are indented relative to the
closest function call:

  call(argument, other_call(parameter = {
                     stuff
                 }, {
                     stuff
                 }))

  call(argument, lapply(data, function(x) {
                     body
                 }))

You can control the details of indentation at `prev-call' with
`r-indent-from-lhs' and `r-indent-from-chain-start'.


When set to `prev-line', blocks are indented relative to the
preceding line:

  call(argument, other_call(parameter = {
      stuff
  }, {
      stuff
  }))

  call(argument, lapply(data, function(x) {
      body
  }))

This setting can also be set to a list containing the the offset
type and the offset size, such as `'(prev-call 2)'. Otherwise,
`r-indent-offset' is used as a default. See `r-style-alist'
for other offsets controlling indentation."
  :group 'r-indent
  :type '(choice (const open-delim)
                 (const prev-call)
                 (const prev-line)))

(defcustom r-offset-continued 'straight
  "This setting controls indentation of continued statements, that is,
consecutive statements separated by operators.

When set to 'straight, continued statements are indented as follows:

  object %>%
      some_function() %>%
      other_function()

When set to 'cascade:

  object %>%
      some_function() %>%
          other_function()

The 'straight and 'cascade settings are actually equivalent to
'(straight . t) and '(cascade . t), where `t' represents the
base indent size. More generally, you can supply '(straight . N)
to control the size of indentation.

See `r-style-alist' for for an overview of ESS indentation."
  :group 'r-indent
  :type '(choice (const straight)
                 (const cascade)))

(defcustom r-align-nested-calls '("ifelse")
  "List of strings declaring function calls for which
`r-offset-arguments-newline' should be ignored. These calls
will be vertically aligned instead. The default is `ifelse',
resulting in the following indentation for nested ifelse calls:

    object <- ifelse(condition1, out1,
              ifelse(condition2, out2, out3))

See `r-style-alist' for for an overview of ESS indentation."
  :group 'r-indent
  :type '(repeat string))

(defcustom r-align-arguments-in-calls '("function[ \t]*(")
  "List of regexes specifying the calls where
`r-offset-arguments' should have no effect on function
declarations. The arguments of those calls will be aligned from
the opening parenthesis.

By default, function declarations are overridden. If for example
`r-offset-arguments' is set to `prev-line', then function calls
are normally indented as in:

  some_function(argument1,
      argument2, argument3
  )

However, the parameters of function declarations will be
vertically aligned:

  fun <- function(argument1,
                  argument2
                  argument3) {
      body
  }

See `r-style-alist' for further details."
  :group 'r-indent
  :type '(reapeat string))

(defcustom r-align-continuations-in-calls t
  "If non-nil, indent code inside calls from the opening delimiter.
This produces the following indentation:

  10 + (1 + 2 +
        3 + 4)
  object[variable1 +
         variable2]

  if (test1 || test2 ||
      test3 || test4) {
      any(test5 &
          test6)
  }

instead of

  10 + (1 + 2 +
            3 + 4)
  object[variable1 +
             variable2]

  if (test1 || test2 ||
        test3 || test4) {
      any(test5 &
            test6)
  }

Definition operators (`<-', `=', `:=' and `~') still trigger an
indentation in all cases. Also, operators at top level and in
curly brackets are not affected by this setting and always induce
an offset:

  {
      var1 +
          var2
  }

See `r-style-alist' for for an overview of ESS indentation."
  :group 'r-indent
  :type 'boolean)

(defcustom r-align-blocks '(control-flow)
  "List of block types for which `r-offset-blocks' should be
ignored. The overridden blocks are vertically aligned. The list
can contain either or both of the symbols `control-flow' and
`fun-decl'.

With `control-flow', if, else for and while blocks will always be
aligned vertically. With `fun-decl', the body of a function
declaration will always be aligned with the call to
`function'."
  :group 'r-indent
  :type '(repeat symbol))

(defcustom r-indent-from-lhs '(arguments fun-decl-opening)
  "List of syntactic elements that should be indented from the
left-hand side of an assignment. The list accepts the symbol
`arguments' and `fun-decl-opening'.

For arguments, this setting only has an effect for offsets set to
`prev-call'. When set, this indentation is produced:

  some_function(parameter = other_function(
                                argument
                            ))

  object <- some_function(
                argument1,
                argument2
            )

instead of:

  some_function(parameter = other_function(
                    argument
                ))

  object <- some_function(
      argument1,
      argument2
  )


`fun-decl-opening' refers to the opening curly following a function
declaration. Setting it produces:

  object <-
      function(argument)
  {
      body
  }

instead of:

  object <-
      function(argument)
      {
          body
      }

This is useful when (a) you have a long function name and want to
break a line after `<-' so that you have room to lay out the
arguments within `fill-column' characters; (b) you still want to
align the function body from the LHS to save horizontal space.

See `r-style-alist' for for an overview of ESS indentation."
  :group 'r-indent
  :type '(repeat symbol))

(defcustom r-indent-from-chain-start t
  "When non-nil, chained calls will be treated as if they were
one call and indentation will start from the first one. This
setting only has an effect for offsets set to `prev-call' or
block offsets set to `opening-delim'.

If `nil':

  some_function(other_function(
                    argument
                ))

If `t':

  some_function(other_function(
      argument
  ))

See `r-style-alist' for for an overview of ESS indentation."
  :group 'r-indent
  :type 'boolean)

(defcustom r-indent-with-fancy-comments t
  "Non-nil means distiguish between #, ##, and ### for indentation.
See `r-style-alist' for for an overview of ESS indentation."
  :group 'r-indent
  :type 'boolean)


;;; Styles

(defcustom r-indent-style 'DEFAULT
  "The default value of `ess-indent-style'.
See the variable `r-style-alist' for how these groups map onto
different settings for variables. DEFAULT style picks
default (aka global) values from the indentation variables."
  :group 'r-indent
  :type '(choice (const DEFAULT)
                 (const RRR+)
                 (const RStudio)
                 (const RStudio-)))

(defvar r-style-alist
  `((DEFAULT
     (r-indent-offset                . ,(default-value 'r-indent-offset))
     (r-offset-arguments             . ,(default-value 'r-offset-arguments))
     (r-offset-arguments-newline     . ,(default-value 'r-offset-arguments-newline))
     (r-offset-block                 . ,(default-value 'r-offset-block))
     (r-offset-continued             . ,(default-value 'r-offset-continued))
     (r-align-nested-calls           . ,(default-value 'r-align-nested-calls))
     (r-align-arguments-in-calls     . ,(default-value 'r-align-arguments-in-calls))
     (r-align-continuations-in-calls . ,(default-value 'r-align-continuations-in-calls))
     (r-align-blocks                 . ,(default-value 'r-align-blocks))
     (r-indent-from-lhs              . ,(default-value 'r-indent-from-lhs))
     (r-indent-from-chain-start      . ,(default-value 'r-indent-from-chain-start))
     (r-indent-with-fancy-comments   . ,(default-value 'r-indent-with-fancy-comments)))

    (RRR+
     (r-indent-offset                . ,(default-value 'r-indent-offset))
     (r-offset-arguments             . ,(default-value 'r-offset-arguments))
     (r-offset-arguments-newline     . ,(default-value 'r-offset-arguments-newline))
     (r-offset-block                 . open-delim)
     (r-offset-continued             . ,(default-value 'r-offset-continued))
     (r-align-nested-calls           . ,(default-value 'r-align-nested-calls))
     (r-align-arguments-in-calls     . ,(default-value 'r-align-arguments-in-calls))
     (r-align-continuations-in-calls . ,(default-value 'r-align-continuations-in-calls))
     (r-align-blocks                 . ,(default-value 'r-align-blocks))
     (r-indent-from-lhs              . (arguments))
     (r-indent-from-chain-start      . nil)
     (r-indent-with-fancy-comments   . ,(default-value 'r-indent-with-fancy-comments)))

    (RStudio
     (r-indent-offset                . ,(default-value 'r-indent-offset))
     (r-offset-arguments             . ,(default-value 'r-offset-arguments))
     (r-offset-arguments-newline     . prev-line)
     (r-offset-block                 . ,(default-value 'r-offset-block))
     (r-offset-continued             . ,(default-value 'r-offset-continued))
     (r-align-nested-calls           . nil)
     (r-align-arguments-in-calls     . ,(default-value 'r-align-arguments-in-calls))
     (r-align-continuations-in-calls . nil)
     (r-align-blocks                 . nil)
     (r-indent-from-lhs              . (arguments))
     (r-indent-from-chain-start      . ,(default-value 'r-indent-from-chain-start))
     (r-indent-with-fancy-comments   . nil))

    (RStudio-
     (r-indent-offset                . ,(default-value 'r-indent-offset))
     (r-offset-arguments             . prev-line)
     (r-offset-arguments-newline     . prev-line)
     (r-offset-block                 . ,(default-value 'r-offset-block))
     (r-offset-continued             . ,(default-value 'r-offset-continued))
     (r-align-nested-calls           . nil)
     (r-align-arguments-in-calls     . ,(default-value 'r-align-arguments-in-calls))
     (r-align-continuations-in-calls . nil)
     (r-align-blocks                 . nil)
     (r-indent-from-lhs              . (arguments))
     (r-indent-from-chain-start      . ,(default-value 'r-indent-from-chain-start))
     (r-indent-with-fancy-comments   . nil)))
  
  "Predefined formatting styles for ESS code. Use
`r-default-style' to apply a style in all R buffers. The values
of all styles except OWN are fixed. To change the value of
variables in the OWN group, customize the variable
`r-own-style-list'. DEFAULT style picks default (aka global)
values from ESS indentation variables. In addition, ESS provides
many indentation styles, the most important being the RRR and the
RStudio variants.

RRR is the common R style that adheres closely to R internal
standards. RRR+ is the same except it also aligns blocks in
function calls with the opening delimiter, producing more
indentation. The C++ style (named like this for historical
reasons rather than any resemblance to existing C++ indentation
schemes) is halfway between these two styles and indent block
arguments from the start of the surrounding function's name.

The RStudio style closely mimics the indentation of the RStudio
editor. RStudio- is the same except it does not align arguments
in function calls, which corresponds to the settings of some
RStudio users.

ESS indentation is fully specified by the following offsets and
variables. See the documentation of these variables for examples.

Offsets:

 - `r-indent-offset': main offset inherited by other settings

 - `r-offset-arguments': offset type for function and bracket
   arguments

 - `r-offset-arguments-newline': offset type of arguments
   when ( or [ is followed by a new line.

 - `r-offset-block': offset type for brace and anonymous
   parenthesis blocks

 - `r-offset-continued': offset type for continuation lines in
   multiline statements


Overrides (implies vertical alignment):

 - `r-align-nested-calls': functions whose nested calls
   should be aligned.

 - `r-align-arguments-in-calls': calls where
   `r-offset-arguments' should be ignored

 - `r-align-continuations-in-calls': whether to ignore
   `r-offset-continued' in calls.

 - `r-align-blocks': whether to ignore `r-offset-blocks' for
   function declarations or control flow statements.


Control variables:

 - `r-indent-from-lhs': whether to indent arguments from
   left-hand side of an assignment or parameter declaration.

 - `r-indent-from-chain-start': whether to indent arguments from
   the first of several consecutive calls.

 - `r-indent-with-fancy-comments': whether to indent #, ## and
   ### comments distinctly.")



;;; Engine

(defun r-indent-line ()
  "Indent current line as ESS R code.
Return the amount the indentation changed by."
  (let ((indent (r-calculate-indent nil))
        beg shift-amt
        (case-fold-search nil)
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
        (if (> (- (point-max) pos) (point))
            (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.
      ;; Else stay at same point in text.
      (when (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos))))
    shift-amt))

(defun r-indent-exp ()
  (save-excursion
    (when current-prefix-arg
      (r-climb-to-top-level))
    (let* ((bounds (r-continuations-bounds))
           (end (cadr bounds))
           (beg (if current-prefix-arg
                    (car bounds)
                  (forward-line)
                  (point))))
      (indent-region beg end))))

(defun r-indent-call (&optional start)
  (save-excursion
    (when (r-escape-calls)
      (setq start (or start (point)))
      (skip-chars-forward "^[(")
      (forward-char)
      (r-up-list)
      (indent-region start (point)))))

(defun r-offset (offset)
  (setq offset (eval (intern (concat "r-offset-" (symbol-name offset)))))
  (when (and (not (eq offset nil))
             (listp offset)
             (or (numberp (cadr offset))
                 (eq (cadr offset) t)
                 (error "Malformed offset")))
    (setq offset (cadr offset)))
  (cond ((numberp offset)
         offset)
        ((null offset)
         0)
        (t
         r-indent-offset)))

(defun r-offset-type (offset)
  (setq offset (eval (intern (concat "r-offset-" (symbol-name offset)))))
  (if (listp offset)
      (car offset)
    offset))

(defun r-overridden-blocks ()
  (append (when (memq 'fun-decl r-align-blocks)
            (list (car r-prefixed-block-patterns)))
          (when (memq 'control-flow r-align-blocks)
            (append (cdr r-prefixed-block-patterns)
                    '("}?[ \t]*else")))))

(defun r-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as ESS code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let* ((indent-point (point))
           (state (syntax-ppss))
           (containing-sexp (cadr state))
           (prev-containing-sexp (car (last (butlast (nth 9 state))))))
      (r-back-to-indentation)
      (cond
       ;; Strings
       ((r-within-string-p state)
        (current-indentation))
       ;; Comments
       ((r-calculate-indent--comments))
       ;; Indentation of commas
       ((looking-at ",")
        (r-calculate-indent--comma))
       ;; Arguments: Closing
       ((r-call-closing-p)
        (r-calculate-indent--call-closing-delim))
       ;; Block: Contents (easy cases)
       ((r-calculate-indent--block-relatively))
       ;; Block: Prefixed block
       ((r-calculate-indent--prefixed-block-curly))
       ;; Continuations
       ((r-calculate-indent--continued))
       ;; Block: Overridden contents
       ((r-calculate-indent--aligned-block))
       ;; Block: Opening
       ((r-block-opening-p)
        (r-calculate-indent--block-opening))
       ;; Bare line
       ((and (null containing-sexp)
             (not (r-unbraced-block-p)))
        0)
       ;; Block: Closing
       ((r-block-closing-p)
        (r-calculate-indent--block 0))
       ;; Block: Contents
       ((r-block-p)
        (r-calculate-indent--block))
       ;; Arguments: Nested calls override
       ((r-calculate-indent--nested-calls))
       ;; Arguments: Contents
       (t
        (r-calculate-indent--args))))))

(defun r-calculate-indent--comments ()
  (when r-indent-with-fancy-comments
    (cond
     ;; ### or #!
     ((or (looking-at "###")
          (and (looking-at "#!")
               (= 1 (line-number-at-pos))))
      0)
     ;; Single # comment
     ((looking-at "#[^#']")
      comment-column))))

(defun r-calculate-indent--comma ()
  (when (r-within-call-p)
    (let ((indent (save-excursion
                    (r-calculate-indent--args)))
          (unindent (progn (skip-chars-forward " \t")
                           ;; return number of skiped chars
                           (skip-chars-forward ", \t"))))
      (- indent unindent))))

(defun r-calculate-indent--call-closing-delim ()
  (cond ((save-excursion
           (r-skip-blanks-backward t)
           (eq (char-before) ?,))
         (r-calculate-indent--args nil))
        ((save-excursion
           (and (r-ahead-operator-p)
                (or (r-ahead-definition-op-p)
                    (not r-align-continuations-in-calls))))
         (r-calculate-indent--continued))
        (t
         (r-calculate-indent--args 0))))

(defun r-calculate-indent--block-opening ()
  (cond
   ;; Block is an argument in a function call
   ((when containing-sexp
      (r-at-containing-sexp
        (r-behind-call-opening "[[(]")))
    (r-calculate-indent--block 0))
   ;; Top-level block
   ((null containing-sexp) 0)
   ;; Block is embedded in another block
   ((r-at-containing-sexp
      (equal (char-after) ?\{)
      (+ (current-indentation)
         (r-offset 'block))))))

(defun r-calculate-indent--aligned-block ()
  ;; Check for `else' opening
  (if (and (memq 'control-flow r-align-blocks)
           (looking-at "else\\b")
           (r-climb-if-else))
      (progn
        (when (looking-at "else\\b")
          (r-skip-curly-backward))
        (current-column))
    ;; Check for braced and unbraced blocks
    (r-save-excursion-when-nil
      (let ((offset (if (looking-at "[{})]")
                        0 (r-offset 'block))))
        (when (and (cond
                    ;; Unbraced blocks
                    ((r-climb-block-prefix))
                    ;; Braced blocks
                    (containing-sexp
                     (when (r-at-containing-sexp
                             (looking-at "{"))
                       (r-escape-prefixed-block))))
                   (some 'looking-at (r-overridden-blocks)))
          (+ (current-column) offset))))))

(defun r-calculate-indent--block-relatively ()
  (r-save-excursion-when-nil
    (let ((offset (if (looking-at "[})]") 0 (r-offset 'block)))
          (start-line (line-number-at-pos)))
      (cond
       ;; Braceless block continuations: only when not in a call
       ((r-save-excursion-when-nil
          (and (not (looking-at "{"))
               (r-goto-char (r-unbraced-block-p))
               (not (looking-at "function\\b"))
               (or (null containing-sexp)
                   (r-at-containing-sexp
                     (not (looking-at "("))))))
        (r-maybe-climb-broken-else 'same-line)
        (r-skip-curly-backward)
        (+ (current-column)
           (r-offset 'block)))
       ;; Don't indent relatively other continuations
       ((r-ahead-continuation-p)
        nil)
       ;; If a block already contains an indented line, we can indent
       ;; relatively from that first line
       ((r-save-excursion-when-nil
          (and (not (looking-at "}"))
               containing-sexp
               (goto-char containing-sexp)
               (looking-at "{")
               (progn
                 (forward-line)
                 (r-back-to-indentation)
                 (/= (line-number-at-pos) start-line))
               (not (looking-at "[ \t]*\\(#\\|$\\)"))
               (save-excursion
                 (or (r-jump-expression)
                     (r-jump-continuations))
                 (< (line-number-at-pos) start-line))))
        (current-column))
       ;; If a block is not part of a call, we can indent relatively
       ;; from the opening {. First check that enclosing { is first
       ;; thing on line
       ((and containing-sexp
             (not (r-unbraced-block-p))
             (goto-char containing-sexp)
             (r-block-opening-p)
             (equal (point) (save-excursion
                              (r-back-to-indentation)
                              (point))))
        (+ (current-column) offset))))))

(defun r-arg-block-p ()
  (unless (or (null containing-sexp)
              ;; Unbraced blocks in a { block are not arg blocks
              (and (r-unbraced-block-p)
                   (r-at-containing-sexp
                     (looking-at "{"))))
    (cond
     ;; Unbraced body
     ((r-at-indent-point
        (and (r-unbraced-block-p)
             (goto-char containing-sexp)
             (r-behind-call-opening "[[(]")))
      'body)
     ;; Indentation of opening brace as argument
     ((r-at-containing-sexp
        (r-behind-call-opening "[[(]"))
      'opening)
     ;; Indentation of body or closing brace as argument
     ((r-at-containing-sexp
        (and (or (looking-at "{")
                 (r-behind-block-paren-p))
             prev-containing-sexp
             (goto-char prev-containing-sexp)
             (r-behind-call-opening "[[(]")))
      'body))))

(defun r-calculate-indent--block (&optional offset)
  (let ((arg-block (r-arg-block-p)))
    (cond (arg-block
           (r-calculate-indent--arg-block offset arg-block))
          (t
           ;; Block is not part of an arguments list. Climb over any
           ;; block opening (function declaration, etc) to indent from
           ;; starting indentation.
           (or (r-climb-block-prefix)
               (and (goto-char containing-sexp)
                    (r-climb-block-prefix)))
           (+ (current-indentation) (or offset (r-offset 'block)))))))

(defun r-calculate-indent--arg-block (offset arg-block)
  (let* ((block-type (cond ((or (r-at-containing-sexp
                                  (and (eq arg-block 'body)
                                       (r-climb-block-prefix "function")))
                                (r-at-indent-point
                                  (and (eq arg-block 'opening)
                                       (r-backward-sexp 2)
                                       (looking-at "function\\b"))))
                            'fun-decl)
                           ((r-at-indent-point
                              (r-unbraced-block-p))
                            'unbraced)
                           ((r-at-containing-sexp
                              (not (r-ahead-attached-name-p)))
                            'bare-block)
                           (t)))
         (call-pos (if (and (not (eq block-type 'unbraced))
                            (not (eq arg-block 'opening)))
                       (goto-char prev-containing-sexp)
                     (prog1 containing-sexp
                       (goto-char indent-point)))))
    (r-calculate-indent--args offset (r-offset-type 'block)
                                call-pos indent-point block-type)))

;; This function is currently the speed bottleneck of the indentation
;; engine. This is due to the need to call (r-maximum-args-indent)
;; to check if some previous arguments have been pushed off from their
;; natural indentation: we need to check the whole call. This is very
;; inefficient especially when indenting a region containing a large
;; function call (e.g. some dplyr's data cleaning code). Should be
;; solved by implementing a cache as in (syntax-ppss), though it's
;; probably not worth the work.
(defun r-calculate-indent--args (&optional offset type call-pos to block)
  (let* ((call-pos (or call-pos containing-sexp))
         (max-col (prog1 (unless (eq type 'prev-line)
                           (r-maximum-args-indent call-pos to))
                    (goto-char call-pos)))
         (override (and r-align-arguments-in-calls
                        (save-excursion
                          (r-climb-object)
                          (some 'looking-at r-align-arguments-in-calls))))
         (type-sym (cond (block 'block)
                         ((looking-at "[[:blank:]]*[([][[:blank:]]*\\($\\|#\\)")
                          'arguments-newline)
                         (t 'arguments)))
         (type (or type
                   (and override 'open-delim)
                   (r-offset-type type-sym)))
         (offset (or offset
                     (and (not block) (eq type 'open-delim) 0)
                     (r-offset type-sym)))
         (indent
          (cond
           ;; Indent from opening delimiter
           ((eq type 'open-delim)
            (r-calculate-indent--args-open-delim))
           ;; Indent from attached name
           ((eq type 'prev-call)
            (r-calculate-indent--args-prev-call))
           ;; Indent from previous line indentation
           ((eq type 'prev-line)
            (r-calculate-indent--args-prev-line))
           (t
            (error "Malformed offset")))))
    (if max-col
        (r-adjust-argument-indent indent offset max-col block)
      (+ indent offset))))

(defun r-calculate-indent--args-open-delim ()
  (forward-char)
  (current-column))

(defun r-calculate-indent--args-prev-call ()
  ;; Handle brackets chains such as ][ (cf data.table)
  (r-climb-chained-delims)
  ;; Handle call chains
  (if r-indent-from-chain-start
      (while (and (r-backward-sexp)
                  (when (looking-back "[[(][ \t,]*" (line-beginning-position))
                    (goto-char (match-beginning 0)))))
    (r-backward-sexp))
  (when r-indent-from-lhs
    (r-climb-lhs))
  (if (and nil
           (eq block 'fun-decl)
           (not (eq arg-block 'opening))
           (not (eq (r-offset-type type-sym) 'open-delim)))
      (+ (r-offset 'block) (current-column))
    (current-column)))

(defun r-calculate-indent--args-prev-line ()
  (r-at-indent-point
    (cond
     ;; Closing delimiters are actually not indented at
     ;; prev-line, but at opening-line
     ((looking-at "[]})]")
      (r-up-list -1)
      (when (looking-at "{")
        (r-climb-block-prefix))
      (current-indentation))
     ;; Function blocks need special treatment
     ((and (eq type 'prev-line)
           (eq block 'fun-decl))
      (goto-char containing-sexp)
      (r-climb-block-prefix)
      (current-indentation))
     ;; Regular case
     (t
      ;; Find next non-empty line to indent from
      (while (and (= (forward-line -1) 0)
                  (looking-at "[ \t]*\\($\\|#\\)")))
      (goto-char (r-line-end-position))
      ;; Climb relevant structures
      (unless (r-climb-block-prefix)
        (when (eq (char-before) ?,)
          (forward-char -1))
        (r-climb-expression)
        (r-climb-continuations))
      ;; The following ensures that only the first line
      ;; counts. Otherwise consecutive statements would get
      ;; increasingly more indented.
      (when (and block
                 containing-sexp
                 (not (eq block 'unbraced))
                 (save-excursion
                   (/= (line-number-at-pos)
                       (progn (goto-char containing-sexp)
                              (line-number-at-pos)))))
        (setq offset 0))
      (current-indentation)))))

;; Indentation of arguments needs to keep track of how previous
;; arguments are indented. If one of those has a smaller indentation,
;; we push off the current line from its natural indentation. For
;; block arguments, we still need to push off this column so we ignore
;; it.
(defun r-adjust-argument-indent (base offset max-col push)
  (if push
      (+ (min base max-col) offset)
    (min (+ base offset) max-col)))

;; When previous arguments are shifted to the left (can happen in
;; several situations) compared to their natural indentation, the
;; following lines should not get indented past them. The following
;; function checks the minimum indentation for all arguments of the
;; current function call or bracket indexing.
(defun r-maximum-args-indent (&optional from to)
  (let* ((to (or to (point)))
         (to-line (line-number-at-pos to))
         (from-line (progn
                      (goto-char (1+ (or from containing-sexp)))
                      (line-number-at-pos)))
         (prev-pos (1- (point)))
         max-col)
    (while (< (line-number-at-pos) to-line)
      (forward-line)
      (r-back-to-indentation)
      ;; Ignore the line with the function call, the line to be
      ;; indented, and empty lines.
      (unless (or (>= (line-number-at-pos) to-line)
                  (looking-at "[ \t]*\\($\\|#\\)"))
        (let ((indent (cond
                       ;; First line: minimum indent is right after (
                       ((= (line-number-at-pos) from-line)
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (current-column)))
                       ;; Handle lines starting with a comma
                       ((save-excursion
                          (looking-at ","))
                        (+ (current-indentation) 2))
                       (t
                        (current-indentation)))))
          (setq max-col (min indent (or max-col indent))))))
    max-col))

;; Move to leftmost side of a call (either the first letter of its
;; name or its closing delim)
(defun r-move-to-leftmost-side ()
  (when (or (looking-at "[({]")
            (r-behind-call-p))
    (r-save-excursion-when-nil
      (let ((start-col (current-column)))
        (skip-chars-forward "^{[(")
        (forward-char)
        (r-up-list)
        (forward-char -1)
        (< (current-column) start-col)))))

(defun r-max-col ()
  (let ((max-col (point)))
    (save-excursion
      (while (< (point) indent-point)
        (unless (and r-indent-with-fancy-comments
                     (looking-at "### "))
          (setq max-col (min max-col (current-column))))
        (forward-line)
        (r-back-to-indentation)))
    max-col))

(defun r-calculate-indent--prefixed-block-curly ()
  (when (looking-at "{")
    (r-save-excursion-when-nil
      (let ((block-type (r-climb-block-prefix)))
        (cond ((r-save-excursion-when-nil
                 (and (memq 'fun-decl-opening r-indent-from-lhs)
                      (string= block-type "function")
                      (r-climb-operator)
                      (r-behind-assignment-op-p)
                      (r-climb-expression)))
               (current-column))
              ((= (save-excursion
                    (back-to-indentation)
                    (point))
                  (point))
               (r-calculate-indent--continued)))))))

(defun r-calculate-indent--continued ()
  "If a continuation line, return an indent of this line,
otherwise nil."
  (save-excursion
    (let* ((start-line (line-number-at-pos))
           (prev-pos 0)
           (cascade (eq (r-offset-type 'continued) 'cascade))
           (climbed (r-climb-continuations cascade))
           max-col)
      (when climbed
        (cond
         ;; Overridden calls
         ((and r-align-continuations-in-calls
               (not (eq climbed 'def-op))
               containing-sexp
               (save-excursion
                 (goto-char containing-sexp)
                 (looking-at "[[(]")))
          (setq max-col (r-max-col))
          (r-move-to-leftmost-side)
          (+ (min (current-column) max-col)
             (if (eq climbed 'def-op)
                 (r-offset 'continued)
               0)))
         ;; Regular case
         (t
          (let ((first-indent (or (eq climbed 'def-op)
                                  (save-excursion
                                    (when (r-ahead-closing-p)
                                      (r-climb-expression))
                                    (not (r-climb-continuations cascade))))))
            ;; Record all indentation levels between indent-point and
            ;; the line we climbed. Some lines may have been pushed off
            ;; their natural indentation. These become the new
            ;; reference.
            (setq max-col (r-max-col))
            ;; Indenting continuations from the front of closing
            ;; delimiters looks better
            (when
                (r-ahead-closing-p)
              (backward-char))
            (+ (min (current-column) max-col)
               (cond
                ((eq (r-offset-type 'continued) 'cascade)
                 (r-offset 'continued))
                (first-indent
                 (r-offset 'continued))
                (t
                 0))))))))))

(defun r-calculate-indent--nested-calls ()
  (when r-align-nested-calls
    (let ((calls (mapconcat 'identity r-align-nested-calls "\\|"))
          match)
      (save-excursion
        (and containing-sexp
             (looking-at (concat "\\(" calls "\\)("))
             (setq match (match-string 1))
             (goto-char containing-sexp)
             (looking-at "(")
             (r-backward-sexp)
             (looking-at (concat match "("))
             (current-column))))))

(provide 'r-indent)
;;; r-indent.el ends here
