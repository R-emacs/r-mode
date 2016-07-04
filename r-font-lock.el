;;; r-font-lock.el --- Font lock support R mode -*- lexical-binding: t; -*-
;; 
;; Copyright © 2016 Vitalie Spinu, Lionel Henry
;;
;; Author: Vitalie Spinu <spinuvit@gmail.com>
;; 
;; URL: http://github.com/r-emacs/r-mode
;; Keywords: languages R
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; 
;; This file is not part of GNU Emacs.
;; 
;;; License:
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;; 
;;; Code:

(defgroup r-font-lock nil
  "Font Lock customization used in r-mode."
  :group 'r
  :prefix "r-")

(defvar r-font-lock-available-features
  '((comments   "Comments")
    (strings    "Strings")
    (backquotes "Backquoted Vars")
    (modifiers  "Modifiers"            :keyword r-keyword:modifiers)
    (keywords   "Keywords"             :keyword r-keyword:keywords)
    (constants  "Constants"            :keyword r-keyword:constants)
    (F&T        "F & T"                :keyword r-keyword:F&T)
    (fun-calls  "Function Calls"       :keyword r-keyword:fun-calls)
    (fun-defs   "Function Definitions" :keyword r-keyword:fun-defs)
    (ops-assign "Assign Operators"     :keyword r-keyword:assign-ops)
    (ops-other  "Operators"            :keyword r-keyword:operators)
    (ops-=      "="                    :keyword r-keyword:=)
    (%op%       "%op%")
    (numbers    "Numbers"              :keyword r-keyword:numbers)
    (delimiters "Delimiters ()[]{}"      :keyword r-keyword:delimiters))
  "An alist of available font-lock features for the R mode.
Each element of the alist is a list of the form (FEATURE NAME
ACTIVE [REST ...]). FEATURE is a symbol naming the feature. NAME
is the human visible name of the features (for use in completion
and menus). ACTIVE is a Boolean value indicating if the feature
is active or not. TYPE is the type of the feature, currently
either nil for plain options and :keyword for Font Lock
keywords. The optional REST list is type specific.  For :keyword
type, it consists of the variable name that contains the
respective font lock keyword.")

(defcustom r-font-lock-features
  '(comments strings modifiers keywords constants F&T fun-calls fun-defs ops-assign %op%)
  "Active font lock features in R mode."
  :group 'r-font-lock
  :type `(set ,@(cl-loop for (k v) in r-font-lock-available-features
                         collect `(const :tag ,v ,k))))

(defvar-local r-font-lock-keywords nil
  "[Internal] Currently active font lock keywords in R mode buffers.")

(defvar r-backquoted-face 'r-backquoted-face)
(defface r-backquoted-face
  '((default (:inherit font-lock-variable-name-face)))
  "Font Lock face for backquoted names."
  :group 'r-font-lock)

(defvar r-operator-face 'r-operator-face)
(defface r-operator-face
  '((default (:inherit font-lock-type-face)))
  "Font Lock face used to highlight operators in r-mode buffers."
  :group 'r-font-lock)

(defvar r-function-call-face 'r-function-call-face)
(defface r-function-call-face
  '((default (:slant normal :inherit font-lock-function-name-face)))
  "Font Lock face used to highlight function calls in ess buffers."
  :group 'r-font-lock)

(defvar r-numbers-face 'r-numbers-face)
(defface r-numbers-face
  '((default (:slant normal :inherit font-lock-type-face)))
  "Font Lock face used to highlight numbers in R mode buffers."
  :group 'r-font-lock)

(defvar r-delimiters-face 'r-delimiters-face)
(defface r-delimiters-face
  '((default (:slant normal :inherit font-lock-type-face)))
  "Font Lock face used to highlight numbers in R mode buffers."
  :group 'r-font-lock)

(defvar r-keyword:keywords
  (let ((keywords '("while" "for" "in" "repeat" "if" "else" "switch" "break" "next" "function"
                    "return" "message" "warning" "stop")))
    (cons (concat "\\<" (regexp-opt keywords 'enc-paren) "\\>") 'font-lock-keyword-face)))

(defvar r-keyword:modifiers
  (let ((modifyiers '("library" "attach" "detach" "source" "require")))
    (cons (concat "\\<" (regexp-opt modifyiers 'enc-paren) "\\>") 'font-lock-builtin-face))
  "Font Lock keyword for R modifiers")

(defvar r-keyword:constants
  (let ((consts '("TRUE" "FALSE" "NA" "NULL" "Inf" "NaN"
                  "NA_integer_" "NA_real_" "NA_complex_" "NA_character_")))
    (cons (concat "\\<" (regexp-opt consts 'enc-paren) "\\>") 'font-lock-constant-face))
  "Font Lock constants keyword.")

(defvar r-keyword:F&T
  (cons "\\b[FT]\\b" 'font-lock-constant-face)
  "Font Lock keyword for T and F symbols in R mode.")

(defvar r-keyword:fun-calls
  (cons "\\(\\sw+\\)[\t ]*(" '(1 'r-function-call-face keep))
  "Font Lock for R's function calls.")

(defvar r-keyword:fun-defs
  (let ((fun-regexp "\\(\\sw+\\)[ \t]*\\(<-\\)[ \t\n]*function\\b"))
    (cons fun-regexp '(1 font-lock-function-name-face nil)))
  "Font Lock keyword for R function definitions.")

(defvar r-keyword:assign-ops
  (let ((ops '("<<-" "<-" "->" "->>")))
    (cons (regexp-opt ops) 'font-lock-keyword-face))
  "Font Lock keywords for R assign operators")

(defvar r-keyword:operators
  (cons "[-=+></%]+" 'r-operator-face)
  "Font Lock keyword for R's operators.")

(defvar r-keyword:=
  (cons "=" 'r-operator-face)
  "Font Lock keyword for = in R mode.")

(defvar r-keyword:numbers
  (cons "\\b[0-9]*[.eE]?[0-9]+[eEL]?\\b" 'r-numbers-face)
  "Font Lock for R's numbers.")

(defvar r-keyword:delimiters
  (cons "\\s(\\|\\s)" 'r-delimiters-face)
  "Font Lock keyword for R's parenthesis.")

(defun r--extract-fl-keywords ()
  "Return active font lock keywords.
Use active features listed in `r-font-lock-keywords' to extract
keywords from `r-font-lock-available-features'."
  (cl-loop for (name _ type sym) in r-font-lock-available-features
           when (and (eq type :keyword) (memq name r-font-lock-features))
           collect (symbol-value sym)))

(defun r--font-lock-toggle-keyword (name)
  (interactive)
  (if (memq name r-font-lock-features)
      (setq r-font-lock-features (delq name r-font-lock-features))
    (push name r-font-lock-features))
  ;; refresh font-lock defaults in all necessary buffers
  (let ((mode major-mode)
        (kwds (r--extract-fl-keywords)))
    (mapc (lambda (b)
            (with-current-buffer b
              (when (eq major-mode mode)
                (setq r-font-lock-keywords kwds)
                (font-lock-refresh-defaults))))
          (buffer-list))))

(defun r--generate-font-lock-submenu (menu)
  "Internal, used to generate ESS font-lock submenu."
  (append (cl-loop for (name desc-name) in r-font-lock-available-features
                   collect `[,desc-name
                             (lambda () (interactive)
                               (r--font-lock-toggle-keyword ',name))
                             :style toggle
                             :enable t
                             :selected ,(not (null (memq name r-font-lock-features)))])
          (list "-----"
                ["Save to custom" (lambda () (interactive)
                                    (customize-save-variable 'r-font-lock-features
                                                             r-font-lock-features))
                 t])))

(defun r-font-lock-syntactic-face-function (state)
  "Function used as `font-lock-syntactic-face-function'.
See `font-lock-syntactic-face-function' for the definition of STATE."
  (let ((string-end (save-excursion
                      (and (nth 3 state)
                           (r-goto-char (nth 8 state))
                           (r-forward-sexp)
                           (point)))))
    (when (eq (nth 3 state) ?`)
      (put-text-property (nth 8 state) string-end 'ess-r-backquoted t))
    (cond
     ((eq (nth 3 state) ?%)
      (if (memq '%op% r-font-lock-features)
          'font-lock-keyword-face
        'default))
     ((save-excursion
        (and (r-goto-char string-end)
             (r-looking-at "<-")
             (r-goto-char (match-end 0))
             (r-looking-at "function\\b")))
      (if (memq 'fun-defs r-font-lock-features)
          'font-lock-function-name-face
        'default))
     ((save-excursion
        (and (r-goto-char string-end)
             (r-looking-at "(")))
      (if (memq 'fun-calls r-font-lock-features)
          'ess-function-call-face
        'default))
     ((eq (nth 3 state) ?`)
      (if (memq 'backquotes r-font-lock-features)
          'r-backquoted-face
        'default))
     ((nth 3 state)
      'font-lock-string-face
      (if (memq 'strings r-font-lock-features)
          'font-lock-string-face
        'default))
     (t
      (if (memq 'comments r-font-lock-features)
          'font-lock-comment-face
        'default)))))

(provide 'r-font-lock)
;;; r-font-lock.el ends here
