;;; r-roxy.el --- convenient editing of in-code roxygen documentation
;;
;; Copyright (C) 2016 Vitalie Spinu, Lionel Henry
;;
;; Authors: Henning Redestig <henning.red * go0glemail c-m>,
;;          Vitalie Spinu <spinuvit@gmail.com>,
;;          Lionel Henry <lionel.hry@gmail.com>
;; Keywords: convenience, R, tools
;; Created: 3 Jul 2016
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 
;;; Commentary:
;;
;; This file was adapted from the Henning Redestig's ess-roxy.el.
;; 
;;; Code:


(require 'r-syntax)
(require 'cl-lib)

(defgroup r-roxy nil
  "Mode for editing in-code Roxygen documentation."
  :group 'r
  :prefix "r-")

(defcustom r-roxy-tags-noparam '("export" "noRd")
  "The tags used in roxygen fields that can be used alone.
Used to decide highlighting and tag completion."
  :group 'r-roxy
  :type '(repeat string))

(defcustom r-roxy-tags-param '("author" "aliases" "concept" "details"
                                 "examples" "format" "keywords"
                                 "method" "exportMethod"
                                 "name" "note" "param"
                                 "include" "references" "return"
                                 "seealso" "source" "docType"
                                 "title" "TODO" "usage" "import"
                                 "exportClass" "exportPattern" "S3method"
                                 "inheritParams"
                                 "importFrom" "importClassesFrom"
                                 "importMethodsFrom" "useDynLib"
                                 "rdname" "section" "slot")
  "The tags used in roxygen fields that require a parameter.
Used to decide highlighting and tag completion."
  :group 'r-roxy
  :type '(repeat string))

(defcustom r-roxy-template-alist
  (list (cons "description"  ".. content for \\description{} (no empty lines) ..")
        (cons "details" ".. content for \\details{} ..")
        (cons "title" "")
        (cons "param"  "")
        (cons "return" "")
        (cons "author" user-full-name))
  "The tags and defaults to insert when creating empty templates.
Param is a place holder for where to enter
parameters. Description and details do not use @ tags, but are
instead placed at the beginning of the entry (and should
therefore also be at the beginning of this template to give
syntactically correct roxygen entries)"
  :group 'r-roxy
  :type '(alist :value-type (group string)))

;; fixme: this should be done automatically from comment-add or something else
(defcustom r-roxy-str "##'"
  "Prefix string to insert before each line in new roxygen
blocks. In existing roxygen blocks, the prefix is taken from
the line at point"
  :group 'r-roxy
  :type 'string)

(defcustom r-roxy-prefix "^#+'"
  "Regular expression to recognize roxygen blocks."
  :group 'r-roxy
  :type 'string)

;; (defvar r-roxy-insert-prefix-on-newline t
;;   "When non-nil, `r-newline-and-indent' will make sure the new
;; line starts with the roxy prefix.")

(defvar r-roxy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'r-roxy-next-entry)
    (define-key map (kbd "p") 'r-roxy-previous-entry)
    (define-key map (kbd "t") 'r-roxy-toggle-roxy-region)
    map))

(defvar r-roxy-font-lock-keywords
  `((,(concat r-roxy-prefix " *\\([@\\]"
              (regexp-opt r-roxy-tags-param t)
              "\\)\\>")
     (1 'font-lock-keyword-face prepend))
    (,(concat r-roxy-prefix " *\\([@\\]"
              (regexp-opt '("param" "importFrom" "importClassesFrom"
                            "importMethodsFrom") t)
              "\\)\\>\\(?:[ \t]+\\(\\sw+\\)\\)?")
     (1 'font-lock-keyword-face prepend)
     (3 'font-lock-variable-name-face prepend))
    (,(concat "[@\\]" (regexp-opt r-roxy-tags-noparam t) "\\>")
     (0 'font-lock-variable-name-face prepend))
    (,(concat r-roxy-prefix)
     (0 'bold prepend))))

(defun r-roxy-tag-completion ()
  "Completion data for emacs >= 24"
  (when (save-excursion (re-search-backward "@\\<\\(\\w*\\)" (point-at-bol) t))
    (let ((token (match-string-no-properties 1))
          (beg (match-beginning 1))
          (end (match-end 1)))
      (when (and end (= end (point)))
        (list beg end (append r-roxy-tags-noparam r-roxy-tags-param) :exclusive 'no)))))


;;;*;;; Info Retrieval

(defun r-roxy-entry-p ()
  "True if point is in a roxy entry."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat r-roxy-prefix))))

(defun r-roxy-in-header-p ()
  "Return t if point is in the description/details field."
  (save-excursion
    (let ((res t)
          (cont (r-roxy-entry-p)))
      (beginning-of-line)
      (while cont
        (if (looking-at (concat r-roxy-prefix " *[@].+"))
            (progn (setq res nil)
                   (setq cont nil)))
        (setq cont (and (= (forward-line -1) 0) (r-roxy-entry-p))))
      res)))

(defun r-roxy-guess-prefix (&optional not-here)
  "Guess the prefix used in the current roxygen block.
If `not-here' is non-nil, guess the prefix for nearest roxygen
block before the point"
  (save-excursion
    (if (r-roxy-entry-p)
        (progn
          (goto-char (point-at-bol))
          (search-forward-regexp r-roxy-prefix))
      (if not-here
          (search-backward-regexp r-roxy-prefix)))
    (if (or not-here (r-roxy-entry-p))
        (match-string 0)
      r-roxy-str)))

(defun r-roxy-adaptive-fill-function ()
  "Return prefix for filling paragraph or nil if not determined."
  (when (r-roxy-entry-p)
    (let ((roxy-str (car (split-string (r-roxy-guess-prefix) "'"))))
      (if (r-roxy-in-header-p)
          (save-excursion
            (r-back-to-roxy)
            (re-search-forward "\\([ \t]*\\)" (line-end-position) t)
            (concat roxy-str "' " (match-string 1)))
        (concat roxy-str "' " (make-string r-indent-offset ? ))))))

(defun r-roxy-beg-of-entry ()
  "Get point at start of current entry, 0 if not in entry."
  (save-excursion
    (let (beg)
      (beginning-of-line)
      (setq beg -1)
      (if (not (r-roxy-entry-p))
          (setq beg 0)
        (setq beg (point)))
      (while (and (= (forward-line -1) 0) (r-roxy-entry-p))
        (setq beg (point)))
      beg)))

(defun r-roxy-end-of-entry ()
  "Get point at end of current entry, 0 if not in entry."
  (save-excursion
    (let ((end))
      (end-of-line)
      (setq end -1)
      (if (not (r-roxy-entry-p))
          (setq end 0)
        (setq end (point)))
      (while (and (= (forward-line 1) 0) (r-roxy-entry-p))
        (end-of-line)
        (setq end (point)))
      end)))

(defun r-roxy-beg-of-field ()
  "Get point at beginning of current field, 0 if not in entry."
  (save-excursion
    (let (cont beg)
      (beginning-of-line)
      (setq beg 0)
      (setq cont t)
      (while (and (r-roxy-entry-p) cont)
        (setq beg (point))
        (if (looking-at (concat r-roxy-prefix " *[@].+"))
            (setq cont nil))
        (if (r-roxy-in-header-p)
            (if (looking-at (concat r-roxy-prefix " *$"))
                (progn
                  (forward-line 1)
                  (setq beg (point))
                  (setq cont nil))))
        (if cont (setq cont (= (forward-line -1) 0))))
      beg)))

(defun r-roxy-end-of-field ()
  "Get point at end of current field, 0 if not in entry."
  (save-excursion
    (let ((end nil)
          (cont nil))
      (setq end 0)
      (if (r-roxy-entry-p) (progn (end-of-line) (setq end (point))))
      (beginning-of-line)
      (forward-line 1)
      (setq cont t)
      (while (and (r-roxy-entry-p) cont)
        (save-excursion
          (end-of-line)
          (setq end (point)))
        (if (or (and (r-roxy-in-header-p)
                     (looking-at (concat r-roxy-prefix " *$")))
                (looking-at (concat r-roxy-prefix " *[@].+")))
            (progn
              (forward-line -1)
              (end-of-line)
              (setq end (point))
              (setq cont nil)))
        (if cont (setq cont (= (forward-line 1) 0))))
      end)))

(defun r-roxy-current-field ()
  "Return the name of the field at point."
  (and (not (r-roxy-in-header-p))
       (save-excursion
         (goto-char (r-roxy-beg-of-field))
         (if (re-search-forward (concat r-roxy-prefix
                                        "[ \t]+@\\([[:alpha:]]+\\)")
                                (line-end-position) t)
             (match-string-no-properties 1)))))

(defun r-roxy-get-args-list-from-def ()
  "Get args list for the current function."
  (save-excursion
    (r-roxy-goto-func-def)
    (let ((args (r-roxy-get-function-args)))
      (mapcar (lambda (x) (cons x '(""))) args))))

(defun r-roxy-find-par-end (stop-point &rest stoppers)
  (mapc #'(lambda (stopper)
            (when (and (> stop-point (point))
                       (save-excursion
                         (re-search-forward stopper stop-point t)))
              (setq stop-point (match-beginning 0))))
        stoppers)
  (save-excursion
    (goto-char stop-point)
    (line-end-position 0)))

(defun r-roxy-get-function-args ()
  "Return the arguments specified for the current function as a
list of strings."
  (save-excursion
    (let ((args-txt
           (progn
             (beginning-of-defun)
             (buffer-substring-no-properties
              (progn
                (search-forward-regexp "\\([=,-]+ *function *\\|^\s*function\\)" nil nil 1)
                (+ (point) 1))
              (progn
                (r-roxy-match-paren)
                (point))))))
      (setq args-txt (replace-regexp-in-string "#+[^\"']*\n" "" args-txt))
      (setq args-txt (replace-regexp-in-string "([^)]+)" "" args-txt))
      (setq args-txt (replace-regexp-in-string "=[^,]+" "" args-txt))
      (setq args-txt (replace-regexp-in-string "[ \t\n]+" "" args-txt))
      (split-string args-txt ","))))


;;;*;;; Navigation

(defun r-back-to-roxy ()
  "Go to roxy prefix."
  (progn
    (end-of-line)
    (re-search-backward (concat r-roxy-prefix " ?") (point-at-bol))
    (goto-char (match-end 0))))

(defun r-roxy-goto-func-def ()
  "Go to the start of function.
Point must be inside or below the current roxygen entry. Throw
error otherwise."
  (if (r-roxy-entry-p)
      (progn
        (r-roxy-goto-end-of-entry)
        (forward-line 1)
        (beginning-of-line))
    (goto-char (car (end-of-defun)))))

(defun r-roxy-goto-end-of-entry ()
  "Put point at the top of the entry at point or above the
function at point. Return t if the point is left in a roxygen
entry, otherwise nil. Error if point is not in function or
roxygen entry."
  (if (not (r-roxy-entry-p))
      (progn
        (goto-char (nth 0 (end-of-defun)))
        (forward-line -1)))
  (if (r-roxy-entry-p)
      (progn
        (goto-char (r-roxy-end-of-entry))
        t)
    (forward-line)
    nil))

(defun r-roxy-goto-beg-of-entry ()
  "put point at the top of the entry at point or above the
function at point. Return t if the point is left in a roxygen
entry, otherwise nil. Error if point is not in function or
roxygen entry."
  (if (not (r-roxy-entry-p))
      (progn
        (goto-char (nth 0 (end-of-defun)))
        (forward-line -1)))
  (if (r-roxy-entry-p)
      (progn
        (goto-char (r-roxy-beg-of-entry))
        t)
    (forward-line)
    nil))

(defun r-roxy-match-paren ()
  "Go to the matching parenthesis"
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))


;;;*;;; Manipulation

(defun r-roxy-narrow-to-field ()
  "Go to to the start of current field"
  (interactive)
  (let ((beg (r-roxy-beg-of-field))
        (end (r-roxy-end-of-field)))
    (narrow-to-region beg end)))

(defun r-roxy-maybe-indent-line ()
  "Indent line when point is in a field, but not in its first line."
  (when (and (not (r-roxy-in-header-p))
             (not (equal (r-roxy-current-field) "examples"))
             (save-excursion
               (beginning-of-line)
               (let ((line-n (count-lines 1 (point))))
                 (goto-char (r-roxy-beg-of-field))
                 (not (equal line-n (count-lines 1 (point)))))))
    (r-back-to-roxy)
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))
    (insert (make-string r-indent-offset ? ))))

(defmacro r-roxy-with-filling-context (examples &rest body)
  (declare (indent 1) (debug (&rest form)))
  `(let ((comment-start "#+'[ \t]+#")
         (comment-start-skip "#+'[ \t]+# *")
         (comment-use-syntax nil)
         (adaptive-fill-first-line-regexp (concat r-roxy-prefix "[ \t]*"))
         (paragraph-start (concat "\\(" r-roxy-prefix "\\(" paragraph-start
                                  "\\|[ \t]*@" "\\)" "\\)\\|\\(" paragraph-start "\\)"))
         (temp-table (if ,examples
                         (make-syntax-table S-syntax-table)
                       Rd-mode-syntax-table)))
     (when ,examples
       ;; Prevent the roxy prefix to be interpreted as comment or string
       ;; starter
       (modify-syntax-entry ?# "w" temp-table)
       (modify-syntax-entry ?' "w" temp-table))
     ;; Neutralise (comment-normalize-vars) because it modifies the
     ;; comment-start regexp in such a way that paragraph filling of
     ;; comments in @examples fields does not work
     (cl-letf (((symbol-function 'comment-normalize-vars) #'ignore))
       (with-syntax-table temp-table
         ,@body))))

;; (defun r-roxy-insert-args (args &optional point)
;;   "Insert an ARGS list at POINT.
;; POINT defaults to the end of roxygen entry."
;;   (let* ((arg-des nil)
;;          (roxy-str (r-roxy-guess-prefix)))
;;     (if (or (not point) (< point 1))
;;         (progn
;;           (r-roxy-goto-end-of-entry)
;;           (beginning-of-line)
;;           (if (not (looking-at "\="))
;;               (progn
;;                 (end-of-line))))
;;       (goto-char point))
;;     (while (stringp (car (car args)))
;;       (setq arg-des (pop args))
;;       (unless (string= (car arg-des) "")
;;         (progn
;;           (insert (concat "\n"
;;                           roxy-str " @param " (car arg-des) " "))
;;           (insert
;;            (r-replace-in-string (concat (car (cdr arg-des))) "\n"
;;                                 (concat "\n" roxy-str)))
;;           (fill-paragraph))))))


;;;*;;; Other stuff from ESS roxy

;; (defcustom r-roxy-package "roxygen2"
;;   "The name of the R package to use for Roxygen."
;;   :group 'r-roxy
;;   :type 'string)

;; (defcustom r-roxy-fill-param-p nil
;;   "Non-nil causes parameter descriptions to be filled (word-wrapped) upon `r-roxy-update-entry'."
;;   :group 'r-roxy
;;   :type '(choice (const :tag "Off" nil)
;;                  (const :tag "On" t)))

;; (defcustom r-roxy-hide-show-p nil
;;   "Non-nil means r-roxy uses hs-minor-mode for block hiding with TAB."
;;   :group 'r-roxy
;;   :type '(choice (const :tag "Off" nil)
;;                  (const :tag "On" t)))

;; (defcustom r-roxy-start-hidden-p nil
;;   "Non-nil means all blocks should be hidden from start."
;;   :group 'r-roxy
;;   :type '(choice (const :tag "Off" nil)
;;                  (const :tag "On" t)))


;;;*;;; Updating (requires sub-process)

;; (defun r-roxy-merge-args (fun ent)
;;   "Take two args lists (alists) and return their union. Result
;; holds all keys from both fun and ent but no duplicates and
;; association from ent are preferred over entries from fun. Also,
;; drop entries from ent that are not in fun and are associated with
;; the empty string."
;;   (let ((res-arg nil)
;;         (arg-des))
;;     (while (stringp (car (car fun)))
;;       (setq arg-des (pop fun))
;;       (if (assoc (car arg-des) ent)
;;           (setq res-arg
;;                 (cons (cons (car arg-des) (cdr (assoc (car arg-des) ent))) res-arg))
;;         (setq res-arg (cons (cons (car arg-des) '("")) res-arg))))
;;     (while (stringp (car (car ent)))
;;       (setq arg-des (pop ent))
;;       (if (and (not (assoc (car arg-des) res-arg)) (not (string= (car (cdr arg-des)) "")))
;;           (setq res-arg (cons (cons (car arg-des) (cdr arg-des)) res-arg))))
;;     (nreverse res-arg)))

;; (defun r-roxy-update-entry ()
;;   "Update the entry at the point or the entry above the function
;; which the point is in. Add a template empty roxygen documentation
;; if no roxygen entry is available. The template can be customized
;; via the variable `r-roxy-template-alist'. The parameter
;; descriptions can are filled if `r-roxy-fill-param-p' is
;; non-nil."
;;   (interactive)
;;   (save-excursion
;;     (let* ((args-fun (r-roxy-get-args-list-from-def))
;;            (args-ent (r-roxy-get-args-list-from-entry))
;;            (args (r-roxy-merge-args args-fun args-ent))
;;            (roxy-str (r-roxy-guess-prefix))
;;            (line-break "")
;;            here key template tag-def)
;;       (r-roxy-goto-func-def)
;;       (if (not (= (forward-line -1) 0))
;;           (progn
;;             (insert "\n")
;;             (forward-line -1)))
;;       (if (and (not (looking-at "^\n")) (not (r-roxy-entry-p)))
;;           (progn
;;             (end-of-line)
;;             (insert "\n")))
;;       (if (r-roxy-entry-p)
;;           (progn
;;             (setq here (1- (r-roxy-delete-args)))
;;             (r-roxy-insert-args args here))
;;         (setq template (copy-sequence r-roxy-template-alist))
;;         (while (stringp (car (car template)))
;;           (setq tag-def (pop template))
;;           (if (string= (car tag-def) "param")
;;               (r-roxy-insert-args args (point))
;;             (if (string= (car tag-def) "description")
;;                 (insert (concat line-break roxy-str " "
;;                                 (cdr tag-def) "\n" roxy-str))
;;               (if (string= (car tag-def) "details")
;;                   (insert (concat line-break roxy-str " " (cdr tag-def)))
;;                 (insert (concat line-break roxy-str " @"
;;                                 (car tag-def) " " (cdr tag-def))))
;;               ))
;;           (setq line-break "\n")
;;           )))))

;; (defun r-roxy-delete-args ()
;;   "remove all args from the entry at point or above the function
;; at point. Return 0 if no deletions were made other wise the point
;; at where the last deletion ended"
;;   (save-excursion
;;     (let* ((args nil)
;;            (cont t)
;;            (field-beg 0)
;;            entry-beg entry-end field-end)
;;       (r-roxy-goto-end-of-entry)
;;       (setq entry-beg (r-roxy-beg-of-entry))
;;       (setq entry-end (r-roxy-end-of-entry))
;;       (goto-char entry-end)
;;       (beginning-of-line)
;;       (while (and (<= entry-beg (point)) (> entry-beg 0) cont)
;;         (if (looking-at
;;              (concat r-roxy-prefix " *@param"))
;;             (progn
;;               (setq field-beg (r-roxy-beg-of-field))
;;               (setq field-end (r-roxy-end-of-field))
;;               (delete-region field-beg (+ field-end 1))))
;;         (setq cont nil)
;;         (if (= (forward-line -1) 0)
;;             (setq cont t)))
;;       field-beg)))

;; (defun r-roxy-get-args-list-from-entry ()
;;   "fill an args list from the entry above the function where the
;; point is"
;;   (save-excursion
;;     (let* (args entry-beg field-beg field-end args-text arg-name desc)
;;       (if (r-roxy-goto-end-of-entry)
;;           (progn
;;             (setq roxy-str (r-roxy-guess-prefix))
;;             (beginning-of-line)
;;             (setq entry-beg (r-roxy-beg-of-entry))
;;             (while (and (< entry-beg (point)) (> entry-beg 0))
;;               (if (looking-at
;;                    (concat r-roxy-prefix " *@param"))
;;                   (progn
;;                     (setq field-beg (r-roxy-beg-of-field))
;;                     (setq field-end (r-roxy-end-of-field))
;;                     (setq args-text (buffer-substring-no-properties
;;                                      field-beg field-end))
;;                     (setq args-text
;;                           (r-replace-in-string args-text roxy-str ""))
;;                     (setq args-text
;;                           (r-replace-in-string
;;                            args-text "[[:space:]]*@param *" ""))
;;                     ;; (setq args-text
;;                     ;;    (r-replace-in-string args-text "\n" ""))
;;                     (string-match "[^[:space:]]*" args-text)
;;                     (setq arg-name (match-string 0 args-text))
;;                     (setq desc (replace-regexp-in-string
;;                                 (concat "^" (regexp-quote arg-name) " *") "" args-text))
;;                     (setq args (cons (list (concat arg-name)
;;                                            (concat desc)) args))))
;;               (forward-line -1))
;;             args)
;;         nil))))

;; (defun r-roxy-toggle-roxy-region (beg end)
;;   "Remove prefix roxy string in this region if point is in a roxy
;; region, otherwise prefix all lines with the roxy
;; string. Convenient for editing example fields."
;;   (interactive "r")
;;   (unless (use-region-p)
;;     (error "region is not active"))
;;   (r-roxy-roxy-region beg end (r-roxy-entry-p)))

;; (defun r-roxy-roxy-region (beg end &optional on)
;;   (save-excursion
;;     (let (RE to-string
;;              (roxy-str (r-roxy-guess-prefix)))
;;       (narrow-to-region beg (- end 1))
;;       (if on
;;           (progn (setq RE (concat r-roxy-prefix " +?"))
;;                  (setq to-string ""))
;;         (setq RE "^")
;;         (setq to-string (concat roxy-str " ")))
;;       (goto-char beg)
;;       (while (re-search-forward RE (point-max) 'noerror)
;;         (replace-match to-string))
;;       (widen))))

;; (defun r-roxy-preview ()
;;   "Use a (possibly newly) connected R session and the roxygen package
;; `r-roxy-package' to generate the Rd code for entry at point, place it
;; in a temporary buffer and return that buffer."
;;   (let ((beg (r-roxy-beg-of-entry))
;;         (tmpf (make-temp-file "r-roxy"))
;;         (roxy-buf (get-buffer-create " *RoxygenPreview*"))
;;         (out-rd-roclet
;;          (cond ((string= "roxygen" r-roxy-package)
;;                 "make.Rd2.roclet()$parse")
;;                ;; must not line break strings to avoid getting +s in the output
;;                ((string= "roxygen2" r-roxy-package)
;;                 "(function(P) { if(compareVersion(paste(packageVersion('roxygen2')), '3.0.0') < 0) { ..results <- roxygen2:::roc_process(rd_roclet(), parse.files(P), \"\");cat(vapply(..results, FUN.VALUE=character(1), function(x) { roxygen2:::rd_out_cache$compute(x, format(x))})) } else {..results <- roc_proc_text(rd_roclet(), readChar(P, file.info(P)$size));cat(vapply(..results, format, FUN.VALUE = character(1))) } })")
;;                (t (error "need to hard code the roclet output call for roxygen package '%s'"
;;                          r-roxy-package))))
;;         )
;;     (if (= beg 0)
;;         (error "Point is not in a Roxygen entry"))
;;     (save-excursion
;;       (goto-char (r-roxy-end-of-entry))
;;       (forward-line 1)
;;       (if (r-end-of-function nil t)
;;           (append-to-file beg (point) tmpf)
;;         (while (and (forward-line 1) (not (looking-at "^$"))
;;                     (not (looking-at r-roxy-prefix))))
;;         (append-to-file beg (point) tmpf))
;;       (r-force-buffer-current)
;;       (r-command (concat "print(suppressWarnings(require(" r-roxy-package
;;                            ", quietly=TRUE)))\n") roxy-buf)
;;       (with-current-buffer roxy-buf
;;         (goto-char 1)
;;         (if (search-forward-regexp "FALSE" nil t)
;;             (error (concat "Failed to load the " r-roxy-package " package; "
;;                            "in R, try  install.packages(\"" r-roxy-package "\")"))))
;;       (r-command (concat out-rd-roclet "(\"" tmpf "\")\n") roxy-buf))
;;     (delete-file tmpf)
;;     roxy-buf))

;; (defun r-roxy-preview-HTML (&optional visit-instead-of-browse)
;;   "Use a (possibly newly) connected R session and the roxygen package to
;; generate a HTML page for the roxygen entry at point and open that
;; buffer in a browser.  Visit the HTML file instead of showing it in
;; a browser if `visit-instead-of-browse' is non-nil."
;;   (interactive "P")
;;   (let* ((roxy-buf (r-roxy-preview))
;;          (rd-tmp-file (make-temp-file "r-roxy-" nil ".Rd"))
;;          (html-tmp-file (make-temp-file "r-roxy-" nil ".html"))
;;          (rd-to-html (concat "Rd2HTML(\"" rd-tmp-file "\",\""
;;                              html-tmp-file "\", stages=c(\"render\"))"))
;;          )
;;     (with-current-buffer roxy-buf
;;       (set-visited-file-name rd-tmp-file)
;;       (save-buffer)
;;       (kill-buffer roxy-buf))
;;     (r-force-buffer-current)
;;     (r-command "print(suppressWarnings(require(tools, quietly=TRUE)))\n")
;;     (if visit-instead-of-browse
;;         (progn
;;           (r-command (concat rd-to-html "\n"))
;;           (find-file html-tmp-file))
;;       (r-command (concat "browseURL(" rd-to-html ")\n")))))

;; (defun r-roxy-preview-text ()
;;   "Use the connected R session and the roxygen package to
;; generate the text help page of the roxygen entry at point."
;;   (interactive)
;;   (with-current-buffer (r-roxy-preview)
;;     (Rd-preview-help)))

;; (defun r-roxy-preview-Rd (&optional name-file)
;;   "Use the connected R session and the roxygen package to
;; generate the Rd code for the roxygen entry at point. If called
;; with a non-nil `name-file' (e.g. universal argument C-u),
;; also set the visited file name of the created buffer to
;; facilitate saving that file."
;;   (interactive "P")
;;   (let ((roxy-buf (r-roxy-preview)))
;;     (pop-to-buffer roxy-buf)
;;     (if name-file
;;         (save-excursion
;;           (goto-char 1)
;;           (search-forward-regexp "name{\\(.+\\)}")
;;           (set-visited-file-name (concat (match-string 1) ".Rd"))))
;;     (Rd-mode)))

;; (defun r-roxy-hide-block ()
;;   "hide current roxygen comment block"
;;   (interactive)
;;   (save-excursion
;;     (let ((end-of-entry (r-roxy-end-of-entry))
;;           (beg-of-entry (r-roxy-beg-of-entry)))
;;       (hs-hide-block-at-point nil (list beg-of-entry end-of-entry)))))

;; (defun r-roxy-toggle-hiding ()
;;   "Toggle hiding/showing of a block.
;; See `hs-show-block' and `r-roxy-hide-block'."
;;   (interactive)
;;   (hs-life-goes-on
;;    (if (hs-overlay-at (point-at-eol))
;;        (hs-show-block)
;;      (r-roxy-hide-block))))

;; (defun r-roxy-show-all ()
;;   "Hide all Roxygen entries in current buffer. "
;;   (interactive)
;;   (r-roxy-hide-all t))

;; (defun r-roxy-hide-all (&optional show)
;;   "Hide all Roxygen entries in current buffer. "
;;   (interactive)
;;   (hs-life-goes-on
;;    (save-excursion
;;      (goto-char (point-min))
;;      (while (re-search-forward (concat r-roxy-prefix) (point-max) t 1)
;;        (let ((end-of-entry (r-roxy-end-of-entry)))
;;          (if show
;;              (hs-show-block)
;;            (r-roxy-hide-block))
;;          (goto-char end-of-entry)
;;          (forward-line 1))))))

;; (defun r-roxy-previous-entry ()
;;   "Go to beginning of previous Roxygen entry. "
;;   (interactive)
;;   (if (r-roxy-entry-p)
;;       (progn
;;         (goto-char (r-roxy-beg-of-entry))
;;         (forward-line -1)))
;;   (search-backward-regexp r-roxy-prefix (point-min) t 1)
;;   (goto-char (r-roxy-beg-of-entry)))

;; (defun r-roxy-next-entry ()
;;   "Go to beginning of next Roxygen entry. "
;;   (interactive)
;;   (if (r-roxy-entry-p)
;;       (progn
;;         (goto-char (r-roxy-end-of-entry))
;;         (forward-line 1)))
;;   (search-forward-regexp r-roxy-prefix (point-max) t 1)
;;   (goto-char (r-roxy-beg-of-entry)))

;; (defun r-roxy-complete-tag ()
;;   "complete the tag at point"
;;   (let ((token-string (thing-at-point 'symbol)))
;;     (when (and token-string (string-match "@.+" token-string))
;;       (comint-dynamic-simple-complete
;;        (replace-regexp-in-string "^@" "" token-string)
;;        (append r-roxy-tags-noparam r-roxy-tags-param)))))

;; (defun r-roxy-removegexp-roxy-re (string)
;;   "Remove the `r-roxy-str' before sending to R process. Useful
;;   for sending code from example section.  This function is placed
;;   in `r-presend-filter-functions'.
;;   "
;;   (if (r-roxy-entry-p)
;;       (replace-regexp-in-string r-roxy-prefix "" string)
;;     string))
;; (add-hook 'r-presend-filter-functions 'r-roxy-removegexp-roxy-re nil)


;; (defadvice r-eval-line-and-step (around r-eval-line-and-step-roxy)
;;   "evaluate line but do not skip over comment (roxy) lines"
;;   (if (r-roxy-entry-p)
;;       (let ((simple-next t))
;;         ad-do-it)
;;     ad-do-it))

;; (defadvice r-indent-command (around r-roxy-toggle-hiding)
;;   "hide this block if we are at the beginning of the line"
;;   (if (and (= (point) (point-at-bol)) (r-roxy-entry-p) 'r-roxy-hide-show-p)
;;       (progn (r-roxy-toggle-hiding))
;;     ad-do-it))

;; (defadvice fill-paragraph (around r-roxy-fill-advise)
;;   "Fill roxygen paragraphs."
;;   (cond
;;    ;; Regular case
;;    ((not (and (eq major-mode 'r-mode)
;;               (string= r-dialect "R")))
;;     ad-do-it)
;;    ;; Filling of code comments in @examples roxy field
;;    ((and (r-roxy-entry-p)
;;          (save-excursion
;;            (back-to-indentation)
;;            (looking-at "#")))
;;     (r-roxy-with-filling-context t
;;                                    ad-do-it))
;;    ((and (not (r-roxy-entry-p))
;;          (r-within-comment-p))
;;     ad-do-it)
;;    ;; Filling of call arguments with point on call name
;;    ((and r-fill-calls
;;          (r-within-call-name-p))
;;     (save-excursion
;;       (skip-chars-forward "^([")
;;       (forward-char)
;;       (r-fill-args)))
;;    ;; Filling of continuations
;;    ((and r-fill-continuations
;;          (r-within-continuation-p))
;;     (r-fill-continuations))
;;    ;; Filling of call arguments
;;    ((and r-fill-calls
;;          (r-within-call-p))
;;     (r-fill-args))
;;    ;; Filling of roxy blocks
;;    ((r-roxy-entry-p)
;;     (save-excursion
;;       (let* ((saved-pos (point))
;;              (saved-line (line-number-at-pos))
;;              (saved-col (current-column))
;;              (buffer (current-buffer))
;;              (par-start (save-excursion
;;                           (if (save-excursion
;;                                 (and (backward-paragraph)
;;                                      (forward-paragraph)
;;                                      (<= (point) saved-pos)))
;;                               (line-beginning-position)
;;                             (progn (backward-paragraph) (point)))))
;;              (par-end (r-roxy-find-par-end
;;                        (save-excursion
;;                          (forward-paragraph)
;;                          (point))
;;                        (concat r-roxy-prefix "[ \t]*@examples\\b") "^[^#]")))
;;         ;; Refill the whole structural paragraph sequentially, field by
;;         ;; field, stopping at @examples
;;         (r-roxy-with-filling-context nil
;;                                        (save-excursion
;;                                          (save-restriction
;;                                            (narrow-to-region par-start par-end)
;;                                            (goto-char 0)
;;                                            (while (< (point) (point-max))
;;                                              (r-roxy-maybe-indent-line)
;;                                              ad-do-it
;;                                              (forward-paragraph))))))))
;;    (t
;;     ad-do-it)))

;; (defadvice move-beginning-of-line (around r-roxy-beginning-of-line)
;;   "move to start"
;;   (if (r-roxy-entry-p)
;;       (let ((new-pos (save-excursion
;;                        (end-of-line)
;;                        (and (re-search-backward (concat r-roxy-prefix " ?") (point-at-bol) t)
;;                             (match-end 0)))))
;;         (if (or (bolp)
;;                 (< new-pos (point)))
;;             (goto-char new-pos)
;;           ad-do-it))
;;     ad-do-it))

;; (defadvice back-to-indentation (around r-roxy-back-to-indentation)
;;   "Handle back-to-indentation in roxygen doc"
;;   (if (r-roxy-entry-p)
;;       (progn
;;         (end-of-line)
;;         (re-search-backward (concat r-roxy-prefix " *") (point-at-bol) t)
;;         (goto-char (match-end 0)))
;;     ad-do-it))

;; (defun r-roxy-indent-new-comment-line ()
;;   (if (not (r-roxy-entry-p))
;;       (indent-new-comment-line)
;;     (r-roxy-indent-on-newline)))

;; (defun r-roxy-newline-and-indent ()
;;   (if (or (not (r-roxy-entry-p))
;;           (not r-roxy-insert-prefix-on-newline))
;;       (newline-and-indent)
;;     (r-roxy-indent-on-newline)))

;; (defun r-roxy-indent-on-newline ()
;;   "Insert a newline in a roxygen field."
;;   (cond
;;    ;; Point at beginning of first line of entry; do nothing
;;    ((= (point) (r-roxy-beg-of-entry))
;;     (newline-and-indent))
;;    ;; Otherwise: skip over roxy comment string if necessary and then
;;    ;; newline and then inset new roxy comment string
;;    (t
;;     (let ((point-after-roxy-string
;;            (save-excursion (forward-line 0)
;;                            (r-back-to-roxy)
;;                            (point))))
;;       (goto-char (max (point) point-after-roxy-string)))
;;     (newline-and-indent)
;;     (insert (concat (r-roxy-guess-prefix t) " ")))))


(provide 'r-roxy)

;;; r-roxy.el ends here
