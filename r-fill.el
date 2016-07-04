;;; r-fill.el --- Indentation engine for r-mode
;; 
;; Copyright (C) 2016 Lionel Henry, Vitalie Spinu
;; 
;; Author: Lionel Henry <lionel.hry@gmail.com>,
;;         Vitalie Spinu <spinuvit@gmail.com>
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

(require 'r-indent)
(require 'r-roxy)

(defgroup r-fill nil
  "r-mode indentation."
  :group 'r
  :prefix "r-fill-")

(defcustom r-fill-calls t
  "If non-nil, refilling a paragraph inside a function or
indexing call will arrange the arguments according to
`fill-column' as in:

  fun_call(argument1, argument2,
           argument3, argument4)


Refilling repeatedly cycles through different styles and
eventually to the original formatting.

 The second style formats according to one argument per line:

  fun_call(argument1,
           argument2,
           argument3,
           argument4)

When `r-fill-calls-newlines' is t, the second style becomes:

  fun_call(
      argument1,
      argument2,
      argument3,
      argument4
  )

Setting `r-offset-arguments' to `prev-line' or `prev-call'
activates a third style. It keeps one argument per line except
for the first N arguments. N is controlled with a prefix. For
example, calling M-q three times sets N to 1 while calling M-q
twice then C-U 2 M-q sets N to 2. Here what the default produces:

  fun_call(argument1,
      argument2,
      argument3,
      argument4,
      argument5
  )

This style is useful for refilling R6 or ggproto class
definitions."
  :group 'r-fill
  :type 'boolean)

(defcustom r-fill-continuations t
  "If non-nil, refilling a paragraph inside a continuation of
statements (expressions separated by operators) will arrange all
its elements, never going past `fill-column'.

  lm(outcome ~ pred1 + pred2 +
       pred3 + pred4, data)

Refilling repeatedly cycles through different styles and
eventually to the original formatting.

The second style lay out the statements according to one
expression per line:

  lm(outcome ~
       pred1 +
       pred2 +
       pred3 +
       pred4, data)"
  :group 'r-fill
  :type 'boolean)

(defcustom r-fill-calls-newlines nil
  "When non-nil, the second refilling style produces newlines
after and before the opening and closing delimiters. This is
intended for example for dplyr-style code:

  fun_call(
      argument1,
      argument2,
      argument3,
      argument4
  )

Note that this setting is temporary and likely to be replaced in
the next ESS version by a more comprehensive and flexible way to
set refill styles."
  :group 'r-fill
  :type 'boolean)

(defun r--fallback-fill-paragraph (&optional justify)
  (let ((fill-paragraph-function nil))
    (fill-paragraph justify)))

;; Unroll arguments to a single line until closing marker is found.
(defun r-fill--unroll-lines (bounds &optional jump-cont)
  (let* ((last-pos (point-min))
         (containing-sexp (r-containing-sexp-position))
         prefix-break)
    (goto-char (car bounds))
    (goto-char (r-line-end-position))
    (while (and (/= (point) last-pos)
                (< (line-end-position)
                   (cadr bounds))
                (not prefix-break))
      (setq last-pos (point))
      ;; Check whether we ended up in a sub call. In this case, jump
      ;; over it, otherwise, join lines.
      (let ((contained-sexp (r-containing-sexp-position)))
        (cond ((and contained-sexp
                    containing-sexp
                    (not (= containing-sexp contained-sexp)))
               (goto-char (1+ contained-sexp))
               (r-up-list))
              ;; Jump over continued statements
              ((and jump-cont (r-ahead-operator-p 'strict))
               (r-climb-token)
               (r-jump-continuations))
              ;; Jump over comments
              ((looking-at "#")
               (forward-line)
               (r-indent-line))
              (t
               (join-line 1))))
      (goto-char (r-line-end-position)))
    (goto-char (car bounds))))

(defvar r-fill--orig-pos nil
  "Original position of cursor.")

(defvar r-fill--orig-state nil
  "Backup of original code to cycle back to original state.")

(defvar r-fill--second-state nil
  "Backup of code produce by very first cycling. If this is equal
  to orig-state, no need to cycle back to original state.")

(defvar r-fill--style-level nil
  "Filling style used in last cycle.")

(defun r-fill--substring (bounds)
  (buffer-substring (car bounds) (cadr bounds)))

(defun r-fill-args--roll-lines (&optional style)
  (let (infinite prefix-break last-pos last-newline start-pos)
    (while (and (not (looking-at "[])]"))
                (/= (point) (or last-pos 1))
                (not infinite))
      (setq prefix-break nil)
      ;; Record start-pos as future breaking point to avoid breaking
      ;; at `=' sign
      (while (looking-at "[ \t]*[\n#]")
        (forward-line)
        (r-back-to-indentation))
      (setq start-pos (point))
      (while (and (< (current-column) fill-column)
                  (not (looking-at "[])]"))
                  (/= (point) (or last-pos 1))
                  ;; Break after one pass if prefix is active
                  (not prefix-break))
        (when (memq style '(2 3))
          (setq prefix-break t))
        (r-jump-token ",")
        (setq last-pos (point))
        ;; Jump expression and any continuations. Reindent all lines
        ;; that were jumped over
        (let ((cur-line (line-number-at-pos))
              end-line)
          (cond ((r-jump-arg)
                 (setq last-newline nil))
                ((r-token-after= ",")
                 (setq last-newline nil)
                 (setq last-pos (1- (point)))))
          (save-excursion
            (when (< cur-line (line-number-at-pos))
              (setq end-line (line-number-at-pos))
              (r-goto-line (1+ cur-line))
              (while (and (<= (line-number-at-pos) end-line)
                          (/= (point) (point-max)))
                (r-indent-line)
                (forward-line))))))
      (when (or (>= (current-column) fill-column)
                prefix-break
                ;; Ensures closing delim on a newline
                (and (= style 4)
                     (looking-at "[ \t]*[])]")
                     (setq last-pos (point))))
        (if (and last-pos (/= last-pos start-pos))
            (goto-char last-pos)
          (r-jump-char ","))
        (cond ((looking-at "[ \t]*[#\n]")
               (forward-line)
               (r-indent-line)
               (setq last-newline nil))
              ;; With levels 2 and 3, closing delim goes on a newline
              ((looking-at "[ \t]*[])]")
               (when (and (memq style '(2 3 4))
                          r-fill-calls-newlines
                          (not last-newline))
                 (newline-and-indent)
                 ;; Prevent indenting infinitely
                 (setq last-newline t)))
              ((not last-newline)
               (newline-and-indent)
               (setq last-newline t))
              (t
               (setq infinite t)))))))

(defun r-fill-args (&optional style)
  (let ((bounds (r-args-bounds 'marker))
        ;; Set undo boundaries manually
        (undo-inhibit-record-point t))
    (when (not bounds)
      (error "Could not find function bounds"))
    (setq style (or style (r-fill-style 'calls bounds)))
    (if (= style 0)
        (progn
          (delete-region (car bounds) (marker-position (cadr bounds)))
          (insert r-fill--orig-state)
          ;; Restore the point manually. (save-excursion) wouldn't
          ;; work here because we delete the text rather than just
          ;; modifying it.
          (goto-char r-fill--orig-pos)
          (message "Back to original formatting"))
      (r-blink-region (nth 2 bounds)
                      (1+ (marker-position (cadr bounds))))
      (undo-boundary)
      (save-excursion
        (r-fill--unroll-lines bounds t)
        (cond
         ;; Some styles start with first argument on a newline
         ((and (memq style '(2 4))
               r-fill-calls-newlines
               (not (looking-at "[ \t]*#")))
          (newline-and-indent))
         ;; Third level, start a newline after N arguments
         ((and (= style 3)
               (not (looking-at "[ \t]*#")))
          (let ((i (if (numberp current-prefix-arg)
                       current-prefix-arg
                     1)))
            (while (and (> i 0)
                        (r-jump-arg)
                        (r-jump-char ","))
              (setq i (1- i))))
          (newline-and-indent)))
        (r-fill-args--roll-lines style)
        ;; Reindent surrounding context
        (r-indent-call (car bounds)))
      ;; Signal marker for garbage collection
      (set-marker (cadr bounds) nil)
      (undo-boundary))))

;; Detect repeated commands
(defun r-fill-style (type bounds)
  (let ((max-level
         ;; This part will be simpler once we have the style alist
         (cond ((eq type 'calls)
                ;; No third style either when r-offset-arguments is
                ;; set to 'open-delim, or when r-fill-calls-newlines
                ;; is nil and no numeric prefix is given
                (if (and (not (eq (r-offset-type 'arguments)
                                  'open-delim))
                         (or r-fill-calls-newlines
                             (numberp current-prefix-arg)))
                    3
                  2))
               ((eq type 'continuations)
                2))))
    (if (not (memq last-command '(fill-paragraph-or-region
                                  fill-paragraph)))
        (progn
          ;; Record original state on first cycling
          (setq r-fill--orig-state (r-fill--substring bounds))
          (setq r-fill--orig-pos (point))
          (setq r-fill--second-state nil)
          (setq r-fill--style-level 1))
      ;; Also record state on second cycling
      (when (and (= r-fill--style-level 1)
                 (null r-fill--second-state))
        (setq r-fill--second-state (r-fill--substring bounds)))
      (cond ((>= r-fill--style-level max-level)
             (let ((same-last-and-orig (string= (r-fill--substring bounds)
                                                r-fill--orig-state))
                   (same-2nd-and-orig (string= r-fill--orig-state
                                               r-fill--second-state)))
               ;; Avoid cycling to the same state twice
               (cond ((and same-last-and-orig
                           same-2nd-and-orig)
                      (setq r-fill--style-level 2))
                     ((or same-last-and-orig
                          same-2nd-and-orig)
                      (setq r-fill--style-level 1))
                     (t
                      (setq r-fill--style-level 0)))))
            (r-fill--style-level
             (setq r-fill--style-level (1+ r-fill--style-level))))))
  r-fill--style-level)

(defun r-fill-continuations (&optional style)
  (let ((bounds (r-continuations-bounds 'marker))
        (undo-inhibit-record-point t)
        (last-pos (point-min))
        last-newline infinite)
    (when (not bounds)
      (error "Could not find statements bounds"))
    (setq style (or style (r-fill-style 'continuations bounds)))
    (if (= style 0)
        (progn
          (delete-region (car bounds) (marker-position (cadr bounds)))
          (insert r-fill--orig-state)
          (goto-char r-fill--orig-pos)
          (message "Back to original formatting"))
      (r-blink-region (car bounds) (marker-position (cadr bounds)))
      (undo-boundary)
      (save-excursion
        (r-fill--unroll-lines bounds)
        (while (and (< (point) (cadr bounds))
                    (/= (point) (or last-pos 1))
                    (not infinite))
          (setq last-pos (point))
          (when (and (r-jump-expression)
                     (indent-according-to-mode)
                     (not (> (current-column) fill-column)))
            (setq last-newline nil))
          (r-jump-operator)
          (if (or (and (> (current-column) fill-column)
                       (goto-char last-pos))
                  (= style 2))
              (progn
                (r-jump-operator)
                (unless (= (point) (cadr bounds))
                  (when last-newline
                    (setq infinite t))
                  (newline-and-indent)
                  (setq last-newline t)))
            (setq last-newline nil)))
        (r-indent-call (car bounds)))
      (set-marker (cadr bounds) nil)
      (undo-boundary))))

(defun r-fill-paragraph (&optional justify)
  "Fill paragraph in R mode."
  (cond
   ;; 1. Code comments in @examples roxy field
   ((and (r-roxy-entry-p)
         (save-excursion
           (back-to-indentation)
           (looking-at "#")))
    (r-roxy-with-filling-context t
      (r--fallback-fill-paragraph justify)))
   ;; 2. Plain comments
   ((and (not (r-roxy-entry-p))
         (r-within-comment-p))
    (r--fallback-fill-paragraph))
   ;; 3. Call arguments with point inside the call name
   ((and r-fill-calls
         (r-within-call-name-p))
    (save-excursion
      (skip-chars-forward "^([")
      (forward-char)
      (r-fill-args)))
   ;; 4. Continuations
   ((and r-fill-continuations
         (r-within-continuation-p))
    (ess-fill-continuations))
   ;; 5. Call arguments
   ((and r-fill-calls
         (r-within-call-p))
    (r-fill-args))
   ;; 5. Roxy blocks
   ((ess-roxy-entry-p)
    (save-excursion
      (let* ((saved-pos (point))
             (par-start (save-excursion
                          (if (save-excursion
                                (and (backward-paragraph)
                                     (forward-paragraph)
                                     (<= (point) saved-pos)))
                              (line-beginning-position)
                            (progn (backward-paragraph) (point)))))
             (par-end (r-roxy-find-par-end
                       (save-excursion
                         (forward-paragraph)
                         (point))
                       (concat ess-roxy-re "[ \t]*@examples\\b") "^[^#]")))
        ;; Refill the whole structural paragraph sequentially, field by
        ;; field, stopping at @examples
        (r-roxy-with-filling-context nil
          (save-excursion
            (save-restriction
              (narrow-to-region par-start par-end)
              (goto-char 0)
              (while (< (point) (point-max))
                (r-roxy-maybe-indent-line)
                (r--fallback-fill-paragraph)
                (forward-paragraph))))))))
   (t
    (r--fallback-fill-paragraph justify)))
  ;; need to return something to inhibit further processing from fill-paragraph
  t)

(provide 'r-fill)
;; r-fill.el ends here

