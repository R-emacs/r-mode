;;; r-utils.el --- Various utilities for R code -*- lexical-binding: t; -*-
;;
;; Copyright © 2016 Vitalie Spinu, Lionel Henry
;; Authors: Lionel Henry <lionel.hry@gmail.com>,
;;          Vitalie Spinu <spinuvit@gmail.com>
;; Created: 4 Jul 2016
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
;;; Code:

(defvar r-symbol-regexp "\\(\\sw\\|\\s_\\)"
  "The regular expression for matching an R symbol")

(defun r-blink-region (start end)
  (when ess-blink-region
    (move-overlay ess-current-region-overlay start end)
    (run-with-timer ess-blink-delay nil
                    (lambda ()
                      (delete-overlay ess-current-region-overlay)))))


;;*;; Position Info

(defun r-looking-at (regex &optional newlines)
  "Like `looking-at' but consumes blanks and comments first."
  (save-excursion
    (r-skip-blanks-forward newlines)
    (looking-at regex)))

(defun r-line-end-position ()
  "Like (line-end-position) but stops at comments"
  (save-excursion
    (or (and (re-search-forward "#" (line-end-position) t)
             (match-beginning 0))
        (line-end-position))))

(defun r-containing-sexp-position ()
  "Return position of the containing sexp."
  (cadr (syntax-ppss)))

(provide 'r-utils)
;;; r-utils.el ends here
