;;; word-stats.el --- Word stats tool

;; Copyright (C) 2016-2017 Launay Gaby

;; Author: Launay Gaby <gaby.launay@tutanota.com>
;; Maintainer: Launay Gaby <gaby.launay@tutanota.com>
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1.0
;; Keywords: word, stats
;; URL: http://github.com/galaunay/word-stats.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Word stats is a small tool to display word stats in a buffer or a region of a
;; buffer

;; Basic usage

;; `word-stats` will display a buffer with statistics on the current buffer word
;; usage.

;;; Code:

(require 'subr-x)
(require 'org)

(defgroup word-stats nil
  "Word stats tool."
  :prefix "word-stats-"
  :group 'tools)

(defcustom word-stats-ignored-symbols "[.,'&+-=#[\"|<>)\\(^\/*%${}]+"
  "Regexp matching symbol to be ignored."
  :type 'regexp
  :group 'word-stats)

(defcustom word-stats-minimum-word-length 3
  "Ignore word wth length smaller than this."
  :type 'string
  :group 'word-stats)

(defcustom word-stats-ignored-words '("the" "to" "and" "i" "a" "of" "in" "on"
                                      "0" "be" "my" "-" "as" "it" "by" "for"
                                      "that" "am" "also" "this" "an"
                                      "at" "is")
  "List of words that will be ignored."
  :type '(repeat string)
  :group 'word-stats)

(defun word-stats--count-raw-word-list (raw-word-list)
  "Count word occurence in RAW-WORD-LIST."
  (cl-loop with result = nil
           for elt in raw-word-list
           do (cl-incf (cdr (or (assoc elt result)
                                (car (push (cons elt 0) result)))))
           finally return (sort result
                                (lambda (a b) (string< (car a) (car b))))))

(defun word-stats ()
  "Show the current buffer or region word stats."
  (interactive)
  (let* ((text (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (buffer-string)))
         ;; remove ignored symbols
         (clean-text (replace-regexp-in-string
                      word-stats-ignored-symbols
                      " " text))
         ;; remove commented lines
         (clean-text (string-join (cl-remove-if
                                   (lambda (elt)
                                     (or (string-match
                                          (format "^[ \t]*%s.*%s$"
                                                  comment-start
                                                  comment-end)
                                          elt)
                                         (when
                                             (string= major-mode 'latex-mode)
                                           (string-match "^\\\\.*$" elt))))
                                   (split-string clean-text "\n")) "\n"))
         ;; split to words
         (words (split-string (downcase clean-text) "[ \f\t\n\r\v]+" t))
         ;; remove ignored words
         (raw-word-list (cl-remove-if
                         (lambda (elt) (member elt word-stats-ignored-words))
                         words))
         ;; remove small words
         (raw-word-list (cl-remove-if
                         (lambda (elt)
                           (< (length elt) word-stats-minimum-word-length))
                         words))
         ;; count occurences
         (word-list (word-stats--count-raw-word-list raw-word-list)))
    (with-current-buffer (get-buffer-create "*word-statistics*")
      (erase-buffer)
      (insert "| word | occurences |
               | <50> | <20>       |
               |-----------+------------|\n")

      (dolist (elt word-list)
        (insert (format "| '%s' | %d |\n" (car elt) (cdr elt))))

      (org-mode)
      (indent-region (point-min) (point-max))
      (goto-char 0)
      (org-cycle) (org-cycle) (org-cycle) (org-cycle) (org-cycle) (org-cycle)
      (org-table-sort-lines nil ?N)))
  (pop-to-buffer "*word-statistics*"))


(provide 'word-stats)
;;; word-stats.el ends here
