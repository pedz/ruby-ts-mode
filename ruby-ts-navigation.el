;;; ruby-ts-navigation.el --- navigation tree-sitter support for Ruby  -*- lexical-binding: t; -*-

;; This is currently a work in progress.  My intent is to release it
;; with whatever copyright notice Free Software Foundation,
;; Inc. wants.

;; Author     : Perry Smith <pedz@easesoftware.com>
;; Created    : December 2022
;; Keywords   : ruby languages tree-sitter

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Attempt to write navigation code that makes sense.  If the cursor
;; is in the middle of a parameter list, <forward> (however that is
;; triggered from the keyboard) should move to the start of the next
;; parameter.  Some people like moving to the end of things (which I
;; totally don't get but... forward-thing can return a Tree Sitter
;; node and then the final piece can put the cursor at the start or
;; end of the node.

;;; Code:

(require 'ruby-ts-mode)

(defun rtsn-parent (&optional node)
  "Return parent of NODE.
NODE defaults to the node at point."
  (treesit-node-parent (or node (treesit-node-at (point)))))

(defun rtsn-next-sibling (cnt)
  "Use `treesit-node-next-sibling' CNT times.
Temporary routine used for experimentation."
  (interactive "p")
  (setq cnt (or cnt 1))
  (let ((node (treesit-node-at (point))))
    (while (and node (> (setq cnt (1- cnt)) 0))
      (setq node (treesit-node-next-sibling node)))
    (if node
        (goto-char (treesit-node-start node))
      (message "no node.  cnt is still %d" cnt))))

(defun rtsn-prev-sibling (cnt)
  "Use `treesit-node-prev-sibling' CNT times.
Temporary routine used for experimentation."
  (interactive "p")
  (setq cnt (or cnt 1))
  (let ((node (treesit-node-at (point))))
    (while (and node (> (setq cnt (1- cnt)) 0))
      (setq node (treesit-node-prev-sibling node)))
    (if node
        (goto-char (treesit-node-start node))
      (message "no node.  cnt is still %d" cnt))))

(defun rtsn--raw-next-sibling (cnt)
  "Find the CNTth child of parent node at `point' starting after `point'."
  (interactive "p")
  (let* ((point (point))
         (node (treesit-node-at (point)))
         (parent (treesit-node-parent (treesit-node-parent node)))
         (children (treesit-node-children parent nil))
         (next-children (seq-filter (lambda (node)
                                      (> (treesit-node-start node) point))
                                    children))
         (child (nth (1- cnt) next-children)))
    (message "node is %s; parent is %s; %d children"
             (treesit-node-type node)
             (treesit-node-type parent)
             (length children))
    (if child
        (goto-char (treesit-node-start child))
      (message "ran out of children"))))

(defun rtsn--raw-prev-sibling (cnt)
  "Find the CNTth child of parent node at `point' ending before `point'."
  (interactive "p")
  (let* ((point (point))
         (node (treesit-node-at (point)))
         (parent (treesit-node-parent (treesit-node-parent node)))
         (children (treesit-node-children parent nil))
         (prev-children (seq-filter (lambda (node)
                                      (< (treesit-node-end node) point))
                                    children))
         (child (nth (1- cnt) prev-children)))
    (message "node is %s; parent is %s; %d children"
             (treesit-node-type node)
             (treesit-node-type parent)
             (length children))
    (if child
        (goto-char (treesit-node-end child))
      (message "ran out of children"))))

(defun rtsn--next-sibling (regexp &optional point)
  "Return next sibling.
Find the closest ancestor node matching REGEXP starting from the
node at POINT.  Return the first direct child of ancestor
starting after POINT or nil if there is no such sibling.
Optional POINT defaults to `point'."
  (setq point (or point (point)))
  (let* ((ancestor (treesit-parent-until (treesit-node-at point) (treesit-type-pred regexp)))
         (children (treesit-node-children ancestor t)))
    (seq-find (lambda (node)
                (> (treesit-node-start node) point))
              children)))

(defun rtsn--prev-sibling (regexp &optional point)
  "Return previous sibling.
Find the closest ancestor node matching REGEXP starting from the node
at POINT.  Return the last direct child of ancestor ending before
POINT or nil if there is no such sibling.  Optional POINT defaults
to `point'."
  (setq point (or point (point)))
  (let* ((ancestor (treesit-parent-until (treesit-node-at point) (treesit-type-pred regexp)))
         (children (reverse (treesit-node-children ancestor t))))
    (seq-find (lambda (node)
                (< (treesit-node-end node) point))
              children)))

(defun rtsn-forward-argument-start (cnt)
  "Move point to the start of the next argument/parameter CNT times."
  (interactive "p")
  (setq cnt (or cnt 1))
  (let ((dest (rtsn--next-sibling
               (rx (or "method_parameters" "argument_list" "block_parameters")))))
    (while (and dest (> (setq cnt (1- cnt)) 0))
      (setq dest (treesit-node-next-sibling dest)))
    (if dest
        (goto-char (treesit-node-start dest))
      (error "Ran out of arguments / parameters"))))

(defun rtsn-forward-statement-start (cnt)
  "Move point to the start of the next argument/parameter CNT times."
  (interactive "p")
  (setq cnt (or cnt 1))
  (let ((dest (rtsn--next-sibling
               (rx (or "body_statement" "block_body" "then" "else" "do" "rescue"
                       "ensure")))))
    (while (and dest (> (setq cnt (1- cnt)) 0))
      (setq dest (treesit-node-next-sibling dest)))
    (if dest
        (goto-char (treesit-node-start dest))
      (error "Ran out of arguments / parameters"))))
           
;;; Start of wrapper routines, et. al.

(define-error 'rtsn-scan-error
              "A scan or movement was done that went too far"
              'treesit-error)

(defun rtsn--treesit-search-forward (start predicate &optional backward all)
  "Call `treesit-search-forward' and signals `rtsn-scan-error' on nil return.
See `treesit-search-forward' for description of START PREDICATE BACKWARD ALL."
  (let ((result (treesit-search-forward start predicate backward all)))
    (unless result
      (signal 'rtsn-scan-error "treesit search failed"))
    result))

(defun rtsn--wrapper-wrapper (arg interactive func)
  "A wrapper for the wrapper functions.
See `rtsn-mark-method' as an example of how ARG and INTERACTIVE
are interpreted.  FUNC is called with point and forward ARG times
with point progressing appropriately on each call.  The retun is
a list of four elements:

    0th - starting point
    1st - ending point
    2nd - `used-region-p'
    3rd - forward"
  (if interactive
      (condition-case e
          (rtsn--wrapper-wrapper arg nil func)
        (rtsn-scan-error
         (message "e: %S" (cdr e))
         (user-error (cdr e))))

    ;; I'm lame... I discovered that use-region-p changes state if
    ;; mark is set and that scared me to just caching up the original
    ;; value and using it everywhere.
    (let* ((used-region-p (use-region-p))
           (back-command (intern (concat (symbol-name this-command) "-back")))
           (forward (and (>= arg 0) (not (eq last-command back-command))))
           (repeating (or (eq last-command this-command)
                          (eq last-command back-command)))
           (start (or (and repeating (use-region-beginning)) (point)))
           (end (or  (and repeating (use-region-end)) (point)))
           (point (if forward end start))
           (first-pair (funcall func point forward))
           (pair first-pair))

      ;; Get repeating the same command backwards to work right.
      (unless forward
        (setq this-command back-command))

      ;; Repeat call to func arg times moving poiint properly each time.
      (setq arg (abs arg))
      (while (and pair (> (setq arg (1- arg)) 0))
        (setq point (if forward (cdr pair) (car pair))
              pair (funcall func point forward)))
      
      (list (min (car first-pair) (car pair))
            (max (cdr first-pair) (cdr pair))
            used-region-p
            forward))))

(defun rtsn--forward-wrapper (arg interactive func)
  "Wrapper function for moving forward.
ARG INTERACTIVE FUNC are passed to `rtsn--wrapper-wrapper' which
returns a list ( start end used-region-p forward ).  If forward
is true, point is set to end else start."
  (let* ((result (rtsn--wrapper-wrapper arg interactive func))
         (start (nth 0 result))
         (end (nth 1 result))
         (forward  (nth 3 result)))

    (when result
      (goto-char (if forward end start)))))

(defun rtsn--mark-wrapper (arg interactive func)
  "Wrapper function for marking a region.
ARG INTERACTIVE FUNC are passed to `rtsn--wrapper-wrapper' which
returns a list ( start end used-region-p forward ).  Generally
point is set to start and mark is set to end but not always.  e.g. if
used-region-p is true and forward is true, only mark is set."
  (let* ((result (rtsn--wrapper-wrapper arg interactive func))
         (start (nth 0 result))
         (end (nth 1 result))
         (used-region-p (nth 2 result))
         (forward  (nth 3 result)))

    (when result
      ;;
      ;; Important NOTE! this routine does NOT move to the start of
      ;; the current line or the next line.  That is the job of func
      ;; if appropriate.
      (unless (and used-region-p (not forward))
        (save-excursion
          (goto-char end)
          (set-mark (point))))
      (unless (and used-region-p forward)
        (goto-char start)))))

(defconst rtsn--method-regexp
  (rx string-start (or "method" "singleton_method") string-end)
  "Regular expression for the node type that matches a method.")

(defun rtsn--method (point forward)
  "Return cons ( start of method . end of method ).
Start of method includes the comments before method as well as
the white space from the beginning of the line.  End of method
includes any text from the end of the method to the start of the
next line.

If POINT is within a method, that method's start and end point is
returned.  Otherwise the next method's start and end points are
returned when FORWARD is non-nil and the previous method's start
and end points are returned when FORWARD is nil."
  (let* ((start-node (treesit-node-at point))
         (method (rtsn--treesit-search-forward start-node
                                               rtsn--method-regexp
                                               (not forward)))
         (comment method)
         temp start end)
    (while (and (setq temp (treesit-node-prev-sibling comment))
                (string-match-p "comment" (treesit-node-type temp)))
      (setq comment temp))
    (save-excursion
      (goto-char (treesit-node-start comment))
      (forward-line 0)
      (setq start (point))
      (goto-char (treesit-node-end method))
      (forward-line 1)
      (setq end (point)))
    (cons start end)))

(defun rtsn-mark-method (&optional arg interactive)
  "Put mark at end of this method, point at beginning.
The method marked is the one that contains point or follows point.
With positive ARG, mark this and that many next methods; with negative
ARG, change the direction of marking.

If the mark is active, it marks the next or previous method(s) after
the one(s) already marked.

If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (rtsn--mark-wrapper arg interactive #'rtsn--method))

(defun rtsn-forward-method (&optional arg interactive)
  "Move point forward ARG methods.
Negative ARG moves backwards.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (rtsn--forward-wrapper arg interactive #'rtsn--method))

(provide 'ruby-ts-navigation)

;;; ruby-ts-navigation.el ends here
