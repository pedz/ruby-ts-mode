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

(defun ruby-ts--parent (&optional node)
  "Return parent of NODE.
NODE defaults to the node at point."
  (treesit-node-parent (or node (treesit-node-at (point)))))

(defun ruby-ts--next-sibling (cnt)
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

(defun ruby-ts--prev-sibling (cnt)
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

(defun ruby-ts--raw-next-sibling (cnt)
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

(defun ruby-ts--raw-prev-sibling (cnt)
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

(defun ruby-ts--next-sibling (regexp &optional point)
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

(defun ruby-ts--prev-sibling (regexp &optional point)
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

(defun ruby-ts--forward-argument-start (cnt)
  "Move point to the start of the next argument/parameter CNT times."
  (interactive "p")
  (setq cnt (or cnt 1))
  (let ((dest (ruby-ts--next-sibling
               (rx (or "method_parameters" "argument_list" "block_parameters")))))
    (while (and dest (> (setq cnt (1- cnt)) 0))
      (setq dest (treesit-node-next-sibling dest)))
    (if dest
        (goto-char (treesit-node-start dest))
      (error "Ran out of arguments / parameters"))))

(defun ruby-ts--forward-statement-start (cnt)
  "Move point to the start of the next argument/parameter CNT times."
  (interactive "p")
  (setq cnt (or cnt 1))
  (let ((dest (ruby-ts--next-sibling
               (rx (or "body_statement" "block_body" "then" "else" "do" "rescue"
                       "ensure")))))
    (while (and dest (> (setq cnt (1- cnt)) 0))
      (setq dest (treesit-node-next-sibling dest)))
    (if dest
        (goto-char (treesit-node-start dest))
      (error "Ran out of arguments / parameters"))))
           
;;; Start of wrapper routines, et. al.


(define-error 'ruby-ts--scan-error
              "A scan or movement was done that went too far"
              'treesit-error)

(defun ruby-ts--desired-item-p (node point forward)
  "Return non-nil if the NODE is the desired item.
The primitive routines take POINT and FORWARD as arguments.
POINT is where to start the search and FORWARD is non-nil if the
search direction is forward.  They all return a cons cell with
CAR set to the start of the item and CDR set to the end of the
item.

As described in the Point node in Info, point is between
characters.  Thus, if the item's start is less than POINT and the
item's end is greater than POINT, then the item contains POINT.

However if the start of the item is equal to POINT, then the item
actually starts after POINT.  If FORWARD is true, then this is the
desired item because moving forward, the item that was found will
be the next item.  But if FORWARD is nil indicating that the
movement is backwards, the item found is not the desired item since
moving backwards will select a different, previous item.

The converse is also true.  If the item's end is equal to POINT,
then the item is actually before POINT.  This is the desired item
if FORWARD is nil and movement is backwards but it is not the
desired item if movement is forward."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))
    (or (and (< start point) (> end point))
        (if forward
            (>= start point)
          (<= end point)))))

(defun ruby-ts--treesit-search-forward (start predicate &optional backward all)
  "Call `treesit-search-forward' and signals `ruby-ts--scan-error' on nil return.
See `treesit-search-forward' for description of START PREDICATE BACKWARD ALL."
  (let ((result (treesit-search-forward start predicate backward all)))
    (unless result
      (signal 'ruby-ts--scan-error "treesit search failed"))
    result))

(defun ruby-ts--wrapper-wrapper (arg interactive func)
  "A wrapper for the wrapper functions.
See `ruby-ts--mark-method' as an example of how ARG and INTERACTIVE
are interpreted.  FUNC is called with point and forward ARG times
with point progressing appropriately on each call.  The retun is
a list of four elements:

    0th - starting point
    1st - ending point
    2nd - `used-region-p'
    3rd - forward"
  (if interactive
      (condition-case e
          (ruby-ts--wrapper-wrapper arg nil func)
        (ruby-ts--scan-error
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

(defun ruby-ts--forward-wrapper (arg interactive func)
  "Wrapper function for moving forward.
ARG INTERACTIVE FUNC are passed to `ruby-ts--wrapper-wrapper' which
returns a list ( start end used-region-p forward ).  If forward
is true, point is set to end else start."
  (let* ((result (ruby-ts--wrapper-wrapper arg interactive func))
         (start (nth 0 result))
         (end (nth 1 result))
         (forward  (nth 3 result)))

    (when result
      (goto-char (if forward end start)))))

(defun ruby-ts--mark-wrapper (arg interactive func)
  "Wrapper function for marking a region.
ARG INTERACTIVE FUNC are passed to `ruby-ts--wrapper-wrapper' which
returns a list ( start end used-region-p forward ).  Generally
point is set to start and mark is set to end but not always.  e.g. if
used-region-p is true and forward is true, only mark is set."
  (let* ((result (ruby-ts--wrapper-wrapper arg interactive func))
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

(defconst ruby-ts--method-regexp
  (rx string-start (or "method" "singleton_method") string-end)
  "Regular expression for the node type that matches a method.")

(defun ruby-ts--method (point forward)
  "Return cons ( start of method . end of method ).
Starting at POINT and moving forward if FORWARD, else backwards, the
next or previous method is found.

Start of method includes the comments before method as well as
the white space from the beginning of the line.  End of method
includes any text from the end of the method to the start of the
next line."
  (let* ((start-node (treesit-node-at point))
         (method (ruby-ts--treesit-search-forward start-node
                                               ruby-ts--method-regexp
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

(defconst ruby-ts--statement-parent-regexp
  (rx string-start
      (or "program"
          "block_body"
          "begin_block"
          "end_block"
          "do"
          "else"
          "then"
          "ensure"
          "body_statement"
          "parenthesized_statements"
          "interpolation")
      string-end)
  "Regular expression of the nodes that can constain statements.")

(defun ruby-ts--is-comment (node)
  "Return t if NODE type is comment."
  (string-match-p "comment" (treesit-node-type node)))

(defun ruby-ts--statement (point forward)
  "Return cons ( start of statement . end of statement ).
Starting at POINT and moving forward if FORWARD, else backwards, the
next or previous statement is found.

If the statement starts the line, then it includes any comment lines
before the statement.  If there is only write space and comment from
the end of the statement to the end of the line then the end of
statement includes up to and including the new line."
  ;; treesit-search-forward can not be used because "statement" in the
  ;; grammer is actually "_statement" which means it is hidden.
  ;; Another problem is "_statement" includes "_expression" but
  ;; obviously a statement can be an expression but an expression
  ;; isn't always a statement.
  ;;
  ;; So the node at point is found.  Then the parents are found until
  ;; a node that can contain statements is found.  This node's type
  ;; will match ruby-ts--statement-parent-regexp.  The immediate child of
  ;; this node will be the statement.
  (let* ((statement (treesit-node-at point))
         (parent (treesit-node-parent statement)))
         ;; start sibling lineno temp)
    (message "point: %S; forward: %S" point forward)
    (message "1 parent: %S; statement: %S" parent statement)
    (while (and parent
                statement
                (not (string-match-p ruby-ts--statement-parent-regexp
                                     (treesit-node-type parent))))
      (setq statement parent
            parent (treesit-node-parent parent))
      (message "2 parent: %S; statement: %S" parent statement))
      

    ;; Move in proper direction over comments.
    (while (and statement
                (or (ruby-ts--is-comment statement)
                    (not (ruby-ts--desired-item-p statement point forward))))
      (setq statement (if forward
                          (treesit-node-next-sibling statement)
                        (treesit-node-prev-sibling statement)))
      (message "3 parent: %S; statement: %S" parent statement))


    ;; Error off if we are off in space.
    (unless statement
      (signal 'ruby-ts--scan-error "statement not found"))
    (cons (treesit-node-start statement) (treesit-node-end statement))))

    ;; (setq lineno (ruby-ts--lineno statement)
    ;;       sibling (treesit-node-prev-sibling statement))
    ;; (if (ruby-ts--is-comment sibling)
    ;;     (progn
    ;;       (while (and (setq temp (treesit-node-prev-sibling sibling))
    ;;                   (ruby-ts--is-comment temp))
    ;;         (setq sibling temp))
    ;;       (setq start (treesit-node-start sibling)))
    ;;   ;;;;  start here tomorrow...
    ;;   (if (= lineno (ruby-ts--lineno sibling))
    ;;       (setq)))))

    
(defun ruby-ts--mark-method (&optional arg interactive)
  "Put mark at end of this method, point at beginning.
The method marked is the one that contains point or follows point.
With positive ARG, mark this and that many next methods; with negative
ARG, change the direction of marking.

If the mark is active, it marks the next or previous method(s) after
the one(s) already marked.

If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (ruby-ts--mark-wrapper arg interactive #'ruby-ts--method))

(defun ruby-ts--mark-statement (&optional arg interactive)
  "Put mark at end of this statement, point at beginning.
The statement marked is the one that contains point or follows point.
With positive ARG, mark this and that many next statements; with negative
ARG, change the direction of marking.

If the mark is active, it marks the next or previous statement(s) after
the one(s) already marked.

If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (ruby-ts--mark-wrapper arg interactive #'ruby-ts--statement))

(defun ruby-ts--forward-method (&optional arg interactive)
  "Move point forward ARG methods.
Negative ARG moves backwards.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (ruby-ts--forward-wrapper arg interactive #'ruby-ts--method))

(provide 'ruby-ts-navigation)

;;; ruby-ts-navigation.el ends here
