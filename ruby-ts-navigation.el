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
  (if interactive
      (condition-case e
          (rtsn-mark-method arg nil)
        (scan-error (user-error (cadr e))))
    (let* ((use-region-p (use-region-p))
           (forward (and (>= arg 0) (not (eq last-command 'rtsn-mark-method-back))))
           ;; we are repeating this command
           (repeat (or (eq last-command 'rtsn-mark-method)
                       (eq last-command 'rtsn-mark-method-back)))
           (start (or (and repeat (use-region-beginning)) (point)))
           (end (or  (and repeat (use-region-end)) (point)))
           (regexp (regexp-opt '("class" "module")))
           (method (treesit-node-at (if forward end start)))
           (body-statement (treesit-node-parent method))
           (class-module (treesit-node-parent body-statement))
           comment first-method temp)
      (setq arg (abs arg))
      (unless forward
        (setq this-command 'rtsn-mark-method-back))

      ;; if class-module is nil, it means we are not within a class or
      ;; module so we use the highest node and find its siblings.
      ;; Otherwise, we move all three up a level until class-module is
      ;; "class" or "module" or becomes nil.
      (while (and class-module (not (string-match-p regexp (treesit-node-type class-module))))
        (setq method body-statement
              body-statement class-module
              class-module (treesit-node-parent class-module)))

      ;; if we quit with class-module set, then method a child of a
      ;; body_statement.  Otherwise we are not within a class / module
      ;; so we pick the highest level node.
      (unless class-module
        (setq method (or body-statement method)))
      (setq first-method method)

      ;; We now move forward or backward "arg" siblings but don't count
      ;; "comment" nodes.  If mark is active, we also need to make sure
      ;; that we don't count methods that are already within the region
      ;; which can happen on the first hit.
      (while (and method (> arg 0))
        (if (and (not (string-match-p "comment" (treesit-node-type method)))
                 (if forward
                     (> (treesit-node-end method) end)
                   (< (treesit-node-start method) start)))
            (setq arg (1- arg)))
        (if (> arg 0)
            (setq method (if forward
                             (treesit-node-next-sibling method)
                           (treesit-node-prev-sibling method)))))

      (when method
        ;; move backwards over comments (to include them in the results)
        ;;
        ;; There are four situations.  Moving forward or backward
        ;; multiplied by starting from point or starting from the active
        ;; region.  In all but one case, we need to search backwards
        ;; (always backwards) to include the comments associated with
        ;; the method.  Thus we have:
        ;;
        ;; forward  use-region-p add comments
        ;;    true            true           no
        ;;    true           false          yes
        ;;   false            true          yes
        ;;   false           false          yes
        ;;
        (setq comment (if forward first-method method))
        (unless (and forward use-region-p)
          (while (and (setq temp (treesit-node-prev-sibling comment))
                      (string-match-p "comment" (treesit-node-type temp)))
            (setq comment temp)))

        ;; SIGH!!!  Now we figure out how to set point and mark.  If
        ;; use-region-p is false, we set point to the node start of
        ;; comment and mark to the node end of method.  If
        ;; use-region-p is true, we set mark to the node end of method
        ;; if we are moving forward and point to the node start of
        ;; comment if we are moving backwards.
        ;; Note that setting mark changes (use-region-p) which is why
        ;; its original value is cached up
        (unless (and use-region-p (not forward))
          (save-excursion
            (goto-char (treesit-node-end method))
            (forward-line 1)
            (set-mark (point))))
        (unless (and use-region-p forward)
          (goto-char (treesit-node-start comment))
          (forward-line 0))))))

(provide 'ruby-ts-navigation)

;;; ruby-ts-navigation.el ends here
