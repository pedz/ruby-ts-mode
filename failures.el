;;;
;;; Four routines that I don't think are going to do what I want.
;;;

(defun rtsm--forward-end (node &optional cnt)
  "Move forward to the end of matching regexp NODE.
If already at the end, move to the next sibling and repeat the search.
Repeat CNT times."
  (interactive "snode (regexp): \np")
  (setq cnt (or cnt 1))
  (let* ((start (treesit-node-at (point)))
         (found (treesit-search-forward start node nil))
         (pos (treesit-node-end found)))
    ;; If we have moved in the correct direction, it counts as 1
    (if (and pos (> pos (point)))
        (setq cnt (1- cnt)))
    (while (and found (> cnt 0))
      (setq start (treesit-node-next-sibling found)
            found (treesit-search-forward start node nil)
            cnt (1- cnt)))
    (if found
        (goto-char (treesit-node-end found))
      (error "Movement to %s failed" node))))

(defun rtsm--forward-start (node &optional cnt)
  "Move forward to the start of matching regexp NODE.
If already at the start, move to the next sibling and repeat the search.
Repeat CNT times."
  (interactive "snode (regexp): \np")
  (setq cnt (or cnt 1))
  (let* ((start (treesit-node-at (point)))
         (found (treesit-search-forward start node nil))
         (pos (treesit-node-start found)))
    ;; If we have moved in the correct direction, it counts as 1
    (if (and pos (> pos (point)))
        (setq cnt (1- cnt)))
    (while (and found (> cnt 0))
      (setq start (treesit-node-next-sibling found)
            found (treesit-search-forward start node nil)
            cnt (1- cnt)))
    (if found
        (goto-char (treesit-node-start found))
      (error "Movement to %s failed" node))))

(defun rtsm--backward-end (node &optional cnt)
  "Move backward to the end of matching regexp NODE.
If already at the end, move to the next sibling and repeat the search.
Repeat CNT times."
  (interactive "snode (regexp): \np")
  (setq cnt (or cnt 1))
  (let* ((start (treesit-node-at (point)))
         (found (treesit-search-forward start node t))
         (pos (treesit-node-end found)))
    ;; If we have moved in the correct direction, it counts as 1
    (if (and pos (< pos (point)))
        (setq cnt (1- cnt)))
    (while (and found (> cnt 0))
      (setq start (treesit-node-next-sibling found)
            found (treesit-search-forward start node t)
            cnt (1- cnt)))
    (if found
        (goto-char (treesit-node-end found))
      (error "Movement to %s failed" node))))

(defun rtsm--backward-start (node &optional cnt)
  "Move backward to the start of matching regexp NODE.
If already at the start, move to the next sibling and repeat the search.
Repeat CNT times."
  (interactive "snode (regexp): \np")
  (setq cnt (or cnt 1))
  (let* ((start (treesit-node-at (point)))
         (found (treesit-search-forward start node t))
         (pos (treesit-node-start found)))
    ;; If we have moved in the correct direction, it counts as 1
    (if (and pos (< pos (point)))
        (setq cnt (1- cnt)))
    (while (and found (> cnt 0))
      (setq start (treesit-node-next-sibling found)
            found (treesit-search-forward start node t)
            cnt (1- cnt)))
    (if found
        (goto-char (treesit-node-start found))
      (error "Movement to %s failed" node))))

;;;
;;;  This routine never got finished.  I thought I could create the
;;;  four previous routines semi-automagically.
;;;

(defun rtsm--arrow (dir &optional end)
  "Return a function that takes ARG which move in DIR direction.

DIR can be:
  'up    -- move up or out to the start of the parent node.
  'down  -- move down or into node to the start of the first child.
  'left  -- move left or backward to the start of the previous sibling.
  'right -- move right or forward to the start of the next sibling.

When moving right, if there is no next sibling, move to the start
of the first child of the praent's next sibling.  Recurse if
necessary.

Likewise, when moving left, if there is no previous sibling, move
to the start of the last child of the parent's previous sibling.
Recurse if necessary.

When moving down or in, if the current position has no children,
the function falls back to what 'right would do.

Optional END, when non-nil, makes all movements to the end rather
than the start.

ARG argument of the returned function uses the prefix arg as a
repeat count."
  (lambda (arg)
    (interactive "p")
    (setq arg (or arg 1))
    (let* ((pnt (point))
           (start (treesit-node-at pnt))
           found))))

;;;
;;;  This is another set of routines, etc that isn't working out as
;;;  desired.  The comments in rtsm--arrow-forward-start and
;;;  rtsm--arrow-forward-end should provide insight into my thoughts.
;;;

(rx-define rtsm--peer-nonlocal-variable-rx
  (| "instance_variable" "class_variable" "global_variable"))
(defconst rtsm--peer-nonlocal-variable (rx rtsm--peer-nonlocal-variable-rx)
  "Peer choices for nonlocal_variable.")

(rx-define rtsm--peer-variable-rx
  (| "constant" "identifier" "self" "super" rtsm--peer-nonlocal-variable-rx))
(defconst rtsm--peer-variable (rx rtsm--peer-variable-rx)
  "Peer choices for lhs.")

(rx-define rtsm--peer-lhs-rx
  (| "call" "element_reference" "false" "nil" "scope_resolution" "true" rtsm--peer-variable-rx))
(defconst rtsm--peer-lhs (rx rtsm--peer-lhs-rx)
  "Peer choices for lhs.")

(rx-define rtsm--peer-simple-numeric-rx
  (| "integer" "float" "complex" "rational"))
(defconst rtsm--peer-simple-numeric (rx rtsm--peer-simple-numeric-rx)
  "Peer choices for simple numeric.")

(rx-define rtsm--peer-numeric-rx
  (| "unary" rtsm--peer-simple-numeric-rx))
(defconst rtsm--peer-numeric (rx rtsm--peer-numeric-rx)
  "Peer choices for numeric.")

(rx-define rtsm--peer-literal-rx
  (| "simple_symbol" "delimited_symbol" rtsm--peer-numeric-rx))
(defconst rtsm--peer-literal (rx rtsm--peer-literal-rx)
  "Peer choices for literal.")

(rx-define rtsm--peer-primary-rx
  (| "array" "begin" "break" "call" "case" "case_match"
     "chained_string" "character" "class" "for" "hash"
     "heredoc_beginning" "if" "lambda" "method" "module" "next"
     "parenthesized_statements" "redo" "regex" "retry" "return"
     "singleton_class" "singleton_method" "string" "string_array"
     "subshell" "symbol_array" "unary" "unless" "until" "while"
     "yield" rtsm--peer-lhs-rx rtsm--peer-literal-rx))
(defconst rtsm--peer-primary (rx rtsm--peer-primary-rx)
  "Peer choices for primary.")

(rx-define rtsm--peer-arg-rx
  (| "assignment" "binary" "conditional" "operator_assignment" "range"
     "unary" rtsm--peer-primary-rx))
(defconst rtsm--peer-arg (rx rtsm--peer-arg-rx)
  "Peer choides for arg.")

(rx-define rtsm--peer-expression-rx
  (| "assignment" "binary" "break" "call" "next" "operator_assignment"
     "return" "unary" "yield" rtsm--peer-arg-rx))
(defconst rtsm--peer-expression (rx rtsm--peer-expression-rx)
  "Peer choices for expression.")

(rx-define rtsm--peer-statement-rx
  (| "undef" "alias" "if_modifier" "unless_modifier" "while_modifier"
     "until_modifier" "rescue_modifier" "begin_block" "end_block"
     rtsm--peer-expression-rx))
(defconst rtsm--peer-statement (rx rtsm--peer-statement-rx)
  "Peer choices for  statement.")

(rx-define rtsm--peer-formal-parameter-rx
  (| "block_parameter" "destructured_parameter" "forward_parameter"
     "hash_splat_nil" "hash_splat_parameter" "identifier"
     "keyword_parameter" "optional_parameter" "splat_parameter"))
(defconst rtsm--peer-formal-parameter (rx rtsm--peer-formal-parameter-rx)
  "Peer choices for formal parameters.")

(rx-define rtsm--peer-keyword-variable-rx
  (| "nil" "self" "true" "false" "line" "file" "encoding"))
(defconst rtsm--peer-keyword-variable (rx rtsm--peer-keyword-variable-rx)
  "Peer choices for keyword variable.")

(rx-define rtsm--peer-pattern-literal-rx
  (| "heredoc_beginning" "regex" "string" "string_array" "subshell"
     "symbol_array" rtsm--peer-keyword-variable-rx rtsm--peer-literal-rx))
(defconst rtsm--peer-pattern-literal (rx rtsm--peer-pattern-literal-rx)
  "Peer choices for pattern literal.")

(rx-define rtsm--peer-pattern-primitive-rx
  (| "lambda" rtsm--peer-pattern-literal-rx))
(defconst rtsm--peer-pattern-primitive (rx rtsm--peer-pattern-primitive-rx)
  "Peer choices for pattern primitive.")

(rx-define rtsm--peer-pattern-constant-rx
  (| "constant" "scope_resolution"))
(defconst rtsm--peer-pattern-constant (rx rtsm--peer-pattern-constant-rx)
  "Peer choices for pattern constant.")

(rx-define rtsm--peer-pattern-value-rx
  (| "range" "variable_reference_pattern"
     "expression_reference_pattern" rtsm--peer-pattern-primitive-rx
     rtsm--peer-pattern-constant-rx))
(defconst rtsm--peer-pattern-value (rx rtsm--peer-pattern-value-rx)
  "Peer choices for pattern value.")

(rx-define rtsm--peer-pattern-expr-basic-rx
  (| "identifier" "array_pattern" "find_pattern" "hash_pattern"
     "parenthesized_pattern" rtsm--peer-pattern-value-rx))
(defconst rtsm--peer-pattern-expr-basic (rx rtsm--peer-pattern-expr-basic-rx)
  "Peer choices for pattern expr basic.")

(rx-define rtsm--peer-pattern-expr-alt-rx
  (| "alternative_pattern" rtsm--peer-pattern-expr-basic-rx))
(defconst rtsm--peer-pattern-expr-alt (rx rtsm--peer-pattern-expr-alt-rx)
  "Peer choices for pattern expr alt.")

(rx-define rtsm--peer-pattern-expr-rx
  (| "as_pattern" rtsm--peer-pattern-expr-alt-rx))
(defconst rtsm--peer-pattern-expr (rx rtsm--peer-pattern-expr-rx)
  "Peer choices for pattern expr.")

(rx-define rtsm--peer-pattern-top-expr-body-rx
  (| "array_pattern" "find_pattern" "hash_pattern" rtsm--peer-pattern-expr-rx))
(defconst rtsm--peer-pattern-top-expr-body (rx rtsm--peer-pattern-top-expr-body-rx)
  "Peer choices for pattern top expr body.")

(rx-define rtsm--peer-if-then-else-rx
  (| "if" "then" "else" "elsif"))
(defconst rtsm--peer-if-then-else (rx rtsm--peer-if-then-else-rx)
  "Peer choices for if then else.")

(rx-define rtsm--peer-body-rescue-etc-rx
  (| "begin" "rescue" "ensure" "else"))
(defconst rtsm--peer-body-rescue-etc (rx rtsm--peer-body-rescue-etc-rx)
  "Peer choices for body rescue etc.")

(rx-define rtsm--peer-argument-rx
  (| "splat_argument" "hash_splat_argument" "forward_argument"
     "block_argument" "pair" rtsm--peer-expression-rx))
(defconst rtsm--peer-argument (rx rtsm--peer-argument-rx)
  "Peer choices for argument.")

(defconst rtsm--peer-groups
  (list "argument_list" "bare_parameters" "block" "block_body"
        "block_parameters" "body_statement" "break_command"
        "command_argument_list" "command_assignment" "command_binary"
        "command_call" "command_call_with_block"
        "command_operator_assignment" "command_unary" "comment"
        "constant_suffix" "destructured_left_assignment" "do"
        "do_block" "empty_statement" "escape_sequence"
        "exception_variable" "exceptions" "externals" "extras"
        "heredoc_body" "identifier_suffix" "if_guard" "in" "in_clause"
        "inline" "interpolation" "keyword_pattern"
        "left_assignment_list" "name" "next_command" "operator"
        "parameters" "parenthesized_unary" "pattern" "program"
        "rescue_modifier_arg" "rescue_modifier_expression"
        "rest_assignment" "return_command" "right_assignment_list"
        "rules" "setter" "superclass" "supertypes" "unary_literal"
        "uninterpreted" "unless_guard" "when" "word" "yield_command"
        rtsm--peer-statement rtsm--peer-argument rtsm--peer-expression
        rtsm--peer-arg rtsm--peer-primary
        rtsm--peer-pattern-top-expr-body rtsm--peer-pattern-expr
        rtsm--peer-pattern-expr-alt rtsm--peer-pattern-expr-basic
        rtsm--peer-pattern-value rtsm--peer-pattern-primitive
        rtsm--peer-pattern-literal rtsm--peer-lhs
        rtsm--peer-formal-parameter rtsm--peer-variable
        rtsm--peer-literal rtsm--peer-keyword-variable
        rtsm--peer-numeric rtsm--peer-nonlocal-variable
        rtsm--peer-body-rescue-etc rtsm--peer-simple-numeric
        rtsm--peer-pattern-constant rtsm--peer-if-then-else)
  "Lexical peer group regular expressions.")

(defun rtsm--find-peer-regexp (list node-type)
  "Return the matching regexp NODE-TYPE from LIST."
  (if list
      (let ((regexp (concat "\\`\\(?:" (car list) "\\)\\'")))
        (if (string-match-p regexp node-type)
            regexp
          (rtsm--find-peer-regexp (cdr list) node-type)))))

(defconst rtsm--highest-at-exclusiom-list
  (list "body_statement")
  "These constructs are too far up the tree.")

(defun rtsm--highest-at-start (node pos)
  "Find highest ancestor of NODE starting at POS."
  (let* ((parent (treesit-node-parent node))
         (start (treesit-node-start parent)))
    (if (and (= start pos)
             (not (member (treesit-node-type parent) rtsm--highest-at-exclusiom-list))
             (message "node: %S; parent: %S" node parent))
        (rtsm--highest-at-start parent pos)
      node)))

(defun rtsm--highest-at-end (node pos)
  "Find highest ancestor of NODE ending at POS."
  (let* ((parent (treesit-node-parent node))
         (end (treesit-node-end parent)))
    (if (and (= end pos)
             (not (member (treesit-node-type parent) rtsm--highest-at-exclusiom-list))
             (message "node: %S; parent: %S" node parent))
        (rtsm--highest-at-end parent pos)
      node)))

(defun rtsm--arrow-forward-start (arg)
  "Move to the start ARG peer nodes forward.
ARG is the prefix argument.  Negative ARG isn't supported (yet).

Find leaf node at point (node)
If the leaf node at point starts before point:
  This counts as a negative movement and ARG is incremented.
  Find type of node (node-type)
Else
  node-type is nil

If node-type is nil
  Find highest node starting at point (ancestor)
  The type of ancstor is found: (node-type)

`rtsm--peer-groups' is searched until a regexp matches
  node-type: (peer-regexp)

Starting with dest equal to ancestor, `treesit-search-forward' is
  called with peer-regexp ARG times moving dest up each time.
  If the result is nil, the loop ends and the most recent dest
  is used.

goto start of dest."
  (interactive "p")
  (setq arg (or arg 1))
  ;; I assume that treesit-node-at will always return a node.  Thus
  ;; start will never be nil
  (let* ((pos (point))
         (node (treesit-node-at pos))
         (start (treesit-node-start node))
         (node-type nil)
         peer-regexp temp-node)
    (when (/= pos start)
      (setq arg (1+ arg)
            node-type (treesit-node-type node)))
    (unless node-type
      (setq node (rtsm--highest-at-start node start)
            node-type (treesit-node-type node)))
    (setq peer-regexp
          (or (rtsm--find-peer-regexp rtsm--peer-groups node-type) "."))
    (message "node-type: %s; peer-regexp: %s" node-type peer-regexp)
    (while (> arg 0)
      (if (setq temp-node (treesit-search-forward node peer-regexp))
          (progn
            (message "node: %S; temp-node: %S" node temp-node)
            (setq node temp-node
                  arg (1- arg)))
        (setq arg 0)))
    (setq start (treesit-node-start node))
    (goto-char start)))

(defun rtsm--arrow-forward-end (arg)
  "Move to the end ARG peer nodes forward.
ARG is the prefix argument.  Negative ARG isn't supported (yet).

Find leaf node at point (node)
If the leaf node at point ends after point:
  This counts as one movement and ARG is decremented.
  Find type of node (node-type)
Else
  node-type is nil

If ARG is 0
  goto end of node
Else
  If node-type is nil
    Find highest node ending at point (ancestor)
    The type of ancstor is found: (node-type)

  `rtsm--peer-groups' is searched until a regexp matches
    node-type: (peer-regexp)

  Starting with dest equal to node, `treesit-search-forward' is
    called with peer-regexp ARG times moving dest up each time.
    If the result is nil, the loop ends and the most recent dest
    is used.

  goto end of dest."
  (interactive "p")
  (setq arg (or arg 1))
  ;; I assume that treesit-node-at will always return a node.  Thus
  ;; end will never be nil
  (let* ((pos (point))
         (node (treesit-node-at pos))
         (end (treesit-node-end node))
         (node-type nil)
         peer-regexp temp-node)
    (when (/= pos end)
      (setq arg (1- arg)
            node-type (treesit-node-type node)))
    (if (< arg 1)
        (goto-char end)
      (unless node-type
        (setq node (rtsm--highest-at-end node end)
              node-type (treesit-node-type node)))
      (setq peer-regexp
            (or (rtsm--find-peer-regexp rtsm--peer-groups node-type) "."))
      (message "node-type: %s; peer-regexp: %s" node-type peer-regexp)
      (while (> arg 0)
        (if (setq temp-node (treesit-search-forward node peer-regexp))
            (progn
              (message "node: %S; temp-node: %S" node temp-node)
              (setq node temp-node
                    arg (1- arg)))
          (setq arg 0)))
      (setq end (treesit-node-end node)))
    (goto-char end)))

;; Changed this to using wrapper routines...
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
    (let* ((used-region-p (use-region-p))
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
        ;; forward  used-region-p add comments
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
        ;; used-region-p is false, we set point to the node start of
        ;; comment and mark to the node end of method.  If
        ;; used-region-p is true, we set mark to the node end of method
        ;; if we are moving forward and point to the node start of
        ;; comment if we are moving backwards.
        ;; Note that setting mark changes (use-region-p) which is why
        ;; its original value is cached up
        (unless (and used-region-p (not forward))
          (save-excursion
            (goto-char (treesit-node-end method))
            (forward-line 1)
            (set-mark (point))))
        (unless (and used-region-p forward)
          (goto-char (treesit-node-start comment))
          (forward-line 0))))))


;; This does not use treesit-search-forward.  I'm going to write one
;; that does and compare...
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
  (let* ((class-regexp (regexp-opt '("class" "module")))
         (method-regexp (regexp-opt '("method" "singleton_method")))
         (method (treesit-node-at point))
         (body-statement (treesit-node-parent method))
         (class-module (treesit-node-parent body-statement))
         comment temp start end)
    
    ;; if class-module is nil, it means we are not within a class or
    ;; module so we use the highest node and find its siblings.
    ;; Otherwise, we move all three up a level until class-module is
    ;; "class" or "module" or becomes nil.
    (while (and class-module
                (not (string-match-p class-regexp (treesit-node-type class-module))))
      (setq method body-statement
            body-statement class-module
            class-module (treesit-node-parent class-module)))

    ;; if the method node spans the point and matches method-regexp we
    ;; are done.  Otherwise, we start moving in direction specified by
    ;; "siblings" of method for the next / prev sibling that matches
    ;; method-regexp and is entirely before or after POINT.
    (unless (or (not method)
                (and (>= (treesit-node-end method) point)
                     (<= (treesit-node-start method) point)
                     (string-match-p method-regexp (treesit-node-type method))))
      (if forward
          (while (and method
                      (not (and (> (treesit-node-start method) point)
                                (string-match-p method-regexp (treesit-node-type method)))))
            (setq method (treesit-node-next-sibling method)))
        (while (and method
                    (not (and (< (treesit-node-end method) point)
                              (string-match-p method-regexp (treesit-node-type method)))))
          (setq method (treesit-node-prev-sibling method)))))

    ;; Now we move backwards to include any preceeding comments
    ;; describing the method.
    (when method
      (setq comment method)
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
      (cons start end))))

