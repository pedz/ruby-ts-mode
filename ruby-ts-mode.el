;;; ruby-ts-mode.el --- tree-sitter support for Ruby  -*- lexical-binding: t; -*-

;;; This is currently a work in progress.  My intent is to release it
;;; with whatever copyright notice Free Software Foundation,
;;; Inc. wants.

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
;;

;;; Code:

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

(defcustom ruby-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `ruby-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'ruby)

(defcustom ruby-ts-mode-indent-style 'base
  "Style used for indentation.

Currently can only be set to BASE.  If one of the supplied styles
doesn't suffice a function could be set instead.  This function
is expected return a list that follows the form of
`treesit-simple-indent-rules'."
  :version "29.1"
  :type '(choice (symbol :tag "Base" 'base)
                 (function :tag "A function for user customized style" ignore))
  :group 'ruby)

(defcustom ruby-ts-mode-right-justify-arrays t
  "Right justify elements in an array.

e.g.             or
  array = [            array = [   145,
       145,                      21110,
     21110,                         11]
        11
    ]

verses           or

  array = [            array = [145,
    145,                 21110,
    21110,               11]
    11
    ]"
  :type 'boolean
  :group 'ruby)

(defcustom ruby-ts-mode-indent-split-exp-by-term t
  "Indent expressions split across lines as `ruby-mode' did.

If set to true, long expressions that are split across lines will be
indented like `enh-ruby-mode' would indent the lines which is similar
to `c-mode'.  Thus:

with_paren = (a + b *
              c * d +
              12)

without_paren = a + b *
  c * d +
  12

If set to nil, long expressions are indented based upon the expression
parsed hierarchy which is similar to how `ruby-mode' indented.  Thus:

with_paren = (a + b *
                  c * d +
              12)

without_paren = a + b *
                    c * d +
                12"
  :type 'boolean
  :group 'ruby)

(defface ruby-ts-mode--constant-assignment-face
  '((((class grayscale) (background light)) :foreground "DimGray" :slant italic)
    (((class grayscale) (background dark))  :foreground "LightGray" :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "VioletRed4")
    (((class color) (min-colors 88) (background dark))  :foreground "plum2")
    (((class color) (min-colors 16) (background light)) :foreground "RosyBrown")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 8)) :foreground "green")
    (t :slant italic))
  "Font Lock face used in `ruby-ts-mode' to highlight assignments to constants."
  :group 'font-lock-faces)

(defface ruby-ts-mode--assignment-face
  '((((class grayscale) (background light)) :foreground "DimGray" :slant italic)
    (((class grayscale) (background dark))  :foreground "LightGray" :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "VioletRed4")
    (((class color) (min-colors 88) (background dark))  :foreground "coral1")
    (((class color) (min-colors 16) (background light)) :foreground "RosyBrown")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 8)) :foreground "green")
    (t :slant italic))
  "Font Lock face used in `ruby-ts-mode' to hightlight assignments."
  :group 'font-lock-faces)

(defvar ruby-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Mostly stolen from ruby-mode but enh-ruby-mode also added ??
    (modify-syntax-entry ?#  "<"  table)
    (modify-syntax-entry ?$  "'"  table)
    (modify-syntax-entry ?%  "."  table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?:  "'"  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ??  "_"  table)
    (modify-syntax-entry ?@  "'"  table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\; "."  table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?\n ">"  table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?_  "_"  table)
    (modify-syntax-entry ?|  "."  table)
    table)
  "Syntax table used by ‘ruby-ts-mode’ buffers.")

(defvar ruby-ts-mode--operators-arithmetic
  '("+" "-" "*" "/" "%" "**")
  "Ruby arithmetic operators for tree-sitter font-locking.")

;; treesit-query-validate doesn't like these:
;; "eql?" "equal?"
(defvar ruby-ts-mode--operators-comparison
  '("==" "!=" ">" "<" ">=" "<=" "<=>" "===")
  "Ruby comparison operators for tree-sitter font-locking.")

(defvar ruby-ts-mode--operators-assignment
  '("=" "+=" "-=" "*=" "/=" "%=" "**=")
  "Ruby assignment operators for tree-sitter font-locking.")

(defvar ruby-ts-mode--operators-bitwise
  '("&" "|" "^" "~" "<<" ">>")
  "Ruby bitwise operators for tree-sitter font-locking.")

(defvar ruby-ts-mode--operators-logical
  '("!" "&&" "and" "not" "or" "||")
  "Ruby logical operators for tree-sitter font-locking.")

(defvar ruby-ts-mode--operators-ternary
  '("?" ":")
  "Ruby ternary operators for tree-sitter font-locking.")

(defvar ruby-ts-mode--operators-range
  '(".." "...")
  "Ruby range operators for tree-sitter font-locking.")

(defvar ruby-ts-mode--operators-defined
  '("defined?")
  "Ruby defined? operators for tree-sitter font-locking.")

(defvar ruby-ts-mode--operators-dot-colon
  '("." "::")
  "Ruby dot and double colon operators for tree-sitter font-locking.")

(defvar ruby-ts-mode--operators
  (append ruby-ts-mode--operators-arithmetic
          ruby-ts-mode--operators-comparison
          ruby-ts-mode--operators-assignment
          ruby-ts-mode--operators-bitwise
          ruby-ts-mode--operators-logical
          ruby-ts-mode--operators-ternary
          ruby-ts-mode--operators-range
          ruby-ts-mode--operators-defined
          ruby-ts-mode--operators-dot-colon)
  "Ruby operators for tree-sitter font-locking.")

;; doc/keywords.rdoc in the Ruby git repository considers these to be
;; reserved keywords.  If these keywords are added to the list, it
;; causes the font-lock to stop working.
;;
;; "__ENCODING__" "__FILE__" "__LINE__" "false" "self" "super" "true"
;;
;; "nil" (which does not exhibit this issue) is also considered a
;; keyword but I removed it and added it as a constant.
;;
(defun ruby-ts-mode--keywords (language)
  "Ruby keywords for tree-sitter font-locking.
Currently LANGUAGE is ignored but shoule be set to `ruby'."
  (let ((common-keywords
         '("BEGIN" "END" "alias" "and" "begin" "break" "case" "class"
           "def" "defined?" "do" "else" "elsif" "end" "ensure" "for"
           "if" "in" "module" "next" "not" "or" "redo" "rescue"
           "retry" "return" "then" "undef" "unless" "until" "when"
           "while" "yield")))
    common-keywords))

;; Ideas of what could be added:
;;   1. The regular expressions start, end, and content could be font
;;      locked.  Ditto for the command strings `foo`.  The symbols
;;      inside a %s, %i, and %I could be given the "symbol" font.
;;      etc.
(defun ruby-ts-mode--font-lock-settings (language)
  "Tree-sitter font-lock settings.
Currently LANGUAGE is ignored but should be set to `ruby'."
  (treesit-font-lock-rules
   :language language
   :feature 'comment
   `((comment) @font-lock-comment-face
     (comment) @contextual)

   :language language
   :feature 'keyword
   `([,@(ruby-ts-mode--keywords language)] @font-lock-keyword-face)

   :language language
   :feature 'constant
   `((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (nil) @font-lock-constant-face
     (self) @font-lock-constant-face
     (super)  @font-lock-constant-face)

   ;; Before 'operator so (unary) works.  (I didn't want to try
   ;; :override)
   :language language
   :feature 'literal
   `((unary ["+" "-"] [(integer) (rational) (float) (complex)]) @font-lock-number-face
     (simple_symbol) @font-lock-number-face
     (delimited_symbol) @font-lock-number-face
     (integer) @font-lock-number-face
     (float) @font-lock-number-face
     (complex) @font-lock-number-face
     (rational) @font-lock-number-face)

   :language language
   :feature 'operator
   `("!" @font-lock-negation-char-face
     [,@ruby-ts-mode--operators] @font-lock-operator-face)

   :language language
   :feature 'string
   `((string) @font-lock-string-face
     (string_content) @font-lock-string-face)

   :language language
   :feature 'type
   `((constant) @font-lock-type-face)

   :language language
   :feature 'assignment
   '((assignment
      left: (identifier) @ruby-ts-mode--assignment-face)
     (assignment
      left: (left_assignment_list (identifier) @ruby-ts-mode--assignment-face))
     (operator_assignment
      left: (identifier) @ruby-ts-mode--assignment-face))

   ;; Constant and scoped constant assignment (declaration)
   ;; Must be enabled explicitly
   :language language
   :feature 'constant-assignment
   :override t
   `((assignment
      left: (constant) @ruby-ts-mode--constant-assignment-face)
     (assignment
      left: (scope_resolution name: (constant) @ruby-ts-mode--constant-assignment-face)))

   :language language
   :feature 'function
   '((call
      method: (identifier) @font-lock-function-name-face)
     (method
      name: (identifier) @font-lock-function-name-face))

   :language language
   :feature 'variable
   '((identifier) @font-lock-variable-name-face)

   :language language
   :feature 'error
   '((ERROR) @font-lock-warning-face)

   :feature 'escape-sequence
   :language language
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language language
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)
   )
  )

(defun treesit-type-pred (regexp)
  "Return predicate taking a node returning non-nil if REGEXP matches type of node."
  (lambda (node)
    (string-match-p regexp (treesit-node-type node))))

(defun parent-node (&rest _)
  "Return the parent node matching ident rule."
  (lambda (_n parent &rest _)
    parent))

(defun bol (pred)
  "Return bol of PRED.
PRED should take (node parent bol &rest rest) and return a node"
  (lambda (node parent bol &rest rest)
    (save-excursion
      (goto-char (treesit-node-start (funcall pred node parent bol rest)))
      (back-to-indentation)
      (point))))

(defun ancestor-start (type)
  "Return start of closest ancestor matching regexp TYPE."
  (lambda (node &rest _)
    (treesit-node-start (treesit-parent-until node (treesit-type-pred type)))))

(defun ancestor-is (type)
  "Check that ancestor's type matches regexp TYPE."
  (lambda (node &rest _)
    (treesit-parent-until node (treesit-type-pred type))))

(defalias 'ancestor-node #'ancestor-is
  "Return ancestor node whose type matches regexp TYPE.")

(defun ruby-ts-mode--right-justify-array-leaf ( node parent &rest _)
  "Right justify leaf NODE within PARENT array."
  (let* ((children (treesit-node-children parent t))
         (open-bracket (nth 0 (treesit-node-children parent nil)))
         (first-child (nth 0 children))
         (same-line (equal (line-number-at-pos (treesit-node-start open-bracket))
                           (line-number-at-pos (treesit-node-start first-child))))
         (max-length (apply #'max (mapcar (lambda ( child )
                                            (- (treesit-node-end child) (treesit-node-start child)))
                                          children)))
         (node-length (- (treesit-node-end node) (treesit-node-start node)))
         (grand-parent-bol (save-excursion
                             (goto-char (treesit-node-start (treesit-node-parent parent)))
                             (back-to-indentation)
                             (point)))
         (align-column (if same-line
                           (- (+ (treesit-node-end open-bracket) max-length 1) ruby-ts-mode-indent-offset)
                         (+ grand-parent-bol max-length 1))))

    (- align-column node-length)))

(defun ruby-ts-mode--indent-styles (_language)
  "Indent rules supported by `ruby-ts-mode'.
Currently LANGUAGE is ignored but should be set to `ruby'"
  (let ((common
         `(
           ;; Slam all top level nodes to the left margin
           ((parent-is "program") parent 0)

           ((node-is ")") parent 0)
           ((node-is "end") grand-parent 0)

           ,@(if ruby-ts-mode-right-justify-arrays
                 `(((query "(array \"[\" ( (integer) ( \",\" (_) )*) @indent \",\"? \"]\")")
                    ruby-ts-mode--right-justify-array-leaf ruby-ts-mode-indent-offset)
                   ((n-p-gp "]" "array" "assignment") grand-parent ruby-ts-mode-indent-offset)))

           ;; method parameters with and without '('
           ((query "(method_parameters \"(\" _ @indent)") first-sibling 1)
           ((parent-is "method_parameters") first-sibling 0)


           ,@(if ruby-ts-mode-indent-split-exp-by-term
                 `(((ancestor-is "parenthesized_statements") (ancestor-start "parenthesized_statements") 1)
                   ((ancestor-is "assignment") (ancestor-start "assignment") ruby-ts-mode-indent-offset)))

           ((node-is "body_statement") parent ruby-ts-mode-indent-offset)
           ((parent-is "body_statement") first-sibling 0)
           ((parent-is "binary") first-sibling 0)

           ;; "when" list spread across multiple lines
           ((n-p-gp "pattern" "when" "case") (nth-sibling 1) 0)
           ((n-p-gp nil "then" "when") grand-parent ruby-ts-mode-indent-offset)

           ;; if / unless unless expressions
           ((node-is "else")  parent-bol 0)
           ((node-is "elsif") parent-bol 0)
           ((node-is "when")  parent-bol 0)
           ((ancestor-is "then") (bol (ancestor-node "if")) ruby-ts-mode-indent-offset)
           ((parent-is "else") parent-bol ruby-ts-mode-indent-offset)
           ((parent-is "elsif") parent-bol ruby-ts-mode-indent-offset)

           ;; for, while, until loops
           ((parent-is "do") grand-parent ruby-ts-mode-indent-offset)
           
           ;; Assignment of hash and array
           ((n-p-gp "}" "hash" "assignment") grand-parent 0)
           ((n-p-gp "pair" "hash" "assignment") grand-parent ruby-ts-mode-indent-offset)
           ((n-p-gp "]" "array" "assignment") grand-parent 0)
           ((n-p-gp ".*" "array" "assignment") grand-parent ruby-ts-mode-indent-offset)

           ;; hash and array other than assignments
           ((node-is "}") first-sibling 0)
           ((parent-is "hash") first-sibling 1)
           ((node-is "]") first-sibling 0)
           ((parent-is "array") first-sibling 1)

           ;; method call arguments with and without '('
           ((query "(argument_list \"(\" _ @indent)") first-sibling 1)
           ((parent-is "argument_list") first-sibling 0)

           )))
    `((base ,@common))))

(defun ruby-ts-mode--class-or-module-p (node)
  "Predicate if NODE is a class or module."
  (string-match-p "class\\|module" (treesit-node-type node)))

(defun ruby-ts-mode--get-name (node)
  "Return the text of the `name' field of NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun ruby-ts-mode--full-name (node)
  "Return the fully qualified name of NODE."
  (let* ((name (ruby-ts-mode--get-name node))
         (delimiter "#"))
    (while (setq node (treesit-parent-until node #'ruby-ts-mode--class-or-module-p))
      (setq name (concat (ruby-ts-mode--get-name node) delimiter name))
      (setq delimiter "::"))
    name))

(defun ruby-ts-mode--imenu-helper (node)
  "Convert a treesit sparse tree NODE in an imenu list.
Helper for `ruby-ts-mode--imenu' which converts a treesit sparse
NODE into a list of imenu ( name . pos ) nodes"
  (let* ((ts-node (car node))
         (subtrees (mapcan #'ruby-ts-mode--imenu-helper (cdr node)))
         (name (when ts-node
                 (ruby-ts-mode--full-name ts-node)))
         (marker (when ts-node
                   (set-marker (make-marker)
                               (treesit-node-start ts-node)))))
    (cond
     ((or (null ts-node) (null name)) subtrees)
     ;; Don't include the anonymous "class" and "module" nodes
     ((string-match-p "(\"\\(class\\|module\\)\")"
                      (treesit-node-string ts-node))
      nil)
     (subtrees
      `((,name ,(cons name marker) ,@subtrees)))
     (t
      `((,name . ,marker))))))

;; For now, this is going to work like ruby-mode and return a list of
;; class, modules, def (methods), and alias.  It is likely that this
;; can be rigged to be easily extended.
(defun ruby-ts-mode--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((root (treesit-buffer-root-node))
         (nodes (treesit-induce-sparse-tree root "^\\(method\\|alias\\|class\\|module\\)$")))
    (ruby-ts-mode--imenu-helper nodes)))

(defun ruby-ts-mode--set-indent-style (language)
  "Helper function to set the indentation style.
Currently LANGUAGE is ignored but should be set to `ruby'."
  (let ((style
         (if (functionp ruby-ts-mode-indent-style)
             (funcall ruby-ts-mode-indent-style)
           (pcase ruby-ts-mode-indent-style
             ('base (alist-get 'base (ruby-ts-mode--indent-styles language)))))))
    `((,language ,@style))))

(define-derived-mode ruby-ts-base-mode prog-mode "Ruby"
  "Major mode for editing Ruby, powered by tree-sitter."
  :syntax-table ruby-ts-mode--syntax-table

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (regexp-opt '("method"
                            "singleton_method")))

  ;; AFAIK, Ruby can not nest methods
  (setq-local treesit-defun-prefer-top-level nil)

  ;; Imenu.
  (setq-local imenu-create-index-function #'ruby-ts-mode--imenu)

  ;; seems like this could be defined when I know more how tree sitter
  ;; works.
  (setq-local which-func-functions nil)

  (setq-local treesit-font-lock-feature-list
              '(( comment definition)
                ( keyword preprocessor string type)
                ( assignment constant escape-sequence label literal property )
                ( bracket delimiter error function operator variable)))
  )

(define-derived-mode ruby-ts-mode ruby-ts-base-mode "Ruby-TS"
  "Major mode for editing Ruby, powered by tree-sitter."
  :group 'ruby

  (unless (treesit-ready-p 'ruby)
    (error "Tree-sitter for Ruby isn't available"))

  (treesit-parser-create 'ruby)

  ;; Comments.
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  (setq-local treesit-simple-indent-rules
              (ruby-ts-mode--set-indent-style 'ruby))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (ruby-ts-mode--font-lock-settings 'ruby))

  (treesit-major-mode-setup))

(provide 'ruby-ts-mode)

;;; ruby-ts-mode.el ends here
