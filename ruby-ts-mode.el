;;; ruby-ts-mode.el --- tree-sitter support for Ruby  -*- lexical-binding: t; -*-

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

;; This file creates ruby-ts-mode which is a major mode for GNU Emacs
;; for editting Ruby files that uses Tree Sitter to parse the
;; language.  More information about Tree Sitter can be found in the
;; ELisp Info pages as well as this website:
;; https://tree-sitter.github.io/tree-sitter/

;; The process to enable using ruby-ts-mode or any major mode
;; leveraging tree sitter is currently a bit complex.  There are four
;; pieces that needs to be accomplished.

;; * Emacs needs to be compiled with tree-sitter enabled
;;     If you compile your own Emacs, this is accomplished with adding
;;     --with-native-compilation to the configure line.  If you are
;;     using pre-built Emacs, then they will need to alter their build
;;     process.  To test to see if your Emacs is enabled, execute
;;     (treesit-available-p)

;; * The tree sitter binary needs to be installed
;;     This is platform dependent.  On macOS, brew can be used.  Other
;;     platforms and their package managers should eventually make it
;;     available.  There is also doing it by hand.
;;
;;         git clone https://github.com/tree-sitter/tree-sitter.git
;;         cd tree-sitter
;;         make
;;         make install

;; * The tree sitter language specific parser needs to be installed
;;     If you have the Emacs source, you can cd to
;;     admin/notes/tree-sitter/build-module and execute:
;;         ./build.sh ruby
;;     or you can download this git repository:
;;         git clone git@github.com:casouri/tree-sitter-module.git
;;         cd tree-sitter-module
;;         ./build.sh ruby
;;     In both cases, a dist subdirectory is created and the shared
;;     library is in that directory.  The library needs to be put
;;     either in one of three places:
;;       a: the standard shared library load path such as
;;          /usr/local/lib.
;;       b: A subdirectory in your emacs-user-directory called
;;          tree-sitter.
;;       c: Any place and point treesit-extra-load-path to them.

;; * The appropriate major mode needs to be loaded and enabled
;;     a: (load "/path/to/ruby-ts-mode")
;;        M-x ruby-ts-mode
;;     b: (require 'ruby-ts-mode)
;;        M-x ruby-ts-mode
;;     c: (add-to-list 'auto-mode-alist '("\\.rb\\)\\'" . ruby-ts-mode))
;;   With the latter two assuming that this file is in your load-path.

;; Tree Sitter brings a lot of power and versitility which can be
;; broken into these features.

;; * Font Lock

;; The ability to color the source code is not new but what is new is
;; the versatility to enable and disable particular font lock rules.
;; I suggest reviewing variable treesit-font-lock-level and function
;; treesit-font-lock-recompute-features to get a better understanding
;; of the following.

;; Currently tree treesit-font-lock-feature-list is set with the
;; following levels:
;;   1: comment
;;   2: keyword regexp string type
;;   3: builtin constant constant-assignment delimiter escape-sequence
;;      function global global-assignment instance instance-assignment
;;      interpolation literal symbol variable variable-assignment
;;   4: bracket error operator punctuation

;; Thus if treesit-font-lock-level is set to level 3 which is its
;; default, all the features listed in levels 1 through 3 above will
;; be enabled.  i.e. those features will font lock or colorize the
;; code accordingly.  Individual features can be added and removed via
;; treesit-font-lock-recompute-features.

;; describe-face can be used to view how a face looks.

;; Fonts defined in font-lock.el:
;;   font-lock-punctuation-face -- User for comma and other
;;     punctuation marks if there are any.  Feature: punctuation

;;   font-lock-bracket-face - Used for brackets: (, ), {, }, [, ].
;;     Feature: bracket

;;   font-lock-delimiter-face -- Used for quotes ("), (') as well as
;;     percent literals: %q, %, %Q, %w, %W, %i, %I, %s, %x.
;;     Feature: delimiter

;;   font-lock-constant-face -- Used for symbols and delimted
;;   symbols (e.g. :"foo dog").  Also, values within %i, %I, and %s
;;   literals.  Feature: symbol

;;   font-lock-builtin-face - Used for Ruby's global variables.
;;     Feature: builtin

;;   font-lock-comment-delimiter-face -- Used for the leading "#" in
;;     comments.  Feature: comment

;;   font-lock-comment-face -  Used for comments.
;;     Feature: comment

;;   font-lock-constant-face - Used for true, false, nil, self, and
;;     super.  Feature: constant

;;   tbd-lvalue -- Used for the lvalue of an assignment.
;;     e.g. the foo will be colored different from the blah or bar:
;;     foo = blah + bar
;;     This allows the user to visually see assignments in the code.
;;     Feature: variable-assignment

;;   tbd-constant-assignment-face -- User for the declaration and
;;     assignment of a constant.  e.g.  Foo = 12 will be colored
;;     differently from a free standing Foo.
;;     Feature: constant-assignment

;;   font-lock-escape-face -- Used to color escape sequences within
;;     strings.  For example in the string "Hippo\tWater", the \t
;;     sequence will be colored differently from the rest of the
;;     string.  Feature: escape-sequence
;;  
;;   font-lock-function-name-face -- Used for method names.  For
;;     example, the foo in either def foo ... or foo(a, b, c)
;;     Feature: function

;;   font-lock-keyword-face -- Used for keywords listed in
;;   ruby-ts-mode--keywords.  Feature: keyword

;;   font-lock-negation-char-face -- Used for '!'
;;     Keyword: operator

;;   font-lock-number-face -- Used for integers, floats, complex, and
;;     rational numbers.  The unary + and - are also font locked when
;;     it precedes a number.  Feature: literal

;;   font-lock-operator-face -- Used for operators listed in
;;     ruby-ts-mode--operators.  Note ruby-ts-mode--operators is made
;;     up of several other lists such as
;;     ruby-ts-mode--operators-arithmetic so the user can easily
;;     customize the set of operators affected.  Feature: operator

;;   font-lock-variable-name-face -- Used for global variable and
;;     instance variable references.  Feature: global

;;   font-lock-property-face -- User for global variable and instance
;;     variable assignments.  Feature: global-assignment

;;   font-lock-regexp-grouping-backslash -- Used for the contents of
;;     regular expressions.  The parser does not pick out grouping
;;     constructs, etc within regular expressions.  I believe, in
;;     theory, this could be added perhaps later.  Feature: regexp

;;   font-lock-regexp-grouping-construct -- User for the '/' or '%r{'
;;     containing a regular expression.  Feature: regexp

;;   font-lock-string-face -- Used for strings.
;;     Feature: string

;;   font-lock-doc-face -- Used for string interpolation delimiters #{
;;   and }.  Feature: interpolation

;;   font-lock-type-face -- used for constants (i.e. identifiers
;;   starting with upper case).  Feature: type

;;   tbd-variable-face -- Used for variables.
;;     Feature: variable

;;   font-lock-warning-face -- Used for syntax errors according to the
;;     tree sitter Ruby language parser.  Feature: error


;; * Indent

;; Describe ruby-ts-mode-right-justify-arrays and
;; ruby-ts-mode-indent-split-exp-by-term.

;; * IMenu
;; * Navigation
;; * Which-func

;;; Code:

(require 'treesit)
(require 'ruby-mode)

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

;; I need to come back and revisit this after the tests are working.
;;
;; (defcustom ruby-ts-mode-indent-split-exp-by-term t
;;   "Indent expressions split across lines as `ruby-mode' did.

;; If set to true, long expressions that are split across lines will be
;; indented like `enh-ruby-mode' would indent the lines which is similar
;; to `c-mode'.  Thus:

;; with_paren = (a + b *
;;               c * d +
;;               12)

;; without_paren = a + b *
;;   c * d +
;;   12

;; If set to nil, long expressions are indented based upon the expression
;; parsed hierarchy which is similar to how `ruby-mode' indented.  Thus:

;; with_paren = (a + b *
;;                   c * d +
;;               12)

;; without_paren = a + b *
;;                     c * d +
;;                 12"
;;   :type 'boolean
;;   :group 'ruby)

(defcustom ruby-ts-mode-include-predefined-constants t
  "Font lock Ruby pre-defined global constants.
When true, `ruby-ts-mode--predefined-constants' are font lock the same
as `ruby-ts-mode--predefined'."
  :type 'boolean
  :group 'ruby)

(defcustom ruby-ts-mode-call-block t
  "When non-nil:

some_variable = 4 +
  some_array.
    duck.
    reduce do |acc, x|
    acc + x
  end

foo
  .bar
  .dog
  .house do |z|
  dog = 12
  cat = 44
end

verses when nil:


some_variable = 4 +
  some_array.
    duck.
    reduce do |acc, x|
      acc + x
    end

foo
  .bar
  .dog
  .house do |z|
    dog = 12
    cat = 44
  end")

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

(defvar ruby-ts-mode--punctuation '(",")
  "Ruby's punctuation characters.")

(defvar ruby-ts-mode--predefined-constants
  (rx (or "ARGF" "ARGV" "DATA" "ENV" "RUBY_COPYRIGHT"
          "RUBY_DESCRIPTION" "RUBY_ENGINE" "RUBY_ENGINE_VERSION"
          "RUBY_PATCHLEVEL" "RUBY_PLATFORM" "RUBY_RELEASE_DATE"
          "RUBY_REVISION" "RUBY_VERSION" "STDERR" "STDIN" "STDOUT"
          "TOPLEVEL_BINDING"))
  "Ruby predefined global constants.
These are currently unused")

(defvar ruby-ts-mode--predefined
  (rx (or "$!" "$@" "$~" "$&" "$‘" "$‘" "$+" "$=" "$/" "$\\" "$," "$;"
          "$." "$<" "$>" "$_" "$*" "$$" "$?" "$LOAD_PATH"
          "$LOADED_FEATURES" "$DEBUG" "$FILENAME" "$stderr" "$stdin"
          "$stdout" "$VERBOSE" "$-a" "$-i" "$-l" "$-p"
          (seq "$" (+ digit))))
  "Ruby global variables (but not global constants.")

(defun rtsn--lineno (node)
  "Return line number of NODE's start."
  (line-number-at-pos (treesit-node-start node)))

;; doc/keywords.rdoc in the Ruby git repository considers these to be
;; reserved keywords.  If these keywords are added to the list, it
;; causes the font-lock to stop working.
;;
;; "__ENCODING__" "__FILE__" "__LINE__" "false" "self" "super" "true"
;;
;; "nil" (which does not exhibit this issue) is also considered a
;; keyword but I removed it and added it as a constant.
;;
(defun ruby-ts-mode--keywords (_language)
  "Ruby keywords for tree-sitter font-locking.
Currently LANGUAGE is ignored but shoule be set to `ruby'."
  (let ((common-keywords
         '("BEGIN" "END" "alias" "and" "begin" "break" "case" "class"
           "def" "defined?" "do" "else" "elsif" "end" "ensure" "for"
           "if" "in" "module" "next" "not" "or" "redo" "rescue"
           "retry" "return" "then" "undef" "unless" "until" "when"
           "while" "yield")))
    common-keywords))

(defun ruby-ts-mode--comment-font-lock (node override start end &rest _)
  "Apply font lock to comment NODE within START and END.
Applies `font-lock-comment-delimiter-face' and
`font-lock-comment-face' See `treesit-fontify-with-override' for
values of OVERRIDE"
  ;; Emperically it appears as if (treesit-node-start node) will be
  ;; where the # character is at and (treesit-node-end node) will be
  ;; the end of the line
  ;; (message "comment-font-lock node:%S start:%S end:%s" node start end)
  (let* ((node-start (treesit-node-start node))
         (plus-1 (1+ node-start))
         (node-end (treesit-node-end node))
         (text (treesit-node-text node t)))
    (if (and (>= node-start start)
             (<= plus-1 end)
             (string-match-p "\\`#" text))
        (treesit-fontify-with-override node-start plus-1 font-lock-comment-delimiter-face override))
    (treesit-fontify-with-override (max plus-1 start) (min node-end end) font-lock-comment-face override)))

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
   '((comment) @ruby-ts-mode--comment-font-lock)

   :language language
   :feature 'builtin
   `(((global_variable) @var (:match ,ruby-ts-mode--predefined @var)) @font-lock-builtin-face
     ,@(when ruby-ts-mode-include-predefined-constants
         `(((constant) @var (:match ,ruby-ts-mode--predefined-constants @var)) @font-lock-builtin-face)))

   :language language
   :feature 'keyword
   `([,@(ruby-ts-mode--keywords language)] @font-lock-keyword-face)

   :language language
   :feature 'constant
   '((true) @font-lock-doc-markup-face
     (false) @font-lock-doc-markup-face
     (nil) @font-lock-doc-markup-face
     (self) @font-lock-doc-markup-face
     (super)  @font-lock-doc-markup-face)

   ;; :language language
   ;; :feature 'constant-assignment
   ;; :override t
   ;; '((assignment
   ;;    left: (constant) @tbd-constant-assignment-face)
   ;;   (assignment
   ;;    left: (scope_resolution name: (constant) @tbd-constant-assignment-face)))

   :language language
   :feature 'symbol
   '((bare_symbol) @font-lock-constant-face
     (delimited_symbol (string_content) @font-lock-constant-face)
     (hash_key_symbol) @font-lock-constant-face
     (simple_symbol) @font-lock-constant-face)

   ;; Before 'operator so (unary) works.
   :language language
   :feature 'literal
   '((unary ["+" "-"] [(integer) (rational) (float) (complex)]) @font-lock-number-face
     (integer) @font-lock-number-face
     (float) @font-lock-number-face
     (complex) @font-lock-number-face
     (rational) @font-lock-number-face)

   ;; Also before 'operator because % and / are operators
   :language language
   :feature 'regexp
   '((regex "/" @font-lock-regexp-grouping-construct)
     (regex _ (string_content) @font-lock-regexp-grouping-backslash))

   :language language
   :feature 'operator
   `("!" @font-lock-negation-char-face
     [,@ruby-ts-mode--operators] @font-lock-operator-face)

   :language language
   :feature 'delimiter
   '((delimited_symbol [ ":\"" "\"" ] @font-lock-delimiter-face)
     (string "\"" @font-lock-delimiter-face)
     (string_array [ "%w(" ")" ] @font-lock-delimiter-face)
     (subshell "`" @font-lock-delimiter-face)
     (symbol_array [ "%i(" ")"] @font-lock-delimiter-face))

   :language language
   :feature 'string
   '((string_content) @font-lock-string-face)

   :language language
   :feature 'interpolation
   '((interpolation "#{" @font-lock-doc-face)
     (interpolation "}" @font-lock-doc-face))

   :language language
   :feature 'type
   '((constant) @font-lock-type-face)

   :language language
   :feature 'global-assignment
   '((assignment
      left: (global_variable) @font-lock-property-face))

   :language language
   :feature 'global
   '((global_variable) @font-lock-variable-name-face)

   :language language
   :feature 'instance-assignment
   '((assignment
      left: (instance_variable) @font-lock-property-face))

   :language language
   :feature 'instance
   '((instance_variable) @font-lock-variable-name-face)

   :language language
   :feature 'function
   '((call
      method: (identifier) @font-lock-function-name-face)
     (method
      name: (identifier) @font-lock-function-name-face))

   ;; :language language
   ;; :feature 'variable
   ;; '((identifier) @tbd-variable-face)

   ;; :language language
   ;; :feature 'variable-assignment
   ;; '((assignment
   ;;    left: (identifier) @tbd-lvalue)
   ;;   (assignment
   ;;    left: (left_assignment_list (identifier) @tbd-lvalue))
   ;;   (operator_assignment
   ;;    left: (identifier) @tbd-lvalue))

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

   :language language
   :feature 'punctuation
   `(([,@ruby-ts-mode--punctuation] @font-lock-punctuation-face))))

;;;
;;; These routines would be better added to treesit.el  They are
;;; intended to be used with indent rules
;;;

(defun treesit-type-pred (regexp)
  "Return predicate taking a node returning non-nil if REGEXP matches type of node."
  (lambda (node)
    (string-match-p regexp (treesit-node-type node))))

(defun parent-node (&rest _)
  "Return the parent node matching ident rule."
  (lambda (_n parent &rest _)
    parent))

(declare-function ruby-smie--indent-to-stmt-p "ruby-mode.el" (keyword))

(defun option-ruby-alignable-keywords (pred)
  "Return either start or bol of PRED.
PRED should specify a node that is listed in
`ruby-alignable-keywords'.  If PRED is listed in user option
`ruby-align-to-stmt-keywords', then return the BOL of PRED.
Otherwise return start of PRED."
  (lambda (node parent bol &rest rest)
    (let* ((pred-node (funcall pred node parent bol rest))
           (temp (treesit-node-start pred-node))
           (keyword (treesit-node-type pred-node))
           (bol (ruby-smie--indent-to-stmt-p keyword)))
      (when temp
        (if bol
            (save-excursion
              (goto-char temp)
              (back-to-indentation)
              (point))
          temp)))))

(defun bol (pred)
  "Return bol of PRED.
PRED should take (node parent bol &rest rest) and return a node.
Returns bol of the current line if PRED returns nil."
  (lambda (node parent bol &rest rest)
    (save-excursion
      (let ((temp (treesit-node-start (funcall pred node parent bol rest))))
        (if temp
            (goto-char temp))
        (back-to-indentation)
        (point)))))

(defun grand-parent-is (type)
  "Check grand parent's type matches regexp TYPE."
  (lambda (_n parent &rest _)
    (string-match-p type (treesit-node-type (treesit-node-parent parent)))))

(defun grand-parent-node ()
  "Return grand parent node."
  (lambda (_n parent &rest _)
    (treesit-node-parent parent)))

(defun ancestor-start (type)
  "Return start of closest ancestor matching regexp TYPE."
  (lambda (node &rest _)
    (treesit-node-start (treesit-parent-until node (treesit-type-pred type)))))

(defun ancestor-is (type)
  "Check that ancestor's type matches regexp TYPE."
  (lambda (node &rest _)
    ;; (message "ancestor-is node: %S" node)
    (treesit-parent-until node (treesit-type-pred type))))

(defun ruby-ts-mode--align-call ()
  "Align chained method calls.
See `ruby-align-chained-calls' for details."
  (lambda (node parent &rest _)
    (message "node: %S; parent: %S" node parent)
    (if ruby-align-chained-calls
        (let* ( temp )
          (while (and parent
                      (setq temp (treesit-node-parent parent))
                      (message "moving up: temp: %S" temp)
                      (string-match-p "call" (treesit-node-type temp)))
            (setq parent temp))
          (setq temp (treesit-search-subtree parent "\\." nil t))
          (message "dot: %S" temp)
          (treesit-node-start temp))
      (+ (treesit-node-start parent) ruby-ts-mode-indent-offset))))

(defun do-ruby-align-chained-calls ()
  "To Be Documented"
  (lambda (node parent &rest _)
    (and ruby-align-chained-calls
         (equal "." (treesit-node-type node))
         (equal "call" (treesit-node-type parent)))))

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
         (grand-parent-node (treesit-node-parent parent))
         ;; if there is no grand-parent, node is adjusted relative to
         ;; the bol of the line where point happens to be.
         (grand-parent-bol (save-excursion
                             (if grand-parent-node
                                 (goto-char (treesit-node-start grand-parent-node)))
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

           ;; a "do_block" has a "body_statement" child which has the
           ;; statements as children within it.  The problem is that
           ;; the first statement starts at the same point as the
           ;; body_statement and so treesit-simple-indent is called
           ;; with node set to body_statement on the first statement
           ;; but with node set to the statement and parent set to
           ;; body_statement for all others. ... Fine.  Be that way.
           ;; Ditto for "block" and "block_body"
           ;;
           ;; FIXME: What follows are two sets of three queries.  The
           ;; first trigger when the option is enabled and the second
           ;; trigger when the option is disabled.  The problem is
           ;; that this is not dynamic.  I plan to rewrite these so
           ;; that the value of the option is retrieved at the time
           ;; the line is indented like I'm doing elsewhere.
           ;;
           ;; Old code -- need to figure out where / when this was used.
           ;;
           ;; ,@(when ruby-ts-mode-call-block
           ;;     '(((n-p-gp "end" "do_block" "call") (bol (grand-parent-node)) 0)
           ;;       ((n-p-gp nil "do_block" "call") (bol (grand-parent-node)) ruby-ts-mode-indent-offset)
           ;;       ((parent-is "body_statement") first-sibling 0)))
           ;; ((node-is "body_statement") parent-bol ruby-ts-mode-indent-offset)
           ;; ((parent-is "body_statement") (bol (grand-parent-node)) ruby-ts-mode-indent-offset)
           ;; ((match "end" "do_block") parent-bol 0)

           ;; ((n-p-gp "block_body" "block" nil) parent-bol ruby-ts-mode-indent-offset)
           ;; ((n-p-gp nil "block_body" "block") (bol (grand-parent-node)) ruby-ts-mode-indent-offset)
           ;; ((match "}" "block") (bol (grand-parent-node)) 0)

           ;; if then else elseif notes:
           ;;
           ;;   1. The "then" starts at the end of the line that ends
           ;;      the if condition which can be on a different line
           ;;      from the "if".
           ;;
           ;;   2. If there is an "elsif", it is a sibling to the then
           ;;      BUT the "else" that follows is now a child of the
           ;;      "elsif".
           ;;
           ;;   3. The statements within each of these are direct
           ;;      children.  There is no intermediate construct such
           ;;      as a block_statement.
           ;;
           ;; I'm using very restrictive patterns hoping to reduce rules
           ;;triggering unintentionally.
           ((match "else" "if")
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((match "elsif" "if")
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((match "end" "if")
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((n-p-gp nil "then\\|else\\|elsif" "if\\|unless")
            (option-ruby-alignable-keywords (grand-parent-node)) ruby-ts-mode-indent-offset)

           ;; case expression can have "case" "when" "in" (called
           ;; in_clause) "else" and "end".  "case" can be in
           ;; ruby-align-to-stmt-keywords.  "when", etc are children
           ;; of "case".  The statements under when is part of a
           ;; "then" that is the child of "when".

           ;; case expression: when, in_clause, and else are all
           ;; children of case.  when and in_clause have pattern and
           ;; body as fields.  body has "then" and then the statemets.
           ;; i.e. the statements are not children of when but then.
           ;; But for the statements are children of else.
           ((match "when" "case")
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((match "in_clause" "case")
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((match "else" "case")
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((match "end" "case")
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((n-p-gp nil "then" "when") grand-parent ruby-ts-mode-indent-offset)
           ((n-p-gp nil "then" "in_clause") grand-parent ruby-ts-mode-indent-offset)
           ((n-p-gp nil "else" "case") parent ruby-ts-mode-indent-offset)

           ;; The beauty of inconsistency :-)
           ;; while / until have only "do" as a child.  The "end" is a
           ;; child of "do".
           ((n-p-gp "end" "do" "while\\|until")
            (option-ruby-alignable-keywords (grand-parent-node)) 0)
           ((n-p-gp nil "do" "while\\|until")
            (option-ruby-alignable-keywords (grand-parent-node)) ruby-ts-mode-indent-offset)
            
           ;; begin can have rescue, ensure, else, and end.
           ;; statements are a child of begin.  rescue, ensure, else,
           ;; and end are also children of begin.  rescue has a then
           ;; as a child thus statements will be grand children of
           ;; rescue.
           ((n-p-gp nil "then" "rescue")
            (option-ruby-alignable-keywords (grand-parent-node)) ruby-ts-mode-indent-offset)
           ((n-p-gp nil "ensure\\|else" "begin")
            (option-ruby-alignable-keywords (parent-node)) ruby-ts-mode-indent-offset)
           ((match "rescue\\|ensure\\|else\\|end" "begin")
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((parent-is "begin")         ;last
            (option-ruby-alignable-keywords (parent-node)) ruby-ts-mode-indent-offset)            

           ;; for ... I don't think I have ever used a for loop in
           ;; Ruby.  The "in" (not an in_clause) and "do" are
           ;; children.  The statements are children of the "do".
           ;; And, of course, the "end" is a child of the "do".
           ((n-p-gp "end" "do" "for")
            (option-ruby-alignable-keywords (grand-parent-node)) 0)
           ((n-p-gp nil "do" "for")
            (option-ruby-alignable-keywords (grand-parent-node)) ruby-ts-mode-indent-offset)

           ;; method has a "body_statement" and the "end" as children.
           ;; The body_statement can have rescue, ensure, and else as
           ;; well as statements.  Note that the first statement of a
           ;; body_statement hits the node as "body_statement" and not
           ;; as the assignment, etc.
           ((match "end" ,treesit-defun-type-regexp)
            (option-ruby-alignable-keywords (parent-node)) 0)
           ((n-p-gp "\\`\\(rescue\\|ensure\\|else\\)\\'" "body_statement" ,treesit-defun-type-regexp)
            (option-ruby-alignable-keywords (grand-parent-node)) 0)
           ((n-p-gp nil "rescue\\|ensure\\|else" "body_statement") parent ruby-ts-mode-indent-offset)
           ((match "body_statement" ,treesit-defun-type-regexp) ;first statement
            (option-ruby-alignable-keywords (parent-node)) ruby-ts-mode-indent-offset)
           ((n-p-gp nil "body_statement" ,treesit-defun-type-regexp) ;other statements
            (option-ruby-alignable-keywords (grand-parent-node)) ruby-ts-mode-indent-offset)

           ;; ((do-ruby-align-chained-calls) parent ruby-ts-mode-indent-offset)
           ((match "\\." "call") (ruby-ts-mode--align-call) 0)

           ;; Old code before option-ruby-alignable-keywords
           ;; "while" and "until" have a "do" child that have
           ;; statements as their children.
           ;; ((n-p-gp "end" "do" "while") grand-parent 0)
           ;; ((parent-is "do") grand-parent ruby-ts-mode-indent-offset)
           
           ((node-is ")") parent 0)

           ;; What I had before -- remove both for now.
           ;; ((node-is "end") parent 0)
           ;; ((node-is "end") parent-bol 0)

           ;; old code before option-ruby-alignable-keywords
           ;; ((parent-is "begin") parent ruby-ts-mode-indent-offset)

           ,@(when ruby-ts-mode-right-justify-arrays
               '(((query "(array \"[\" ( (integer) ( \",\" (_) )*) @indent \",\"? \"]\")")
                  ruby-ts-mode--right-justify-array-leaf ruby-ts-mode-indent-offset)
                 ((n-p-gp "]" "array" "assignment") grand-parent ruby-ts-mode-indent-offset)))

           ;; method / block parameters / arguments with and without '('
           ((query "(method_parameters \"(\" _ @indent)") first-sibling 1)
           ((parent-is "method_parameters") first-sibling 0)
           ((query "(argument_list \"(\" _ @indent)") first-sibling 1)
           ((parent-is "argument_list") first-sibling 0)
           ((parent-is "block_parameters") first-sibling 1)


           ;; I need to come back and revisit this after the tests are working.
           ;;
           ;; ,@(when ruby-ts-mode-indent-split-exp-by-term
           ;;     '(((ancestor-is "parenthesized_statements") (ancestor-start "parenthesized_statements") 1)
           ;;       ((ancestor-is "assignment") (ancestor-start "assignment") ruby-ts-mode-indent-offset)))

           ;; Changed for ruby-align-to-stmt-keywords-t
           ((parent-is "binary") first-sibling ruby-ts-mode-indent-offset)

           ;; "when" list spread across multiple lines
           ;; old code...
           ;; ((n-p-gp "pattern" "when" "case") (nth-sibling 1) 0)
           ;; ((n-p-gp nil "else" "case") parent ruby-ts-mode-indent-offset)
           ;; ((node-is "when")  parent-bol 0)

           ;; Assignment of hash and array
           ((n-p-gp "}" "hash" "assignment") grand-parent 0)
           ((n-p-gp "pair" "hash" "assignment") grand-parent ruby-ts-mode-indent-offset)
           ((n-p-gp "]" "array" "assignment") grand-parent 0)
           ((n-p-gp ".*" "array" "assignment") grand-parent ruby-ts-mode-indent-offset)

           ;; hash and array other than assignments.  Note that the
           ;; first sibling is the "{" or "[".
           ((node-is "}") first-sibling 0)
           ((parent-is "hash") first-sibling 1)
           ((node-is "]") first-sibling 0)
           ((parent-is "array") first-sibling 1)

           ;; method call arguments with and without '('

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

(defun rtsm--arrow-up-start (arg)
  "Move to the start ARG levels up or out."
  (interactive "p")
  (setq arg (or arg 1))
  (let* ((pnt (point))
         (found (treesit-node-at pnt))
         (pos (treesit-node-start found))
         new-pos)
    (while (and found pos (> arg 0))
      (setq found (treesit-node-parent found)
            new-pos (treesit-node-start found))
      (when (and new-pos (not (= new-pos pos)))
        (setq arg (1- arg)
              pos new-pos)))
    (if pos
        (goto-char pos)
      (error "Something didn't work"))))

(defun ruby-ts-mode--class-name (node)
  "Return NODE's name.
Assumes NODE's type is \"class\" or \"method\""
  (list
   (treesit-node-text
    (treesit-node-child-by-field-name
     node
     (if (equal "singleton_class" (treesit-node-type node)) "value" "name"))
    t)))
  
(defun ruby-ts-mode--method-name (node)
  "Return the method name of NODE.
Assumes NODE's type is method or singleton_method."
  (if (equal "method" (treesit-node-type node))
      (list (treesit-node-text (treesit-node-child-by-field-name node "name") t))
    (let* ((children (treesit-node-children node))
           ;; 0th is "def"
           (first (nth 1 children))
           (third (nth 3 children)))
      (cond
       ((equal "(" (treesit-node-type first))
        (list (treesit-node-text (nth 2 children) t)
              (treesit-node-text (nth 5 children) t)))
       ;; ((equal "self" (treesit-node-type first))
       ;;  (list (treesit-node-text third t)))
       (t (mapcar (lambda (n)
                    (treesit-node-text n t))
                  (list first third)))))))

(defconst ruby-ts-mode--class-or-module-regex
  (rx string-start
      (or "class" "module" "singleton_class")
      string-end)
  "Regular expression that matches a class or module's node type.")

(defun ruby-ts-mode--log-current-function ()
  "Return the current method name as a string.
The hash (#) is for instance methods only which are methods
\"defined on a class\" -- which is 99% of methods.  Otherwise, a
dot (.) is used.  Double colon (::) is used between classes.  The
leading double colon is not added."
  (let* ((node (treesit-node-at (point)))
         (method (treesit-parent-until node (treesit-type-pred treesit-defun-type-regexp)))
         (class (or method node))
         (result nil)
         (sep "#")
         (method-list nil)
         (class-list nil)
         (method-name nil))

    (when method
      (setq method-list (ruby-ts-mode--method-name method))
      (unless (= 1 (length method-list))
        (setq sep ".")))
    (while (setq class (treesit-parent-until class
                                             (treesit-type-pred
                                              ruby-ts-mode--class-or-module-regex)))
      (setq class-list (append (ruby-ts-mode--class-name class) class-list)))
    (message "class-list: %S method-list:%S" class-list method-list)
    (setq method-name (car (last method-list))
          method-list (butlast method-list))
    (when (equal (car method-list) (car (last class-list)))
      (setq method-list (cdr method-list)))
    (message "class-list: %S method-list:%S method-name:%S" class-list method-list method-name)
    (dolist (ele (append class-list method-list))
      (cond
       ((equal "self" ele)
        (setq sep "."))
       ((string-match-p "\\`[^A-Z]" ele) ;not a class
        (setq sep "."
              result (if result
                         (concat result "::" ele)
                       ele)))
       (t (setq result (if result
                           (concat result "::" ele)
                         ele)))))
    (if method-name
        (concat result sep method-name)
      result)))

(defvar-keymap ruby-ts-mode--arrow-keys
  :doc "Transient keymap for arrow keys"
  ;; "<right>" #'rtsn-forward-argument-start
  "<right>" #'rtsn-forward-statement-start
  "<up>"    #'rtsm--arrow-up-start
  )
  
(defvar-keymap ruby-ts-mode-map
  :doc "Keymap used in Ruby mode"
  :parent prog-mode-map
  "C-M-h"     #'rtsn-mark-method
  "H-<right>"  #'rtsn-mark-statement
  "s-<right>"  #'rtsn-forward-method
  "M-<left>"  #'rtsn--raw-prev-sibling
  "M-<right>" #'rtsn--raw-next-sibling
  "C-c" ruby-ts-mode--arrow-keys)

(define-derived-mode ruby-ts-base-mode prog-mode "Ruby"
  "Major mode for editing Ruby, powered by tree-sitter."
  :syntax-table ruby-ts-mode--syntax-table

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (rx string-start
                  (or "method" "singleton_method")
                  string-end))

  ;; AFAIK, Ruby can not nest methods
  (setq-local treesit-defun-prefer-top-level nil)

  ;; Imenu.
  (setq-local imenu-create-index-function #'ruby-ts-mode--imenu)

  ;; seems like this could be defined when I know more how tree sitter
  ;; works.
  (setq-local which-func-functions nil)

  ;; FIXME -- make this a customizable variable so the user can
  ;; configure his own levels easily.
  (setq-local treesit-font-lock-feature-list
              '(( comment )
                ( keyword regexp string type)
                ( builtin constant constant-assignment
                  delimiter escape-sequence function global
                  global-assignment instance instance-assignment
                  interpolation literal symbol variable variable-assignment )
                ( bracket error operator punctuation ))))

;;;###autoload
(define-derived-mode ruby-ts-mode ruby-ts-base-mode "Ruby"
  "Major mode for editing Ruby, powered by tree-sitter."
  :group 'ruby

  (unless (treesit-ready-p 'ruby)
    (error "Tree-sitter for Ruby isn't available"))

  (treesit-parser-create 'ruby)

  (setq-local add-log-current-defun-function #'ruby-ts-mode--log-current-function)

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
