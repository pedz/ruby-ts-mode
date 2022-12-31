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
;;   ruby-ts--keywords.  Feature: keyword

;;   font-lock-negation-char-face -- Used for '!'
;;     Keyword: operator

;;   font-lock-number-face -- Used for integers, floats, complex, and
;;     rational numbers.  The unary + and - are also font locked when
;;     it precedes a number.  Feature: literal

;;   font-lock-operator-face -- Used for operators listed in
;;     ruby-ts--operators.  Note ruby-ts--operators is made
;;     up of several other lists such as
;;     ruby-ts--operators-arithmetic so the user can easily
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
(declare-function ruby-smie--indent-to-stmt-p "ruby-mode.el" (keyword))

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

;; Remove for now.
;;
;; (defcustom ruby-ts-mode-right-justify-arrays t
;;   "Right justify elements in an array.
;;
;; e.g.             or
;;   array = [            array = [   145,
;;        145,                      21110,
;;      21110,                         11]
;;         11
;;     ]
;;
;; verses           or
;;
;;   array = [            array = [145,
;;     145,                 21110,
;;     21110,               11]
;;     11
;;     ]"
;;   :type 'boolean
;;   :group 'ruby)

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
When true, `ruby-ts--predefined-constants' are font lock the same
as `ruby-ts--predefined'."
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

(defvar ruby-ts--syntax-table
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

(defvar ruby-ts--operators-arithmetic
  '("+" "-" "*" "/" "%" "**")
  "Ruby arithmetic operators for tree-sitter font-locking.")

;; treesit-query-validate doesn't like these:
;; "eql?" "equal?"
(defvar ruby-ts--operators-comparison
  '("==" "!=" ">" "<" ">=" "<=" "<=>" "===")
  "Ruby comparison operators for tree-sitter font-locking.")

(defvar ruby-ts--operators-assignment
  '("=" "+=" "-=" "*=" "/=" "%=" "**=")
  "Ruby assignment operators for tree-sitter font-locking.")

(defvar ruby-ts--operators-bitwise
  '("&" "|" "^" "~" "<<" ">>")
  "Ruby bitwise operators for tree-sitter font-locking.")

(defvar ruby-ts--operators-logical
  '("!" "&&" "and" "not" "or" "||")
  "Ruby logical operators for tree-sitter font-locking.")

(defvar ruby-ts--operators-ternary
  '("?" ":")
  "Ruby ternary operators for tree-sitter font-locking.")

(defvar ruby-ts--operators-range
  '(".." "...")
  "Ruby range operators for tree-sitter font-locking.")

(defvar ruby-ts--operators-defined
  '("defined?")
  "Ruby defined? operators for tree-sitter font-locking.")

(defvar ruby-ts--operators-dot-colon
  '("." "::")
  "Ruby dot and double colon operators for tree-sitter font-locking.")

(defvar ruby-ts--operators
  (append ruby-ts--operators-arithmetic
          ruby-ts--operators-comparison
          ruby-ts--operators-assignment
          ruby-ts--operators-bitwise
          ruby-ts--operators-logical
          ruby-ts--operators-ternary
          ruby-ts--operators-range
          ruby-ts--operators-defined
          ruby-ts--operators-dot-colon)
  "Ruby operators for tree-sitter font-locking.")

(defvar ruby-ts--punctuation '(",")
  "Ruby's punctuation characters.")

(defvar ruby-ts--predefined-constants
  (rx (or "ARGF" "ARGV" "DATA" "ENV" "RUBY_COPYRIGHT"
          "RUBY_DESCRIPTION" "RUBY_ENGINE" "RUBY_ENGINE_VERSION"
          "RUBY_PATCHLEVEL" "RUBY_PLATFORM" "RUBY_RELEASE_DATE"
          "RUBY_REVISION" "RUBY_VERSION" "STDERR" "STDIN" "STDOUT"
          "TOPLEVEL_BINDING"))
  "Ruby predefined global constants.
These are currently unused")

(defvar ruby-ts--predefined
  (rx (or "$!" "$@" "$~" "$&" "$‘" "$‘" "$+" "$=" "$/" "$\\" "$," "$;"
          "$." "$<" "$>" "$_" "$*" "$$" "$?" "$LOAD_PATH"
          "$LOADED_FEATURES" "$DEBUG" "$FILENAME" "$stderr" "$stdin"
          "$stdout" "$VERBOSE" "$-a" "$-i" "$-l" "$-p"
          (seq "$" (+ digit))))
  "Ruby global variables (but not global constants.")

(defconst ruby-ts--class-or-module-regex
  (rx string-start
      (or "class" "module" "singleton_class")
      string-end)
  "Regular expression that matches a class or module's node type.")

(defconst ruby-ts--method-regex
  (rx string-start
      (or "method" "singleton_method")
      string-end)
  "Regular expression matching methods and singleton methods.")

(defun ruby-ts--lineno (node)
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
(defun ruby-ts--keywords (_language)
  "Ruby keywords for tree-sitter font-locking.
Currently LANGUAGE is ignored but shoule be set to `ruby'."
  (let ((common-keywords
         '("BEGIN" "END" "alias" "and" "begin" "break" "case" "class"
           "def" "defined?" "do" "else" "elsif" "end" "ensure" "for"
           "if" "in" "module" "next" "not" "or" "redo" "rescue"
           "retry" "return" "then" "undef" "unless" "until" "when"
           "while" "yield")))
    common-keywords))

(defun ruby-ts--comment-font-lock (node override start end &rest _)
  "Apply font lock to comment NODE within START and END.
Applies `font-lock-comment-delimiter-face' and
`font-lock-comment-face' See `treesit-fontify-with-override' for
values of OVERRIDE"
  ;; Emperically it appears as if (treesit-node-start node) will be
  ;; where the # character is at and (treesit-node-end node) will be
  ;; the end of the line
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
(defun ruby-ts--font-lock-settings (language)
  "Tree-sitter font-lock settings.
Currently LANGUAGE is ignored but should be set to `ruby'."
  (treesit-font-lock-rules
   :language language
   :feature 'comment
   '((comment) @ruby-ts--comment-font-lock)

   :language language
   :feature 'builtin
   `(((global_variable) @var (:match ,ruby-ts--predefined @var)) @font-lock-builtin-face
     ,@(when ruby-ts-mode-include-predefined-constants
         `(((constant) @var (:match ,ruby-ts--predefined-constants @var)) @font-lock-builtin-face)))

   :language language
   :feature 'keyword
   `([,@(ruby-ts--keywords language)] @font-lock-keyword-face)

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
     [,@ruby-ts--operators] @font-lock-operator-face)

   :language language
   :feature 'delimiter
   '((delimited_symbol [ ":\"" "\"" ] @font-lock-delimiter-face)
     (string "\"" @font-lock-delimiter-face)
     (string_array [ "%w(" ")" ] @font-lock-delimiter-face)
     (subshell "`" @font-lock-delimiter-face)
     (symbol_array [ "%i(" ")"] @font-lock-delimiter-face))

   :language language
   :feature 'string
   '((string_content) @font-lock-string-face
     (heredoc_beginning) @font-lock-string-face
     (heredoc_content) @font-lock-string-face
     (heredoc_end) @font-lock-string-face)

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
   `(([,@ruby-ts--punctuation] @font-lock-punctuation-face))))

(defun ruby-ts--first-non-comment-child (node)
  "Return the first named child of NODE that is not a comment"
  (let ((child (treesit-node-child node 0 t)))
    (while (and child
                (equal "comment" (treesit-node-type child)))
      (setq child (treesit-node-next-sibling child t)))
    child))

;;
;; These routines would be better added to treesit.el  They are
;; intended to be used with indent rules
;;
;; I think this is over simplified but basically
;; treesit--simple-indent-eval calls the result with node, parent, and
;; bol. Thus all of these functions return a lambda that accepts three
;; arguments.  Somewhere something explains that &rest should always
;; be used in case extra arguments are added in the future.
;;

(defun ruby-ts--type-pred (regexp)
  "Return predicate taking a node returning non-nil if REGEXP matches type of node."
  (lambda (node)
    (string-match-p regexp (treesit-node-type node))))

(defun ruby-ts--parent-node (&rest _)
  "Return the parent node matching ident rule."
  (lambda (_n parent &rest _)
    parent))

(defun ruby-ts--align-keywords (pred)
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

(defun ruby-ts--bol (pred)
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

(defun ruby-ts--grand-parent-is (type)
  "Check grand parent's type matches regexp TYPE."
  (lambda (_n parent &rest _)
    (string-match-p type (treesit-node-type (treesit-node-parent parent)))))

(defun ruby-ts--grand-parent-node ()
  "Return grand parent node."
  (lambda (_n parent &rest _)
    (treesit-node-parent parent)))

(defun ruby-ts--ancestor-start (type)
  "Return start of closest ancestor matching regexp TYPE."
  (lambda (node &rest _)
    (treesit-node-start (treesit-parent-until node (ruby-ts--type-pred type)))))

(defun ruby-ts--ancestor-is (type)
  "Check that ancestor's type matches regexp TYPE."
  (lambda (node &rest _)
    (treesit-parent-until node (ruby-ts--type-pred type))))

(defun ruby-ts--align-chain-p (&rest _)
  "Return value of `ruby-align-chained-calls'."
  ruby-align-chained-calls)

(defun ruby-ts--parenless-call-arguments-indent-p (&rest -)
  "Return value of `ruby-parenless-call-arguments-indent'."
  ruby-parenless-call-arguments-indent)

(defun ruby-ts--align-chain (node parent &rest _)
  "Align chained method calls.
Align NODE which will be the dot (.) to the dot of the
first (outermost) call in the chain.  See
`ruby-align-chained-calls' for details.  PARENT will be the
\"call\" node.  Called only when `ruby-align-chained-calls' is
non-nil."
  (let* (first-call )
    (while (and parent
                (setq first-call (treesit-node-parent parent))
                (string-match-p "call" (treesit-node-type first-call)))
      (setq parent first-call))
    (treesit-node-start (treesit-search-subtree parent "\\." nil t))))

(defun ruby-ts--align-chain-calls ()
  "To Be Documented"
  (lambda (node parent &rest _)
    (and ruby-align-chained-calls
         (equal "." (treesit-node-type node))
         (equal "call" (treesit-node-type parent)))))

;; (defun ruby-ts--argument-indent (node parent &rest _)
;;   "Align arguments as specified by `ruby-parenless-call-arguments-indent'.
;; PARENT is argument_list.  NODE can be the closing paren.

;; This routine has N modes:

;; Args start on same line with parens:
;; foo(arg1,
;;     arg2)

;; Args start on same line without paren and
;; 'ruby-parenless-call-arguments-indent' is t.
;; foo arg1,
;;     arg2

;; Args start on same line without paren and
;; 'ruby-parenless-call-arguments-indent' is nil.
;; foo arg1,
;;   arg2

;; Args start on next line:
;; foo(
;;   arg1,
;;   arg2
;; )"
;;   (let* ((open-paren (treesit-node-child parent 0))
;;          (first-arg (ruby-ts--first-non-comment-child parent))
;;          (is-close-paren (equal ")" (treesit-node-type node))))
;;     ;; (message "paren: %S; first-arg: %S; is-close-paren: %S"
;;     ;;          open-paren first-arg is-close-paren)
;;     ;; (message "lineno: %d %d" (ruby-ts--lineno first-arg) (ruby-ts--lineno open-paren))
;;     (if (equal "(" (treesit-node-type open-paren))
;;         (if (= (ruby-ts--lineno first-arg) (ruby-ts--lineno open-paren))
;;             (treesit-node-start (if is-close-paren open-paren first-arg))
;;           ;; (message "here")
;;           (save-excursion
;;             (goto-char (treesit-node-start open-paren))
;;             (back-to-indentation)
;;             (point)))
;;       (if ruby-parenless-call-arguments-indent
;;           (treesit-node-start first-arg)
;;         (save-excursion
;;           (goto-char (treesit-node-start open-paren))
;;           (back-to-indentation)
;;           (point))))))

(defun ruby-ts--same-line-args-p (node parent &rest _)
  "Return non-nil when first argument is on the same line as the method.
PARENT will be argument_list.  NODE can be the close paren."
  (let* ((method (treesit-node-parent parent))
         (first-param (ruby-ts--first-non-comment-child parent)))
    (= (ruby-ts--lineno method) (ruby-ts--lineno first-param))))

(defun ruby-ts--same-line-params-p (node parent &rest _)
  "Return non-nil when first parameter is on the same line as the method.
PARENT will be method_parameters.  NODE can be the close paren."
  (let* ((method (treesit-node-parent parent))
         (first-param (ruby-ts--first-non-comment-child parent)))
    (= (ruby-ts--lineno method) (ruby-ts--lineno first-param))))

(defun ruby-ts--param-indent (node parent &rest _)
  "Indent parameters that start on next line.
Given: NODE is the parameter.  PARENT is
method_parameters. `ruby-ts--same-line-params-p' is nil.  Indent
according to `ruby-method-params-indent'

ruby-method-params-indent is 0
def foo(
  param1,
  param2
)

Params start on next line, ruby-method-params-indent is t
def foo(
      param1,
      param2
    )"
  (let ((method (treesit-node-parent parent)))
    (if (eq t ruby-method-params-indent)
        ;; For methods, the "name" is the name of the method but for
        ;; singleton methods, we need to find "object"
        (let* ((singleton (equal "singleton_method" (treesit-node-type method)))
               (name-node (treesit-node-child-by-field-name
                           method
                           (if singleton "object" "name"))))
          (message "name-node: %S" name-node)
          (treesit-node-start name-node))
      ;; Small Danger: if the method name plus the parent is less than
      ;; `ruby-method-params-indent', then the addition will put the
      ;; result on the next line and indented incorrectly.  There are
      ;; plausible ways to fix this but the probability seems rather
      ;; remote.
      (+ (treesit-node-start method) (or ruby-method-params-indent 0)))))

(defun ruby-ts--hash-array-indent (node parent &rest _)
  "Return column to indent hash or array elements.
NODE is the element or the closing brace or bracket.  PARENT is
the hash or array.

If the first named child is on the same line as the opening
bracket (the first unnamed child), then the elements are aligned
under the first child with the closing brace/bracket under the
opening brace/brack.

If the first named child is on a new line, then it is indented
`ruby-ts-mode-indent-offset' from the bol that the opening brace
or bracket is on.

FIXME Note that since this routine doesn't need any extra
argument, it does not need to use a lambda and so in the query,
it is used without parens.  e.g.:

   ((parent-is \"hash\") ruby-ts--hash-array-indent 0)

This is not consistent and I should cean it up the other
functions that do not take additional arguments."
  (let* ((open-brace (treesit-node-child parent 0 nil))
         (first-child (ruby-ts--first-non-comment-child parent))
         (node-type (treesit-node-type node))
         (is-brace (string-match-p "\\`\\(]\\|}\\)\\'" node-type))
         temp)
    (message "open-brace: %S; first-child: %S; node-type: %S; is-brace: %S"
             open-brace first-child node-type is-brace)
    (message "lineno %d %d" (ruby-ts--lineno open-brace) (ruby-ts--lineno first-child))
    (if (= (ruby-ts--lineno open-brace) (ruby-ts--lineno first-child))
        (treesit-node-start (if is-brace open-brace first-child))
      (message "here")
      (setq temp (+ (save-excursion     ;bug
                      (goto-char (treesit-node-start open-brace))
                      (back-to-indentation)
                      (message "point: %S" (point))
                      (point))
                    (if is-brace 0 ruby-ts-mode-indent-offset)))
      (message "temp: %S" temp)
      temp)))

;;
;; end of functions that can be used for queries
;;

(defalias 'ancestor-node #'ruby-ts--ancestor-is
  "Return ancestor node whose type matches regexp TYPE.")

(defun ruby-ts--right-justify-array-leaf ( node parent &rest _)
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
         (ruby-ts--grand-parent-node (treesit-node-parent parent))
         ;; if there is no grand-parent, node is adjusted relative to
         ;; the bol of the line where point happens to be.
         (grand-parent-bol (save-excursion
                             (if ruby-ts--grand-parent-node
                                 (goto-char (treesit-node-start ruby-ts--grand-parent-node)))
                             (back-to-indentation)
                             (point)))
         (align-column (if same-line
                           (- (+ (treesit-node-end open-bracket) max-length 1) ruby-ts-mode-indent-offset)
                         (+ grand-parent-bol max-length 1))))

    (- align-column node-length)))


           ;; REMOVED FROM ruby-ts--indent-styles
           ;;
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
           ;;     '(((n-p-gp "end" "do_block" "call") (ruby-ts--bol (ruby-ts--grand-parent-node)) 0)
           ;;       ((n-p-gp nil "do_block" "call") (ruby-ts--bol (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)
           ;;       ((parent-is "body_statement") first-sibling 0)))


           ;; Old code before ruby-ts--align-keywords
           ;; "while" and "until" have a "do" child that have
           ;; statements as their children.
           ;; ((n-p-gp "end" "do" "while") grand-parent 0)
           ;; ((parent-is "do") grand-parent ruby-ts-mode-indent-offset)
           
           ;; What I had before -- remove both for now.
           ;; ((node-is "end") parent 0)
           ;; ((node-is "end") parent-bol 0)

           ;; old code before ruby-ts--align-keywords
           ;; ((parent-is "begin") parent ruby-ts-mode-indent-offset)

           ;; I want to redo this as well so lets just remove it for now.
           ;; ,@(when ruby-ts-mode-right-justify-arrays
           ;;     '(((query "(array \"[\" ( (integer) ( \",\" (_) )*) @indent \",\"? \"]\")")
           ;;        ruby-ts--right-justify-array-leaf ruby-ts-mode-indent-offset)
           ;;       ((n-p-gp "]" "array" "assignment") grand-parent ruby-ts-mode-indent-offset)))

           ;; I need to come back and revisit this after the tests are working.
           ;;
           ;; ,@(when ruby-ts-mode-indent-split-exp-by-term
           ;;     '(((ruby-ts--ancestor-is "parenthesized_statements") (ruby-ts--ancestor-start "parenthesized_statements") 1)
           ;;       ((ruby-ts--ancestor-is "assignment") (ruby-ts--ancestor-start "assignment") ruby-ts-mode-indent-offset)))

           ;; "when" list spread across multiple lines
           ;; old code...
           ;; ((n-p-gp "pattern" "when" "case") (nth-sibling 1) 0)
           ;; ((n-p-gp nil "else" "case") parent ruby-ts-mode-indent-offset)
           ;; ((node-is "when")  parent-bol 0)

           ;; Assignment of hash and array
           ;; This needs to be controlled by an option.
           ;; ((n-p-gp "}" "hash" "assignment") grand-parent 0)
           ;; ((n-p-gp "pair" "hash" "assignment") grand-parent ruby-ts-mode-indent-offset)
           ;; ((n-p-gp "]" "array" "assignment") grand-parent 0)
           ;; ((n-p-gp ".*" "array" "assignment") grand-parent ruby-ts-mode-indent-offset)

(defun ruby-ts--indent-styles (_language)
  "Indent rules supported by `ruby-ts-mode'.
Currently LANGUAGE is ignored but should be set to `ruby'"
  (let ((common
         `(
           ;; Slam all top level nodes to the left margin
           ((parent-is "program") parent 0)

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
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((match "elsif" "if")
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((match "end" "if")
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((n-p-gp nil "then\\|else\\|elsif" "if\\|unless")
            (ruby-ts--align-keywords (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)

           ;; case expression: when, in_clause, and else are all
           ;; children of case.  when and in_clause have pattern and
           ;; body as fields.  body has "then" and then the statemets.
           ;; i.e. the statements are not children of when but then.
           ;; But for the statements are children of else.
           ((match "when" "case")
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((match "in_clause" "case")
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((match "else" "case")
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((match "end" "case")
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((n-p-gp nil "then" "when") grand-parent ruby-ts-mode-indent-offset)
           ((n-p-gp nil "then" "in_clause") grand-parent ruby-ts-mode-indent-offset)
           ((n-p-gp nil "else" "case") parent ruby-ts-mode-indent-offset)

           ;; The beauty of inconsistency :-)
           ;; while / until have only "do" as a child.  The "end" is a
           ;; child of "do".
           ((n-p-gp "end" "do" "while\\|until")
            (ruby-ts--align-keywords (ruby-ts--grand-parent-node)) 0)
           ((n-p-gp nil "do" "while\\|until")
            (ruby-ts--align-keywords (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)
            
           ;; begin can have rescue, ensure, else, and end.
           ;; statements are a child of begin.  rescue, ensure, else,
           ;; and end are also children of begin.  rescue has a then
           ;; as a child thus statements will be grand children of
           ;; rescue.
           ((n-p-gp nil "then" "rescue")
            (ruby-ts--align-keywords (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)
           ((n-p-gp nil "ensure\\|else" "begin")
            (ruby-ts--align-keywords (ruby-ts--parent-node)) ruby-ts-mode-indent-offset)
           ((match "rescue\\|ensure\\|else\\|end" "begin")
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((parent-is "begin")         ;last
            (ruby-ts--align-keywords (ruby-ts--parent-node)) ruby-ts-mode-indent-offset)            

           ;; for ... I don't think I have ever used a for loop in
           ;; Ruby.  The "in" (not an in_clause) and "do" are
           ;; children.  The statements are children of the "do".
           ;; And, of course, the "end" is a child of the "do".
           ((n-p-gp "end" "do" "for")
            (ruby-ts--align-keywords (ruby-ts--grand-parent-node)) 0)
           ((n-p-gp nil "do" "for")
            (ruby-ts--align-keywords (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)

           ;; method has a "body_statement" and the "end" as children.
           ;; The body_statement can have rescue, ensure, and else as
           ;; well as statements.  Note that the first statement of a
           ;; body_statement hits the node as "body_statement" and not
           ;; as the assignment, etc.
           ((match "end" ,ruby-ts--method-regex)
            (ruby-ts--align-keywords (ruby-ts--parent-node)) 0)
           ((n-p-gp "\\`\\(rescue\\|ensure\\|else\\)\\'" "body_statement" ,ruby-ts--method-regex)
            (ruby-ts--align-keywords (ruby-ts--grand-parent-node)) 0)
           ((n-p-gp nil "rescue\\|ensure\\|else" "body_statement") parent ruby-ts-mode-indent-offset)
           ((match "body_statement" ,ruby-ts--method-regex) ;first statement
            (ruby-ts--align-keywords (ruby-ts--parent-node)) ruby-ts-mode-indent-offset)
           ((n-p-gp nil "body_statement" ,ruby-ts--method-regex) ;other statements
            (ruby-ts--align-keywords (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)

           ;; Chained calls:
           ;; if `ruby-align-chained-calls' is true, the first query
           ;; matches and the node is aligned under the first dot (.);
           ;; else the second query aligns
           ;; `ruby-ts-mode-indent-offset' spaces in from the parent.
           ((and ruby-ts--align-chain-p (match "\\." "call")) ruby-ts--align-chain 0)
           ((match "\\." "call") parent ruby-ts-mode-indent-offset)

           ;; ruby-indent-after-block-in-continued-expression
           ((match "begin" "assignment") parent ruby-ts-mode-indent-offset)

           ;; method parameters -- four styles:
           ;; 1) With paren, first arg on same line:
           ((and (query "(method_parameters \"(\" _ @indent)")
                 ruby-ts--same-line-params-p
                 (node-is ")"))
            first-sibling 0)
           ((and (query "(method_parameters \"(\" _ @indent)")
                 ruby-ts--same-line-params-p)
            first-sibling 1)
           ;; ;; 2) With paren, first arg on next line, ruby-method-params-indent eq t
           ;; ;; 3) With paren, first arg on next line, ruby-method-params-indent neq t
           ((and (query "(method_parameters \"(\" _ @indent)") (node-is ")")) ruby-ts--param-indent 0)
           ((query "(method_parameters \"(\" _ @indent)") ruby-ts--param-indent ruby-ts-mode-indent-offset)
           ;; 4) No paren:
           ((parent-is "method_parameters") first-sibling 0)

           ;; Argument lists:
           ;; 1) With paren, 1st arg on same line
           ((and (query "(argument_list \"(\" _ @indent)")
                 ruby-ts--same-line-args-p
                 (node-is ")"))
            first-sibling 0)
           ((and (query "(argument_list \"(\" _ @indent)")
                 ruby-ts--same-line-args-p)
            first-sibling 1)
           ;; 2) With paren, 1st arg on next line
           ((and (query "(argument_list \"(\" _ @indent)")
                 (node-is ")"))
            grand-parent 0)
           ((query "(argument_list \"(\" _ @indent)")
            grand-parent ruby-ts-mode-indent-offset)
           ;; 3) No paren, ruby-parenless-call-arguments-indent is t
           ((and ruby-ts--parenless-call-arguments-indent-p (parent-is "argument_list"))
            first-sibling 0)
           ;; 4) No paren, ruby-parenless-call-arguments-indent is nil
           ((parent-is "argument_list") grand-parent ruby-ts-mode-indent-offset)

           ;; ((and (query "(argument_list \"(\" _ @indent)") (node-is ")")) first-sibling 0)
           ;; ((query "(argument_list \"(\" _ @indent)") first-sibling 1)
           ;; ((match ")" "argument_list") ruby-ts--argument-indent 0)
           ;; ((parent-is "argument_list") ruby-ts--argument-indent ruby-ts-mode-indent-offset)

           ;; Old -- probably remove.
           ;; ((parent-is "argument_list") first-sibling 0)

           ;; Old... probably too simple
           ((parent-is "block_parameters") first-sibling 1)

           ;; Changed for ruby-align-to-stmt-keywords-t
           ((parent-is "binary") first-sibling ruby-ts-mode-indent-offset)

           ;; ruby-mode does not touch these...
           ((match "bare_string" "string_array") no-indent 0)

           ;; hash and array other than assignments.  Note that the
           ;; first sibling is the "{" or "[".  There is a case where
           ;; the hash is an argument to a method.  These need to be
           ;; processed first.
           ((n-p-gp "}" "hash" "argument_list") first-sibling 0)
           ((n-p-gp nil "hash" "argument_list") first-sibling ruby-ts-mode-indent-offset)
           ((parent-is "hash") ruby-ts--hash-array-indent 0)
           ((node-is "}") ruby-ts--hash-array-indent 0)
           ((parent-is "array") ruby-ts--hash-array-indent 0)
           ((node-is "]") ruby-ts--hash-array-indent 0)

           ;; If the previous method isn't finished yet, this will get
           ;; the next method indented properly.
           ((n-p-gp ,ruby-ts--method-regex "body_statement" ,ruby-ts--class-or-module-regex)
            (ruby-ts--bol (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)

           ;; Match the end of a class / modlue
           ((match "end" ,ruby-ts--class-or-module-regex) parent 0)

           ;; Do not indent here docs or the end.  Not sure why it
           ;; takes the grand-parent but ok fine.
           ((n-p-gp nil nil "heredoc_body") no-indent 0)
           ((parent-is "heredoc_body") no-indent 0)
           ((node-is "heredoc_body") no-indent 0)

           ;; a "do_block" has a "body_statement" child which has the
           ;; statements as children within it.  The problem is that
           ;; the first statement starts at the same point as the
           ;; body_statement and so treesit-simple-indent is called
           ;; with node set to body_statement on the first statement
           ;; but with node set to the statement and parent set to
           ;; body_statement for all others. ... Fine.  Be that way.
           ;; Ditto for "block" and "block_body"
           ((node-is "body_statement") parent-bol ruby-ts-mode-indent-offset)
           ((parent-is "body_statement") (ruby-ts--bol (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)
           ((match "end" "do_block") parent-bol 0)
           ((n-p-gp "block_body" "block" nil) parent-bol ruby-ts-mode-indent-offset)
           ((n-p-gp nil "block_body" "block") (ruby-ts--bol (ruby-ts--grand-parent-node)) ruby-ts-mode-indent-offset)
           ((match "}" "block") (ruby-ts--bol (ruby-ts--grand-parent-node)) 0)

           ;; Try and indent two spaces when all else fails.
           (catch-all parent-bol ruby-ts-mode-indent-offset))))
    `((base ,@common))))

(defun ruby-ts--class-or-module-p (node)
  "Predicate if NODE is a class or module."
  (string-match-p ruby-ts--class-or-module-regex (treesit-node-type node)))

(defun ruby-ts--get-name (node)
  "Return the text of the `name' field of NODE."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun ruby-ts--full-name (node)
  "Return the fully qualified name of NODE."
  (let* ((name (ruby-ts--get-name node))
         (delimiter "#"))
    (while (setq node (treesit-parent-until node #'ruby-ts--class-or-module-p))
      (setq name (concat (ruby-ts--get-name node) delimiter name))
      (setq delimiter "::"))
    name))

(defun ruby-ts--imenu-helper (node)
  "Convert a treesit sparse tree NODE in an imenu list.
Helper for `ruby-ts--imenu' which converts a treesit sparse
NODE into a list of imenu ( name . pos ) nodes"
  (let* ((ts-node (car node))
         (subtrees (mapcan #'ruby-ts--imenu-helper (cdr node)))
         (name (when ts-node
                 (ruby-ts--full-name ts-node)))
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
(defun ruby-ts--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((root (treesit-buffer-root-node))
         (nodes (treesit-induce-sparse-tree root "^\\(method\\|alias\\|class\\|module\\)$")))
    (ruby-ts--imenu-helper nodes)))

(defun ruby-ts--set-indent-style (language)
  "Helper function to set the indentation style.
Currently LANGUAGE is ignored but should be set to `ruby'."
  (let ((style
         (if (functionp ruby-ts-mode-indent-style)
             (funcall ruby-ts-mode-indent-style)
           (pcase ruby-ts-mode-indent-style
             ('base (alist-get 'base (ruby-ts--indent-styles language)))))))
    `((,language ,@style))))

(defun ruby-ts--arrow-up-start (arg)
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

(defun ruby-ts--class-name (node)
  "Return NODE's name.
Assumes NODE's type is \"class\" or \"method\""
  (list
   (treesit-node-text
    (treesit-node-child-by-field-name
     node
     (if (equal "singleton_class" (treesit-node-type node)) "value" "name"))
    t)))
  
(defun ruby-ts--method-name (node)
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

(defun ruby-ts--log-current-function ()
  "Return the current method name as a string.
The hash (#) is for instance methods only which are methods
\"defined on a class\" -- which is 99% of methods.  Otherwise, a
dot (.) is used.  Double colon (::) is used between classes.  The
leading double colon is not added."
  (let* ((node (treesit-node-at (point)))
         (method (treesit-parent-until node (ruby-ts--type-pred ruby-ts--method-regex)))
         (class (or method node))
         (result nil)
         (sep "#")
         (method-list nil)
         (class-list nil)
         (method-name nil))

    (when method
      (setq method-list (ruby-ts--method-name method))
      (unless (= 1 (length method-list))
        (setq sep ".")))
    (while (setq class (treesit-parent-until class
                                             (ruby-ts--type-pred
                                              ruby-ts--class-or-module-regex)))
      (setq class-list (append (ruby-ts--class-name class) class-list)))
    (setq method-name (car (last method-list))
          method-list (butlast method-list))
    (when (equal (car method-list) (car (last class-list)))
      (setq method-list (cdr method-list)))
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

(defvar-keymap ruby-ts--arrow-keys
  :doc "Transient keymap for arrow keys"
  ;; "<right>" #'ruby-ts-forward-argument-start
  "<right>" #'ruby-ts--forward-statement-start
  "<up>"    #'ruby-ts--arrow-up-start
  )
  
(defvar-keymap ruby-ts-mode-map
  :doc "Keymap used in Ruby mode"
  :parent prog-mode-map
  "C-M-h"     #'ruby-ts--mark-method
  "H-<right>"  #'ruby-ts--mark-statement
  "s-<right>"  #'ruby-ts--forward-method
  "M-<left>"  #'ruby-ts--raw-prev-sibling
  "M-<right>" #'ruby-ts--raw-next-sibling
  "C-c" ruby-ts--arrow-keys)

(define-derived-mode ruby-ts-base-mode prog-mode "Ruby"
  "Major mode for editing Ruby, powered by tree-sitter."
  :syntax-table ruby-ts--syntax-table

  ;; Navigation.
  (setq-local treesit-defun-type-regexp ruby-ts--method-regex)

  ;; AFAIK, Ruby can not nest methods
  (setq-local treesit-defun-prefer-top-level nil)

  ;; Imenu.
  (setq-local imenu-create-index-function #'ruby-ts--imenu)

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

  (setq-local add-log-current-defun-function #'ruby-ts--log-current-function)

  ;; Comments.
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")

  (setq-local treesit-simple-indent-rules (ruby-ts--set-indent-style 'ruby))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (ruby-ts--font-lock-settings 'ruby))

  (treesit-major-mode-setup))

(provide 'ruby-ts-mode)

;;; ruby-ts-mode.el ends here
