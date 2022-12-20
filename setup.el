(deftheme test "Theme used for testing")

(custom-theme-set-faces
 'test
 '(font-lock-operator-face ((t (:foreground "PaleGreen1"))))
 '(font-lock-bracket-face ((t (:foreground "LightGreen"))))
 '(font-lock-builtin-face ((t (:foreground "MediumPurple1"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "magenta"))))
 '(font-lock-comment-face ((t (:foreground "pink"))))
 '(font-lock-constant-face ((t (:foreground "orange1"))))
 '(font-lock-delimiter-face ((t (:foreground "RosyBrown1"))))
 '(font-lock-doc-face ((t (:foreground "yellow1"))))
 '(font-lock-doc-markup-face ((t (:foreground "PaleGreen1"))))
 '(font-lock-escape-face ((t (:foreground "LightCyan1"))))
 '(font-lock-function-name-face ((t (:foreground "azure1"))))
 '(font-lock-keyword-face ((t (:foreground "purple"))))
 '(font-lock-misc-punctuation-face ((t (:foreground "LightSalmon"))))
 '(font-lock-negation-char-face ((t (:foreground "LightGoldenrodYellow"))))
 '(font-lock-number-face ((t (:foreground "MediumTurquoise"))))
 '(font-lock-operator-face ((t (:foreground "PaleGreen"))))
 '(font-lock-preprocessor-face ((t (:foreground "LightSlateBlue"))))
 '(font-lock-property-face ((t (:foreground "VioletRed1"))))
 '(font-lock-punctuation-face ((t (:foreground "chocolate1"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "LightGoldenrod1"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "RoyalBlue1"))))
 '(font-lock-string-face ((t (:foreground "SlateBlue1"))))
 '(font-lock-type-face ((t (:foreground "DarkOrchid"))))
 '(font-lock-variable-name-face ((t (:foreground "MediumVioletRed"))))
 '(font-lock-warning-face ((t (:foreground "LightCoral")))))

;; (defface font-lock-bracket-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightGreen")) "Doc")
;; (defface font-lock-builtin-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "MediumPurple1")) "Doc")
;; (defface font-lock-comment-delimiter-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "plum1")) "Doc")
;; (defface font-lock-comment-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightPink1")) "Doc")
;; (defface font-lock-constant-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "orange1")) "Doc")
;; (defface font-lock-delimiter-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "RosyBrown1")) "Doc")
;; (defface font-lock-doc-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "yellow1")) "Doc")
;; (defface font-lock-doc-markup-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "PaleGreen1")) "Doc")
;; (defface font-lock-escape-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightCyan1")) "Doc")
;; (defface font-lock-function-name-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "azure1")) "Doc")
;; (defface font-lock-keyword-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightPink")) "Doc")
;; (defface font-lock-misc-punctuation-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightSalmon")) "Doc")
;; (defface font-lock-negation-char-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightGoldenrodYellow")) "Doc")
;; (defface font-lock-number-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "MediumTurquoise")) "Doc")
;; (defface font-lock-operator-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "PaleGreen")) "Doc")
;; (defface font-lock-preprocessor-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightSlateBlue")) "Doc")
;; (defface font-lock-property-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "VioletRed1")) "Doc")
;; (defface font-lock-punctuation-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "chocolate1")) "Doc")
;; (defface font-lock-regexp-grouping-backslash
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightGoldenrod1")) "Doc")
;; (defface font-lock-regexp-grouping-construct
;;   '((((class color) (min-colors 88) (background dark)) :foreground "RoyalBlue1")) "Doc")
;; (defface font-lock-string-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "SlateBlue1")) "Doc")
;; (defface font-lock-type-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "DarkOrchid")) "Doc")
;; (defface font-lock-variable-name-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "MediumVioletRed")) "Doc")
;; (defface font-lock-warning-face
;;   '((((class color) (min-colors 88) (background dark)) :foreground "LightCoral")) "Doc")

  
;; (defface font-lock-operator-face
;;   '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
;;     (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
;;     (((class color) (min-colors 88) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 88) (background dark))  :foreground "Orange")
;;     (((class color) (min-colors 16) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
;;     (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
;;     (t :weight bold))
;;   "Font Lock mode face used to highlight keywords."
;;   :group 'font-lock-faces)


;; (defface font-lock-misc-punctuation-face
;;   '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
;;     (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
;;     (((class color) (min-colors 88) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 88) (background dark))  :foreground "Blue")
;;     (((class color) (min-colors 16) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
;;     (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
;;     (t :weight bold))
;;   "Font Lock mode face used to highlight keywords."
;;   :group 'font-lock-faces)

;; (defface font-lock-number-face
;;   '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
;;     (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
;;     (((class color) (min-colors 88) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 88) (background dark))  :foreground "LightSkyBlue")
;;     (((class color) (min-colors 16) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
;;     (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
;;     (t :weight bold))
;;   "Font Lock mode face used to highlight keywords."
;;   :group 'font-lock-faces)

;; (defface font-lock-bracket-face
;;   '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
;;     (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
;;     (((class color) (min-colors 88) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 88) (background dark))  :foreground "Pink")
;;     (((class color) (min-colors 16) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
;;     (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
;;     (t :weight bold))
;;   "Font Lock mode face used to highlight keywords."
;;   :group 'font-lock-faces)

;; (defface font-lock-negation-char-face
;; '((((class grayscale) (background light)) :foreground "LightGray" :weight bold)
;;     (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
;;     (((class color) (min-colors 88) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 88) (background dark))  :foreground "Purple")
;;     (((class color) (min-colors 16) (background light)) :foreground "Purple")
;;     (((class color) (min-colors 16) (background dark))  :foreground "Cyan")
;;     (((class color) (min-colors 8)) :foreground "cyan" :weight bold)
;;     (t :weight bold))
;;   "Font Lock mode face used to highlight keywords."
;;   :group 'font-lock-faces)

(defun redo ()
  (interactive)
  (load "/Users/pedz/Source/ruby-ts-mode/ruby-ts-mode.el")
  ;; (ruby-ts-mode)
  (setq-local treesit-simple-indent-rules (ruby-ts-mode--set-indent-style 'ruby))
  (setq-local treesit-font-lock-settings (ruby-ts-mode--font-lock-settings 'ruby))
  (setq-local treesit-font-lock-level 4)
  (treesit-font-lock-recompute-features)
  (treesit-inspect-mode))

;; (treesit-query-validate 'ruby ruby-ts-mode--operators)
(global-set-key (kbd "H-c") #'describe-char)

(defun set-treesit-font-lock-level-4 ()
  "Sets treesit-font-lock-level to 4."
  (setq-local treesit-font-lock-level 4)
  (treesit-font-lock-recompute-features))

(add-hook 'ruby-ts-mode-hook #'set-treesit-font-lock-level-4)
