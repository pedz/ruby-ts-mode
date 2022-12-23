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
;; (add-to-list 'auto-mode-alist (cons "\\.rb\\'" #'ruby-ts-mode))
(add-to-list 'auto-mode-alist
             (cons (purecopy (concat "\\(?:\\.\\(?:"
                                     "rbw?\\|ru\\|rake\\|thor"
                                     "\\|jbuilder\\|rabl\\|gemspec\\|podspec"
                                     "\\)"
                                     "\\|/"
                                     "\\(?:Gem\\|Rake\\|Cap\\|Thor"
                                     "\\|Puppet\\|Berks\\|Brew"
                                     "\\|Vagrant\\|Guard\\|Pod\\)file"
                                     "\\)\\'"))
                   'ruby-ts-mode))
(dolist (name (list "ruby" "rbx" "jruby" "ruby1.9" "ruby1.8"))
  (add-to-list 'interpreter-mode-alist (cons (purecopy name) 'ruby-ts-mode)))
