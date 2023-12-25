

;; Only works in java-ts-mode

;; TODO Create a yasnippet for the tests
;; use yas-expand-snippet

(defun eje-create-test-for-class ()
  (let* ((class-name (eje--get-class-name))
         (test-path (eje--find-test-path class-name (eje--get-package))))
    ;; create file - or just switch to buffer without saving
    ;; switch to buffer
    (if (eje--test-class? class-name)
        (error "Attempting to create test for a test")
      (progn (switch-to-buffer (find-file-noselect test-path))))
    )
  )

;; TODO
(defun eje--find-test-path (class-name package)
  "Given a string representing a java class-name package, return the path for testing that class."
  )

(defun eje--get-package ()
  "Get the package name from a Java class in the current buffer."
  (eje--get-node-text '((program (package_declaration (scoped_identifier) @x)))))

(defun eje--get-class-name ()
  "Get the class name from a Java class declaration in the current buffer using Tree-sitter."
  (eje--get-node-text '((program (class_declaration (identifier) @x)))))

(defun eje--get-node-text (query)
  (let* ((root-node (treesit-buffer-root-node))
         (query-value (treesit-query-capture root-node query)))
    (treesit-node-text (cdr (car query-value)))))

(defun eje--test-class? (class-name)
  "Return t if current buffer is a test class"
  (or (eje--has-test-in-string? buffer-file-name) (eje--has-test-in-string? class-name)))

(defun eje--has-test-in-string? (s)
  "Return t if string contains test."
  (when (cl-search "test" (downcase s)) t))
