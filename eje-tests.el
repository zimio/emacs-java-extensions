;; Example exanded env
;; ((yas-indent-line 'fixed) (yas-java-test-methods '("void testa()" "void testb()" "void testc()")))

;; Only works in java-ts-mode

;; TODO Create a yasnippet for the tests
;; use yas-expand-snippet

(defun eje-create-test-for-class ()
  (interactive)
  (let* ((class-name (eje--get-class-name))
         (package (eje--get-package))
         ;;(test-path (eje--find-test-path class-name (eje--get-package)))
         (test-path "/home/jlagrang/pruba.java")
         (snippet (yas-lookup-snippet "unittest" 'java-ts-mode))
         (public-methods (eje--get-public-methods))
         )
    ;; create file - or just switch to buffer without saving
    ;; switch to buffer
    (if (eje--test-class? class-name)
        (error "Attempting to create test for a test")
      (progn (switch-to-buffer (find-file-noselect test-path))
             (yas-expand-snippet snippet
                                 (point-min)
                                 (point-max)
                                 '( (yas-indent-line 'fixed)
                                    (yas-java-test-methods public-methods)
                                    (yas-java-package package)
                                    (yas-java-class class-name)))))
    )
  )

;; TODO
(defun eje--find-test-path (class-name package)
  "Given a string representing a java class-name package, return the path for testing that class."
  )

(defun eje--get-public-methods  ()
  (let* ((root-node (treesit-buffer-root-node))
         (query '((program (class_declaration  (class_body (method_declaration (modifiers "public") @modifier_node (identifier) @method_name) )))))
         (query-result (treesit-query-capture root-node query)))
    (cl-remove-duplicates
      (cl-map 'list
              'treesit-node-text
              (eje--get-nodes 'method_name query-result))
              :test 'string=)))


(defun eje--get-nodes (node-type query-result)
  "Returns a list of nodes given a node-type and a query-result"
  (cl-map 'list (lambda (x) (cdr x))
                (cl-remove-if-not (lambda (x) (eq (car x) node-type))
                                  query-result)))

(defun eje--get-package ()
  "Get the package name from a Java class in the current buffer."
  (eje--get-node-text '((program (package_declaration (scoped_identifier) @x)))))

(defun eje--get-class-name ()
  "Get the class name from a Java class declaration in the current buffer using Tree-sitter."
  (eje--get-node-text '((program (class_declaration (identifier) @x)))))

(defun eje--get-node-text (query)
  (let* ((root-node (treesit-buffer-root-node))
         (query-result (treesit-query-capture root-node query)))
    (treesit-node-text (cdr (car query-result)))))

(defun eje--test-class? (class-name)
  "Return t if current buffer is a test class"
  (or (eje--has-test-in-string? buffer-file-name) (eje--has-test-in-string? class-name)))

(defun eje--has-test-in-string? (s)
  "Return t if string contains test."
  (when (cl-search "test" (downcase s)) t))
