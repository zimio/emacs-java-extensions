
;; Only works in java-ts-mode

(defun eje-create-test-for-class ()
  (interactive)
  (let* ((class-name (eje--get-class-name))
         (package (eje--get-package))
         (test-path (eje--find-test-path class-name (eje--get-package)))
         (snippet (yas-lookup-snippet "unittest" 'java-ts-mode))
         (public-methods (eje--get-public-methods)))
    ;; create file - or just switch to buffer without saving
    ;; switch to buffer
    (if (eje--test-class? class-name)
        (error "Attempting to create test for a test")
      (progn (switch-to-buffer (find-file-noselect test-path))
             (yas-expand-snippet snippet
                                 (point-min)
                                 (point-max)
                                 '((yas-indent-line 'fixed)
                                   (yas-java-test-methods public-methods)
                                   (yas-java-package package)
                                   (yas-java-class class-name)))))))

(defun eje-go-to-test-class ()
  (interactive)
  (let* ((class-name (eje--get-class-name))
         (package (eje--get-package))
         (test-path (eje--find-test-path class-name (eje--get-package))))
    (find-file (eje--add-wildcard-path test-path) t)))

(defun eje-run-test-file ()
  (interactive)
  (eje--projectile-test-project (eje--get-class-name)))

(defun eje-run-test-method ()
  (interactive)
  (let ((method (eje--get-current-method)))
    (when method
      (eje--projectile-test-project (concat (eje--get-class-name)
                                            "#"
                                            method)))))

(defun eje--get-current-method ()
  (eje--get-node-text '((method_declaration (identifier) @x))
                      (treesit-defun-at-point)))

(defun eje--projectile-test-project (test-arg)
  (let ((command (projectile-test-command (projectile-compilation-dir)))
        (command-map (if (projectile--cache-project-commands-p) projectile-test-cmd-map)))
    (when (cl-search "mvn" command)
      (projectile--run-project-cmd (concat command " -Dtest=" test-arg)
                                   command-map
                                   :show-prompt nil
                                   :prompt-prefix "Test command: "
                                   :save-buffers t
                                   :use-comint-mode projectile-test-use-comint-mode))))

(defun eje--add-wildcard-path (test-path)
  (let ((split-path (split-string test-path "/")))
    (concat
     (string-join (butlast split-path) "/")
     "/*"
     (car (split-string (car (last split-path)) "\\."))
     "*"
     ".java")))

(defun eje--find-test-path (class-name package)
  "Given a string representing a java class-name package, return the path for testing that class."
  (if (cl-search "src" buffer-file-name)
      (concat (car (split-string buffer-file-name "src"))
              "src/test/java/"
              (subst-char-in-string ?. ?/ package)
              "/"
              class-name
              ".java")
    (error "Couldn't find source path")))

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
  (eje--get-node-text '((program (package_declaration (scoped_identifier) @x)))
                      (treesit-buffer-root-node)))

(defun eje--get-class-name ()
  "Get the class name from a Java class declaration in the current buffer using Tree-sitter."
  (eje--get-node-text '((program (class_declaration (identifier) @x)))
                      (treesit-buffer-root-node)))

(defun eje--get-node-text (query node)
  (let ((query-result (treesit-query-capture node query)))
    (treesit-node-text (cdr (car query-result)))))

(defun eje--test-class? (class-name)
  "Return t if current buffer is a test class"
  (or (eje--has-test-in-string? buffer-file-name) (eje--has-test-in-string? class-name)))

(defun eje--has-test-in-string? (s)
  "Return t if string contains test."
  (when (cl-search "src/test" (downcase s)) t))


(provide 'emacs-java-extensions)
