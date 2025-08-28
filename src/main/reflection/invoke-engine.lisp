;; github-file-path: https://github.com/Doctor0Evil/ALN_Programming_Language/blob/main/reflection/invoke-engine.lisp

(defun load-aln-engine (dll-path type-name method-name &optional args)
  "Load the ALN Engine DLL, resolve a type, and invoke a method."
  (let* ((assembly (load-assembly dll-path))
         (type     (assembly-get-type assembly type-name))
         (method   (type-get-method type method-name))
         (instance (if (static-method-p method)
                       nil
                       (create-instance type))))
    (invoke-method method instance args)))

(defun enumerate-engine-types (dll-path)
  "Enumerates public types and their methods from the ALN Engine DLL."
  (let ((assembly (load-assembly dll-path)))
    (mapcar (lambda (t)
              (list :class (type-full-name t)
                    :methods (mapcar #'method-name
                                     (type-public-methods t))))
            (assembly-public-types assembly))))

(defun quick-static-run (dll-path type-name method-name &optional args)
  "Shortcut for calling a static entry point method quickly."
  (add-type dll-path)
  (static-invoke type-name method-name args))
