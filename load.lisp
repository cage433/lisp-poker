(load "package.lisp")
(in-package :cage433-lisp-poker)

(dbind (x y) '(3 4 ) (cons x y))

(proclaim '(optimize (debug 3)))
(defun load-and-compile-source()
  (format t "Loading source ~%")
    (and
      (cage433-ci:load-and-compile-if-necessary "package")
      (cage433-ci:load-and-compile-if-necessary "cards")
      (cage433-ci:load-and-compile-if-necessary "test-cards")
      (cage433-ci:load-and-compile-if-necessary "tests")
      ))

(defun compile-and-run-tests()
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
    (if (load-and-compile-source)

      (run-tests (info "poker-tests"
          (hand-analysis-suite)))
        ))

(in-package :common-lisp-user)
(defun ci()
  (cage433-ci:run-ci-function #'cage433-lisp-poker::compile-and-run-tests)
  )
