(load "package.lisp")
(in-package :cage433-lisp-poker)

(proclaim '(optimize (debug 3)))
(defun load-and-compile-source()
  (format t "Loading source ~%")
    (and
      (cage433-ci:load-and-compile-if-necessary "package")
      (cage433-ci:load-and-compile-if-necessary "cards")
      (cage433-ci:load-and-compile-if-necessary "hand-analysis")
      (cage433-ci:load-and-compile-if-necessary "tests/generators")
      (cage433-ci:load-and-compile-if-necessary "deck")
      (cage433-ci:load-and-compile-if-necessary "tests/test-hand-analysis")
      (cage433-ci:load-and-compile-if-necessary "tests/deck-tests")
      ))

(defun compile-and-run-tests()
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
    (if (load-and-compile-source)

      (run-tests (info "poker-tests"
          (hand-analysis-suite)
          (shuffle-test-suite)
          (hand-frequency-suite)
          ))))

(in-package :common-lisp-user)
(defun ci()
  (cage433-ci:run-ci-function #'cage433-lisp-poker::compile-and-run-tests)
  )
