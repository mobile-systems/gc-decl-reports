(define-module (gnucash report hello-world))
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(debug-enable 'debug)
(debug-enable 'backtrace)
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0)

(define d:report
  (lambda (name defs-list title subtitle filter)
    (let ((options-generator
           (lambda ()
             (gnc:new-options)))
          (renderer
           (lambda (options)
             (gnc:make-html-document))))
      (gnc:define-report
       'version 1
       'name (N_ title)
       'report-guid "898d78ec92854402bf76e20a36d24ade"
       'options-generator options-generator
       'renderer renderer))))

(define d:filter-none
  (lambda () ()))

(d:report "income-statement" 0 "Hello, World" "2011-01-01 to 2011-07-31" d:filter-none)
