(define-module (gnucash report hello-world))
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(debug-enable 'debug)
(debug-enable 'backtrace)
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0)

(define d:report
  (lambda (name defs-list title subtitle reportbuilders)
    (let ((options-generator
           (lambda ()
             (let ((options (gnc:new-options)))
               options)))
          (renderer
           (lambda (options)
             (let ((document (gnc:make-html-document)))
               (gnc:html-document-set-title! document (_ title))
               (gnc:html-document-add-object!
                document
                (gnc:make-html-text
                 (gnc:html-markup-h3
                  (gnc:html-markup/format
                   (_ subtitle)))))
               (map
                (lambda (reportbuilder)
                  (reportbuilder document options))
                reportbuilders)
               document))))
      (gnc:define-report
       'version 1
       'name (N_ title)
       'report-guid "898d78ec92854402bf76e20a36d24ade"
       'options-generator options-generator
       'renderer renderer))))

(define d:filter-none
  (lambda reportbuilders reportbuilders))

(define d:p
  (lambda (text)
    (lambda (document options)
      (gnc:html-document-add-object!
       document
       (gnc:make-html-text
        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ text))))))))

(d:report
 "income-statement" ; name
 0 ; defs
 ; Have to keep this title while experimenting in the sample report that
 ; comes with GnuCash
 "Hello, World" ; title
 "2011-01-01 to 2011-07-31" ; subtitle
 (d:filter-none ; body
  (d:p "Some text.")
  (d:p "A little more text.")))
