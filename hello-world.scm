; Declarative reports for GnuCash
(define-module (gnucash report hello-world))
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(debug-enable 'debug)
(debug-enable 'backtrace)
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0)

; d:report - Create a declarative report
; Arguments:
;  name - The name of the report. I don't really have a use for it yet, but
;  am just trying to get names for all declared objects for future use.
;
;  defs - A list of definitions (defs). A def is like an object that is
;  accessible throughout the body of the report, and also doubles as
;  options in the report's options dialog box.
;
;  title - The title of the report. This should appear in the Reports menu
;  and on the top of the report page.
;
;  subtitle - The report subtitle. This should appear right under the
;  report title.
;
;  reportbuilders - A list of actions (called reportbuilders) which carry
;  out the tasks needed to actually gather the data and generate the HTML
;  on the report page.
; Returns:
;  The generated report.
(define d:report
  (lambda (name defs title subtitle reportbuilders)
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

; d:filter-none - A top-level filter which doesn't filter any of the data
; Arguments:
;  reportbuilders - A list of reportbuilders.
; Returns:
;  reportbuilders.
(define d:filter-none
  (lambda reportbuilders reportbuilders))

; d:p - Generate a paragraph reportbuilder
; Arguments:
;  text - The text to put in the paragraph.
; Returns:
;  A function that, when called with the documents and options objects,
;  adds an HTML paragraph to the report page.
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
