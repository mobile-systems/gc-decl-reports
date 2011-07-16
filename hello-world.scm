; Declarative reports for GnuCash

; Some functionality copied from:
;
; * The LAML library and programs written by Kurt Normark, Aalborg
; University, Denmark.
; Copyright (C) 1999-2009  Kurt Normark, normark@cs.aau.dk.
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(define-module (gnucash report hello-world))
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(debug-enable 'debug)
(debug-enable 'backtrace)
(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/html" 0)

; take - Take some items from a list
; Arguments:
;  n - The number of items to take.
;
;  lst - The list to take from.
; Returns:
;  A list of the first n items from lst. If n is 0 or lst is an empty list,
;  returns an empty list.
(define (take n lst)
  (cond ((= n 0) '())
        ((null? lst) '())
        (#t (cons (car lst) (take (- n 1) (cdr lst))))))

; split-point - See
; http://www.cs.aau.dk/~normark/scheme/lib/man/general.html#split-point
(define (split-point ch str)
 (call-with-current-continuation
  (lambda (exit)
   (cond ((equal? str "") #f)
         ((eqv? ch (string-ref str 0)) 0)
         (else (let ((res (split-point ch (substring str 1 (string-length str)))))
                  (if (not res)
                      (exit #f)
                      (+ 1 res))))))))

; split-on - See
; http://www.cs.aau.dk/~normark/scheme/lib/man/general.html#split-on
(define (split-on ch str)
 (let ((sp (split-point ch str)))
   (list (substring str 0 sp)
         (substring str (+ sp 1) (string-length str)))))

; d:report - Create a declarative report
; Arguments:
;  guid - The GUID of the report.
;
;  menuitem - The name for the report that will appear in the GnuCash menu.
;
;  defs - A list of definitions (defs). A def is like an object that is
;  accessible throughout the body of the report, and also doubles as
;  options in the report's options dialog box.
;
;  reportbuilders - A list of actions (called reportbuilders) which carry
;  out the tasks needed to actually gather the data and generate the HTML
;  on the report page.
; Returns:
;  The generated report.
(define d:report
  (lambda (guid menuitem defs . reportbuilders)
    (let ((options-generator
           (lambda ()
             (let ((opts (gnc:new-options)))
               (map
                (lambda (def) (def opts))
                defs)
               opts)))

          (renderer
           (lambda (report-obj)
             (let ((document (gnc:make-html-document)))
               (map
                (lambda (reportbuilder)
                  (reportbuilder
                   document
                   (gnc:report-options report-obj)))
                reportbuilders)
               document))))

      (gnc:define-report
       'version 1
       'name (N_ menuitem)
       'report-guid guid
       'options-generator options-generator
       'renderer renderer))))

; d:defs - Tells options defs where they will be placed on the report
; options dialog box.
; Arguments:
;  defs - A list of defs like date defs, account defs, etc.
; Returns:
;  A list of definition function which now know where they will be placed
;  in the options dialog box, but need to be called with the report options
;  object as their argument (this will happen in the d:report function
;  where they are fed in as the defs argument).
(define d:defs
  (lambda defs
    (map
     (lambda (def pos) (def pos))
     defs
     (take
      (length defs)
      '("ca" "cb" "cc" "cd" "ce" "cf" "cg" "ch" "ci" "cj" "ck" "cl" "cm"
        "cn" "co" "cp" "cq" "cr" "cs" "ct" "cu" "cv" "cx" "cy" "cz" "da"
        "db" "dc" "de" "df" "dg" "dh" "di" "dj" "dk" "dl" "dm" "dn" "do"
        "dp" "dq" "dr" "ds" "dt" "du" "dv" "dw" "dx" "dy" "dz")))))

; d:def-label - A label definition
; Arguments:
;  page - The name of the page (tab) in the report options dialog box on
;  which this option will appear.
;
;  name - The name of the option that will appear on the report options
;  dialog box.
;
;  val - The value being given to the option.
; Returns:
;  A label definition function that takes a positioning word as its
;  argument.
(define d:def-label
  (lambda (page name val)
    (lambda (pos)
      (lambda (opts)
        (gnc:register-option
         opts
         (gnc:make-string-option page name pos name val))))))

; d:def-date - A date definition
; Arguments:
;  page - The name of the page (tab) in the report options dialog box on
;  which this option will appear.
;
;  name - The name of the option that will appear on the report options
;  dialog box.
;
;  val - The value being given to the option, in the form of a string like
;  "2011-12-31".
; Returns:
;  A date definition function that takes a positioning word as its argument.
(define d:def-date
  (lambda (page name val)
    (lambda (pos)
      (lambda (opts)
        (gnc:register-option
         opts
         (gnc:make-date-option
          page
          name
          pos
          name
          (lambda ()
            (cons
             'absolute
             (cons (car (mktime (car (strptime "%Y-%m-%d" val)))) 0)))
          #f
          'absolute #f))))))

; di:get-option-value - A generic option getter helper function
; Arguments:
;  opts - The options object.
;
;  page - The page (tab) in the options dialog box on which the option is
;  located.
;
;  name - The name of the option.
; Returns:
;  The value of the option.
(define di:get-option-value
  (lambda (opts page name)
    (gnc:option-value
     (gnc:lookup-option opts page name))))

; d:label - A getter for the value of a label def
; Arguments:
;  page - The page (tab) in the options dialog box on which the option is
;  located.
;
;  name - The name of the option.
; Returns:
;  A reportbuilder (function) that gets the label value.
(define d:label
  (lambda (page name)
    (lambda (document opts)
      (di:get-option-value opts page name))))

; di:put-text - A helper function add text markup to the report's HTML
; document
; Arguments:
;  text - The text to add to the document.
;
;  markup-func - The function for the kind of markup to add
;  (e.g. gnc:html-markup-h3)
;
;  document - The HTML document to add to.
;
;  opts - The report options object.
; Returns:
;  Nothing.
(define di:put-text
  (lambda (text markup-func document opts)
    (gnc:html-document-add-object!
     document
     (gnc:make-html-text
      (markup-func
       (_ (if (procedure? text) (text document opts) text)))))))

; d:p - Generate a paragraph reportbuilder
; Arguments:
;  text - The text to put in the paragraph.
; Returns:
;  A function that, when called with the documents and options objects,
;  adds an HTML paragraph to the report page.
(define d:p
  (lambda (text)
    (lambda (document opts)
      (di:put-text text gnc:html-markup-p document opts))))

; d:title - Set the report title visible on the page
; Arguments:
;  text - A string or D:LABEL value that contains the title to set.
; Returns:
;  A reportbuilder that takes the report document and options objects and
;  sets the document title on the page.
(define d:title
  (lambda (text)
    (lambda (document opts)
      (gnc:html-document-set-title!
       document
       (_ (if (procedure? text) (text document opts) text))))))

; d:subtitle - Set the report title visible on the page
; Arguments:
;  text - A string or D:LABEL value that contains the subtitle to set.
; Returns:
;  A reportbuilder that takes the report document and options objects and
;  sets the document subtitle on the page.
(define d:subtitle
  (lambda (text)
    (lambda (document opts)
      (di:put-text text gnc:html-markup-h3 document opts))))

(d:report
 "decafbad"
 "Declarative Report"
 (d:defs
   (d:def-label "General" "Report title" "Declarative Report")
   (d:def-label "General" "Report subtitle" "2011-01-01 to 2011-07-31")
   (d:def-date "General" "Start date" "2011-01-01")
   (d:def-date "General" "End date" "2011-07-31"))

 (d:title (d:label "General" "Report title"))
 (d:subtitle (d:label "General" "Report subtitle"))
 (d:p "Some text.")
 (d:p "A little more text."))
