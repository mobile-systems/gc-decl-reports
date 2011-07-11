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
               (map
                (lambda (def) (def options))
                defs)
               options)))
          (renderer
           (lambda (options)
             (let ((document (gnc:make-html-document)))
               (gnc:html-document-set-title! document (_ title))
               (gnc:html-document-add-object!
                document
                (gnc:make-html-text
                 (gnc:html-markup-h3 (_ subtitle))))
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

; d:def-date - A date definition
; Arguments:
;  name - The name of this date def
;
;  title - The title of this date def in the form of "Pagename/Widgetname"
;  where Pagename is the page (tab) in the options dialog box where the def
;  will be placed in the form of an option, and Widgetname is the name the
;  option will be given on the dialog box.
;
;  date-str - A date string in the form 2011-12-31.
; Returns:
;  A date definition function that takes a positioning character "a"-"z" as
;  its argument.
(define d:def-date
  (lambda (name title date-str)
    (lambda (pos)
      (lambda (options)
        (let* ((name-split (split-on #\/ title))
               (pagename (N_ (car name-split)))
               (widgetname (N_ (cadr name-split))))
          (gnc:register-option
           options
           (gnc:make-date-option
            pagename
            widgetname
            pos
            widgetname
            (lambda ()
              (cons 'absolute
                    (cons (car (mktime (car (strptime "%Y-%m-%d" date-str)))) 0)))
            #f
            'absolute #f)))))))

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
          (_ text)))))))

(d:report
 "income-statement" ; name
 (d:defs
   (d:def-date "start-date" "General/Start date" "2011-01-01")
   (d:def-date "end-date" "General/End date" "2011-07-31"))
 ; Have to keep this title while experimenting in the sample report that
 ; comes with GnuCash
 "Hello, World" ; title
 "2011-01-01 to 2011-07-31" ; subtitle
 (d:filter-none ; body
  (d:p "Some text.")
  (d:p "A little more text.")))
