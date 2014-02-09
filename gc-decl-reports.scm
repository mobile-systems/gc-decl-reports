; Declarative reports for GnuCash

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

(define-module (gnucash report gc-decl-reports)
  #:export
    (
    take
    ))
(use-modules (gnucash main))
(use-modules (gnucash gnc-module))
(use-modules (srfi srfi-19))

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
  (define (take-helper n lst ret-lst)
    (cond
      ((= 0 n) ret-lst)
      ((null? lst) ret-lst)
      (else (take-helper (- n 1) (cdr lst) (cons (car lst) ret-lst)))))
  (reverse (take-helper n lst '())))

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
(define (d:report guid menuitem defs . reportbuilders)
  (define ui-facing-name (N_ menuitem))

  (define (options-generator)
    (define opts (gnc:new-options))

    ;; This is an impure action (options are registered below).
    (for-each (lambda (def) (def opts)) defs)
    ;; All options are registered by this point.
    opts)

  (define (renderer report-obj)
    (define document (gnc:make-html-document))
    (define report-opts (gnc:report-options report-obj))

    (gnc:report-starting ui-facing-name)
    (for-each
      (lambda (reportbuilder) (reportbuilder document report-opts))
      reportbuilders)
    (gnc:report-finished)
    document)

  (gnc:define-report
    'version 1
    'name ui-facing-name
    'report-guid guid
    'options-generator options-generator
    'renderer renderer
    'menu-tip ui-facing-name ;; Tooltip same as menu item--for now.
    'menu-path (list gnc:menuname-utility)))

; d:defs - Tells options defs where they will be placed on the report
; options dialog box. The lambda passed to the map function takes two
; arguments, so the map function itself takes two lists (the defs and the
; list of positioning characters).
; Arguments:
;  defs - A list of defs like date defs, account defs, etc.
; Returns:
;  A list of definition function which now know where they will be placed
;  in the options dialog box, but need to be called with the report options
;  object as their argument (this will happen in the d:report function
;  where they are fed in as the defs argument).
(define (d:defs . defs)
  (map
    (lambda (def pos) (def pos))
    defs
    (take
      (length defs)
      '("ca" "cb" "cc" "cd" "ce" "cf" "cg" "ch" "ci" "cj" "ck" "cl" "cm"
      "cn" "co" "cp" "cq" "cr" "cs" "ct" "cu" "cv" "cx" "cy" "cz" "da"
      "db" "dc" "de" "df" "dg" "dh" "di" "dj" "dk" "dl" "dm" "dn" "do"
      "dp" "dq" "dr" "ds" "dt" "du" "dv" "dw" "dx" "dy" "dz"))))

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
(define (d:def-label page name val)
  (lambda (pos)
    (lambda (opts)
      (gnc:register-option
        opts
        (gnc:make-string-option page name pos name val)))))

; d:def-date - A date definition
; Arguments:
;  page - The name of the page (tab) in the report options dialog box on
;  which this option will appear.
;
;  name - The name of the option that will appear on the report options
;  dialog box.
;
;  yyyy - The year (four digits).
;
;  mm - The month (1 is January, 12 is December).
;
;  dd = The day of the month (1 to 31).
; Returns:
;  A date definition function that takes a positioning word as its argument.
(define (d:def-date page name yyyy mm dd)
  (define dt (localtime 0))
  (set-tm:year dt (- yyyy 1900))
  (set-tm:mon dt (- mm 1))
  (set-tm:mday dt dd)

  (lambda (pos)
    (lambda (opts)
      (gnc:register-option
        opts
        (gnc:make-date-option
          page
          name
          pos
          name
          (lambda () (cons 'absolute (cons (car (mktime dt)) 0)))
          #f
          'absolute
          #f)))))

; d:def-account-group - An account group definition
; Arguments:
;  page - The name of the page (tab) in the report options dialog box on
;  which this option will appear.
;
;  name - The name of the option that will appear on the report options
;  dialog box.
;
;  account-guids - Arbitrary number of strings containing account GUIDs. The
;  Account objects will be looked up and selected by default in the account
;  list widget in the options dialog box.
; Returns:
;  An account group definition function that takes a positioning word as
;  its argument.
(define (d:def-account-group page name . account-guids)
  (lambda (pos)
    (lambda (opts)
      (define cur-book (gnc-get-current-book))
      (gnc:register-option
        opts
        (gnc:make-account-list-option
          page
          name
          pos
          name
          (lambda ()
            (map
              (lambda (account-guid)
                (xaccAccountLookup account-guid cur-book))
              account-guids))
          #f
          #t)))))

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
(define (di:get-option-value opts page name)
  (gnc:option-value (gnc:lookup-option opts page name)))

; d:label - A getter for the value of a label def
; Arguments:
;  page - The page (tab) in the options dialog box on which the option is
;  located.
;
;  name - The name of the option.
; Returns:
;  A reportbuilder (function) that gets the label value.
(define (d:label page name)
  (lambda (document opts)
    (di:get-option-value opts page name)))

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
(define (di:put-text text markup-func document opts)
  (gnc:html-document-add-object!
    document
    (gnc:make-html-text
      (markup-func
      (_ (if (procedure? text) (text document opts) text))))))

; d:p - Generate a paragraph reportbuilder
; Arguments:
;  text - The text to put in the paragraph.
; Returns:
;  A function that, when called with the documents and options objects,
;  adds an HTML paragraph to the report page.
(define (d:p text)
  (lambda (document opts)
    (di:put-text text gnc:html-markup-p document opts)))

; d:title - Set the report title visible on the page
; Arguments:
;  text - A string or D:LABEL value that contains the title to set.
; Returns:
;  A reportbuilder that takes the report document and options objects and
;  sets the document title on the page.
(define (d:title text)
  (lambda (document opts)
    (gnc:html-document-set-title!
      document
      (_ (if (procedure? text) (text document opts) text)))))

; d:subtitle - Set the report title visible on the page
; Arguments:
;  text - A string or D:LABEL value that contains the subtitle to set.
; Returns:
;  A reportbuilder that takes the report document and options objects and
;  sets the document subtitle on the page.
(define (d:subtitle text)
  (lambda (document opts)
    (di:put-text text gnc:html-markup-h3 document opts)))

;; Report is supposed to look like the below.

(define report-name "GnuCash Declarative Reports - Test")

(d:report
  "decafbad"
  report-name
  (d:defs
    (d:def-label "General" "Report title" report-name)
    (d:def-label "General" "Report subtitle"
      (string-append "2011-01-01" " to " "2011-07-31"))
    (d:def-date "General" "Start date" 2011 1 1)
    (d:def-date "General" "End date" 2011 7 31)
    (d:def-account-group "Account Groups" "Income Accounts"
      "3288757aa761c7bba993766bfb433466"
      "e49c976843a1dba40570ed6295e89a33"
      "90d5b9035aa48a78e6584b77a44406b4"
      "fd38199026670043e1b534823589cde0")
    (d:def-account-group "Account Groups" "Expense Accounts"
      "df72fb29f46e90e12c2ba10d582a6148"
      "e518845492b7b9cfa614cc405a8cf715"
      "1ff6aa53c16083141d2e8f94ee06b9af"
      "abedd60f6f23f7544ba336007cb1d7cd"
      "7ae9864e5cf38ab3dad493ba22665a5a"
      "7bb4442daabd2152560feb9b0d80c275"
      "39655393f9a3d5140d522203d5dc0084"
      "0567c7b48dbc171b247316f212c30d8b"
      "bb5937c9397cde23b0ceab3498c7fc23"
      "7181899d1a563e107f6817a41a60fe87"
      "61c43be7097840fb64a34483969a2705"
      "9eb841fd996ce94143215b1c7a02bf1f"
      "ab3957aceb9cac4365f31fa58a20e42e"
      "03cd0b3bf985a4057ddb4bc0c6fa33df"
      "8604c0e2730314e3602cfb764f93397f"
      "39b0dbead49d5f6b463006d54eb1305a"
      "8fbd8636160210db8c139df1d3b70d27"
      "c70c5934df7a2db15caf36dd9da6dbc3"
      "0ca78dd5125ca2f0c9cb3b3ffb8b1ad3"
      "0d79529f99230620bff72b26e685e6b1"
      "85f90c79b26a7a82f6387dbaf184840f"
      "4920ef5a601b8dc41a09eed0af9bf960"
      "5fbf74bf49ecd371092b5b4918047332"
      "eaa862a0bac6632aa8fefcf7cd59455e"
      "97b973a463e09fad41b5f033c6b1f1bd"
      "466f6a4b3466f863e85187569050c39c"
      "02e4d83288fd7c024bbf9f1883ae53a3"
      "7ed651c57b8532acafc9eed8a6379502"
      "286b745f1eddf7bd2ccf3ba957c73b5d"
      "24119c9309714ec8c21b071c3e47c5fb"
      "86a56b49024ff08548ee150642484a4a"
      "65529b88a6c92a6b6b841d95fbc2525b"
      "4fb1db5436df3a5dc88704e49b59de0d")
    (d:def-account-group "Account Groups" "Liability Accounts"
      "1f117de75c9c87fac121f214b27b37b9"
      "8c582d38860b41e085a4683b9b6076f2"))
  (d:title (d:label "General" "Report title"))
  (d:subtitle (d:label "General" "Report subtitle"))
  (d:p "Some text.")
  (d:p "A little more text.")
;  (d:table
;    (d:totals 'sum ;; Add a totals row to the bottom of the 
;      (d:account-group "Account Groups" "Income Accounts"))
;    (d:totals 'sum
;      (d:account-group "Account Groups" "Expense Accounts")))
  )

