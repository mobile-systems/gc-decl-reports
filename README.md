# Declarative Reports for GnuCash

An experimental GnuCash Scheme (Guile) report to explore building an
easy-to-use, declarative reporting vocabulary for GnuCash. The end
result should be an experience similar to authoring a simple HTML page.

## Installation Instructions

Copy the `gc-decl-reports.scm` file into your `$HOME/.gnucash` directory
and edit (or create) the `$HOME/.gnucash/config.user` file to contain
the following line:

    (load "gc-decl-reports.scm")

The report will be visible with the next restart of GnuCash as the
`Reports > Sample & Custom > GnuCash Declarative Reports - Test` menu
item.

## Status

Currently the report will load and display an error. This is happening
because I haven't yet figured out how to parse a date string in Scheme
code into a date format that GnuCash's report options object will
accept. Work ongoing....

