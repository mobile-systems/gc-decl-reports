(use-modules (gnucash report gc-decl-reports))

(suite "With the take function"
  (tests
    (test "It should take nothing from an empty list"
      e
      ((e 'assert-equal) end (take 10 end)))
    (test "It should take nothing if nothing is asked for"
      e
      ((e 'assert-equal) end (take 0 (e 'lst))))
    (test "It should return a single-item list if 1 item requested"
      e
      ((e 'assert-equal) '(1) (take 1 (e 'lst))))
    (test "It should return items in the same order as the input list"
      e
      ((e 'assert-equal) '(1 2) (take 2 (e 'lst)))))
  (options)
  (setups
    (setup 'lst (list 1 2 3 4))))

