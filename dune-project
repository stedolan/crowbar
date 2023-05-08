(lang dune 2.9)
(name crowbar)

(formatting disabled)
(generate_opam_files true)

(source (github stedolan/crowbar))
(license MIT)
(authors "Stephen Dolan <stephen.dolan@cl.cam.ac.uk>")
(maintainers "Stephen Dolan <stephen.dolan@cl.cam.ac.uk>")

(package
 (name crowbar)
 (synopsis "Write tests, let a fuzzer find failing cases")
 (description
"\| Crowbar is a library for testing code, combining QuickCheck-style
"\| property-based testing and the magical bug-finding powers of
"\| [afl-fuzz](http://lcamtuf.coredump.cx/afl/).
 )
 (depends
  (ocaml (>= "4.08"))
  (cmdliner (>= 1.1.0))
  (afl-persistent (>= "1.1"))
  ("calendar" (and (>= "2.00") :with-test))
  ("fpath" :with-test)
  ("pprint" :with-test)
  ("uucp" :with-test)
  ("uunf" :with-test)
  ("uutf" :with-test)))
