;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;;; * Introduction

;;;; FiveAM is a testing framework. It takes care of all the boring
;;;; bookkeeping associated with managing a test framework allowing
;;;; the developer to focus on writing tests and code.

;;;; FiveAM was designed with the following premises:

;;;; - Defining tests should be about writing tests, not
;;;; infrastructure. The developer should be able to focus on what
;;;; they're testing, not the testing framework.

;;;; - Interactive testing is the norm. Common Lisp is an interactive
;;;; development environment, the testing environment should allow the
;;;; developer to quickly and easily redefine, change, remove and run
;;;; tests.

(defpackage :it.bese.fiveam
  (:use :common-lisp :alexandria)
  (:nicknames :5am :fiveam)
  #+sb-package-locks
  (:lock t)
  (:export
   ;; creating tests and test-suites
   #:make-suite
   #:def-suite
   #:def-suite*
   #:in-suite
   #:in-suite*
   #:make-test
   #:test
   #:def-test
   #:get-test
   #:rem-test
   #:test-names
   #:*default-test-compilation-time*
   ;; fixtures
   #:make-fixture
   #:def-fixture
   #:with-fixture
   #:get-fixture
   #:rem-fixture
   ;; running checks
   #:is
   #:is-every
   #:is-true
   #:is-false
   #:signals
   #:finishes
   #:skip
   #:pass
   #:fail
   #:*test-dribble*
   #:for-all
   #:*num-trials*
   #:*max-trials*
   #:gen-integer
   #:gen-float
   #:gen-character
   #:gen-string
   #:gen-list
   #:gen-tree
   #:gen-buffer
   #:gen-one-element
   ;; running tests
   #:run
   #:run-all-tests
   #:explain
   #:explain!
   #:run!
   #:debug!
   #:!
   #:!!
   #:!!!
   #:*run-test-when-defined*
   #:*debug-on-error*
   #:*debug-on-failure*
   #:*on-error*
   #:*on-failure*
   #:*verbose-failures*
   #:*print-names*
   #:results-status))

;;;; You can use #+5am to put your test-defining code inline with your
;;;; other code - and not require people to have fiveam to run your
;;;; package.

(pushnew :5am *features*)

;;;;@include "check.lisp"

;;;;@include "random.lisp"

;;;;@include "fixture.lisp"

;;;;@include "test.lisp"

;;;;@include "suite.lisp"

;;;;@include "run.lisp"

;;;;@include "explain.lisp"

;;;; * Colophon

;;;; This documentaion was written by Edward Marco Baringer
;;;; <mb@bese.it> and generated by qbook.

;;;; ** COPYRIGHT

;;;; Copyright (c) 2002-2003, Edward Marco Baringer
;;;; All rights reserved.

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;  - Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.

;;;;  - Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.

;;;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;;;    of its contributors may be used to endorse or promote products
;;;;    derived from this software without specific prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
