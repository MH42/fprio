# fprio

fprio is a library that realizes a novel test case prioritiztion technique. In order to reduce the effort of regression testing, it tries to prioritize these tests that have - according to special prioritization criteria - the highest chance to detect a fault in the source code. Our prioritization criteria are based on the assumption that tests which
execute the most code modification the most often will have the highest chance to reveal faults as there is a great chance that these tests explore the application in many different states. For this purpose, we use both the output of regression test selection as well as test traces
obtained during test development.
