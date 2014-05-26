## FUNCTION findRefs
##
## Authored at http://stackoverflow.com/questions/8174695/find-all-functions-in-a-package-that-use-a-function/8175211#8175211
##
## utility for finding all functions
## in a package that use another function
## -----------------------------

findRefs <- function(pkg, fn) {
  ns <- getNamespace(pkg)
  found <- vapply(ls(ns, all.names=TRUE), function(n) {
    f <- get(n, ns)
    is.function(f) && fn %in% all.names(body(f))
  }, logical(1))
  
  names(found[found])
}

## --- useage ----------------
## findRefs('lattice', 'xyplot')
