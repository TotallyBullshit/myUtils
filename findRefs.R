## FUNCTION findRefs
## utility for getting reference
## information about an existing
## R function
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
