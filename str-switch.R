# function str.switch() ----
# re-orders a string of alphanumerics so that
# either all letters or numbers are out front
# --- 05/23/2014 -----------

str.switch <- function(x, num.first = FALSE)
{
  if (!is.character(x)) 
    x <- structure(as.character(x), names = names(x))
  l <- gsub('[^A-Za-z]', '', x)
  n <- gsub('[^0-9]', '', x)
  if(!num.first) paste0(l, n) else paste0(n, l)
}

## ---- examples ------------
# vec <- with(dietox, c(levels(Evit), levels(Cu)))
# str.switch(vec, TRUE)
# str.switch(vec)
