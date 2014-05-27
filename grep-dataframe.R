## ---- FUNCTION grep.dataframe() ----------------------------------------------
## 
## View, as a list, the columns of a data frame where pattern matching occurs
## -----------------------------------------------------------------------------
grep.dataframe <- function(pattern, X, ...)
{
    #### --- CHECK ARGUMENTS ---------------------------------------------------
    ## --- logical check of missing non-default arguments ---
    miss <- c(pattern, X)[c(missing(pattern), missing(X))]
    if(missing(pattern) || missing(X))
        stop(sprintf('argument "%s" is missing, with no default', miss))
    ## --- coerce pattern to character ---
    if(!is.character(pattern)) pattern <- as.character(pattern)
    ## --- coerced X to data frame ---
    if(!is.data.frame(X)) 
        X <- structure(data.frame(X), names = names(X))
    #### --- END CHECK ARGUMENTS -----------------------------------------------
    
    ## --- IF '...' ARGUMENTS PRESENT ------------------------------------------
    if(length(list(...))){
        
        ## function to remove the 'value' argument from 'grep' and call
        .dotdot <- function(pattern, x, ...){
            dots <- list(...)
            dots$value <- NULL
            args <- c(list(pattern = pattern, x = x, value = TRUE), dots)
            do.call(grep, args)
        }
        
        ## search the data frame and create the result
        ap <- lapply(X, function(y){
            cbind(value = .dotdot(pattern, y, ...), row = grep(pattern, y, ...))
        })  
        dc <- lapply(ap, as.data.frame)
        res <- lapply(dc, function(z) if(length(rownames(z))) z else NULL)
    
    }
    ## --- ELSE IF NO '...' ARGUMENTS PRESENT-------------------------------------
    else { 
           
        ap <- lapply(X, function(y){
            cbind(value = grep(pattern, y, value = TRUE), row = grep(pattern, y))
        })
        dc <- lapply(ap, as.data.frame)
        res <- lapply(dc, function(z) if(length(rownames(z))) z else NULL)
    
    }
    ## --- set attributes and return result -------------------------------------
    attr(res, "class") <- "list"
    return(res)

}
#### --- END FUNCTION grep.dataframe() ------------------------------------------
