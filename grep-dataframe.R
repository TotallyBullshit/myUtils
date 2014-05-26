## ---- FUNCTION grep.dataframe -----
##
## function to view or replace pattern-matched 
## string elements from data frames

grep.dataframe <- function(pattern, X, sub = NULL, ...)
{
    ## check and handle argument errors
    miss <- c(pattern, X)[c(missing(pattern), missing(X))]
    
    if(missing(pattern) || missing(X))
        stop(sprintf('argument "%s" is missing, with no default', miss))
    
    if(!is.character(pattern)) pattern <- as.character(pattern)
    
    if(!is.data.frame(X)) 
        X <- structure(data.frame(X), names = names(X))
    
    if(!is.null(sub) & !is.character(sub)) 
        stop('argument "sub" must be NULL or character')
    ## check args done
    
    
    
    ## sub not NULL
    if(!is.null(sub)){
        ap <- lapply(X, function(y) gsub(pattern, sub, y, ...))
        dc <- data.frame(ap)
        if(length(dc)) res <- dc else res <- NULL
        return(res)
    }    
    ## sub not NULL done
    
    ## for pattern, X, sub = NULL, and '...'
    if(is.null(sub)){
        ## if there are '...' arguments present
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
        } else { ## if there are no '...' arguments present at top level
            ap <- lapply(X, function(y){
                cbind(value = grep(pattern, y, value = TRUE), row = grep(pattern, y))
            })
            dc <- lapply(ap, as.data.frame)
            res <- lapply(dc, function(z) if(length(rownames(z))) z else NULL)
        }
        return(res)
    }
}

