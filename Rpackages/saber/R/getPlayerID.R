## FUNCTION getPlayerID ------------------------------------
## ---------------------------------------------------------
## Result:
## A data frame of basic player information, to be fed to
## other functions in the package. 
## If the 'retire' column contains 'NA', that player's final 
## appearance has yet to be recorded.
## ----------------------------------------------------------
## Arguments:
## 1. lastName - A non-zero length character vector of the 
## player's last name(s).
## 2. id.only - if TRUE, returns a vector of matched IDs only, 
## and defaults other arguments to FALSE automatically.
## 2. firstName - if TRUE, adds the player's first name to the 
## result.  Useful for determining the desired player if when
## FALSE returns duplicated last names for different players.
## ---------------------------------------------------------
## Will return an error for IDs entered for members of the 
## Lahman database that never appeared in a professional game
## ---------------------------------------------------------
## Last update: 05/21/2014

getPlayerID <- function(lastName, id.only = FALSE, firstName = FALSE)
{
  ##
  Master <- Lahman::Master
  Appearances <- Lahman::Appearances
  Batting <- Lahman::Batting
  ##  
  if( missing(lastName) ){stop("please enter a name")}
  ##
  allLast <- lapply(lastName, function(a){
    paste0(toupper(substr(a, 1, 1)), tolower(substr(a, 2, nchar(a))))
    })
  ##
  lastList <- with(Master, unique(nameLast))
  ##
  mln <- match.arg(unlist(allLast), lastList, TRUE)
  ##
  ID <- unlist(sapply(1:length(mln), function(i){
    with(Master, playerID[nameLast == mln[i]])
    }))
  ##
  id <- ID[is.na(ID) == FALSE]
  ##
  if(id.only) return(id)
  ##
  if(length(id) > 1){
    message("more than one match found")
    res <- do.call(rbind, lapply(1:length(id), function(i){
    rg <- with(Appearances, range(yearID[ playerID == id[i] ]))
    data.frame(playerID = id[i], 
               debut = rg[1], 
               retire = ifelse(rg[2] != max(Batting$yearID), rg[2], NA))
    }))
    result <- res[order(res$debut), ]
  } else if(length(id) == 1){
    result <- id
  } else {
    stop(sprintf("player '%s' not found in database", lastName))
  }
  ##    
  if(firstName){
    matchFirst <- as.vector(sapply(1:length(id), function(i){
      with(Master, nameFirst[playerID == id[i]])
      }))
    matchFirst <- matchFirst[!is.na(matchFirst)]
    result <- cbind(playerID = res[, 1], first = matchFirst, res[, 2:3])
    result <- result[order(result$debut), ]
  }
  ##
  rownames(result) <- seq(length = nrow(result))
  return(result)
}

## examples of getplayerID usage------------
#  getPlayerID("Cabrera", firstName = TRUE)
#  getPlayerID(c("Cabrera", "Pujols"), id.only = TRUE)
## end examples -------------------------------

