rankall <- function(outcome, num = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fulldata   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fulldata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  fulldata[, eval(outcome)] <- as.numeric(fulldata[, eval(outcome)])
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    list_state <- with(fulldata, split(fulldata, state))
    orderedtable  <- list()
    for (i in seq_along(list_state)){
      list_state[[i]] <- list_state[[i]][order(list_state[[i]][, eval(outcome)], 
                                           list_state[[i]][, "hospital"]), ]
      orderedtable[[i]]  <- c(list_state[[i]][num, "hospital"], list_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, orderedtable)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      list_state <- with(fulldata, split(fulldata, state))
      orderedtable  <- list()
      
      ## For each state, find the hospital of the given rank
      
      for (i in seq_along(list_state)){
        list_state[[i]] <- list_state[[i]][order(list_state[[i]][, eval(outcome)], 
                                             list_state[[i]][, "hospital"]), ]
        orderedtable[[i]]  <- c(list_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, orderedtable)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      list_state <- with(fulldata, split(fulldata, state))
      orderedtable  <- list()
      for (i in seq_along(list_state)){
        list_state[[i]] <- list_state[[i]][order(list_state[[i]][, eval(outcome)], 
                                             list_state[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        orderedtable[[i]]  <- c(list_state[[i]][1, c("hospital", "state")])
      }
      
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      
      result <- do.call(rbind, orderedtable)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
      stop('invalid num')
    }
  }
  return(output)
}