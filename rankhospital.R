rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fulldata   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fulldata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% fulldata[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    matchstate <- which(fulldata[, "state"] == state)
    tablestate <- fulldata[matchstate, ]                     
    # extracting dataframe for the called state
    tablestate[, eval(outcome)] <- as.numeric(tablestate[, eval(outcome)])
    tablestate <- tablestate[order(tablestate[, eval(outcome)], tablestate[, "hospital"]), ]
    output <- tablestate[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      matchstate <- which(fulldata[, "state"] == state)
      tablestate <- fulldata[matchstate, ]    
      tablestate[, eval(outcome)] <- as.numeric(tablestate[, eval(outcome)])
      tablestate <- tablestate[order(tablestate[, eval(outcome)], tablestate[, "hospital"], decreasing = TRUE), ]
      output <- tablestate[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}