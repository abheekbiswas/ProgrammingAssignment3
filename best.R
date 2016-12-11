best <- function(state, outcome) {
  ##Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fulldata   <- as.data.frame(cbind(data[, 2],   ##hospital
                              data[, 7],   ##state
                              data[, 11],  ##heart attack
                              data[, 17],  ##heart failure
                              data[, 23]), ##pneumonia
                              stringsAsFactors = FALSE)
  colnames(fulldata) <- c("hospital", "state", "heart attack", "heart failure"
                          , "pneumonia")
  
  ##Check that state and outcome are valid
  if(!state %in% fulldata[, "state"]){
    stop('Invalid State')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
          stop('Invalid Outcome')
  } else {
        matchstate <- which(fulldata[, "state"] == state)
        tablestate <- fulldata[matchstate, ]    
    
    ##Return a data frame with the hospital names and the abbreviated state 
    ##name
    outputdata <- as.numeric(tablestate[, eval(outcome)])
    min_val <- min(outputdata, na.rm = TRUE)
    result  <- tablestate[, "hospital"][which(outputdata == min_val)]
    output  <- result[order(result)]
  }
  return(output)
}
