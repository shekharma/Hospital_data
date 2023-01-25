rankhospital <- function(state, outcome, num = "best"){
  # Read csv for outcome data
  extract <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  req_data <- as.data.frame(cbind(extract[, 2], extract[, 7], extract[, 11], extract[, 17], extract[, 23]))
  colnames(req_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  # Verify valid state data 
  if (!state %in% req_data[, "state"]) {
    stop('invalid state')
  } 
  # Verify valid outcome data 
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } 
  else if (is.numeric(num)) {                      ##is.numeric checks the num is numeric or not
    selrows <- which(req_data[, "state"] == state)  ##select rowindices for given state
    grouped <- req_data[selrows, ]               ##group all the data for given state
    grouped[, outcome] <- as.numeric(grouped[, outcome])  ##making data in numeric form for given outcome
    a <- grouped[, outcome]
    grouped <- grouped[order(a, grouped[, "hospital"]), ]      ##arranging data into ascending order for given outcome 
    output<- grouped[num, "hospital"]
  } else if (!is.numeric(num)){
    if (num == "best") {
      output <- best(state, outcome)                ##it calls the best function to give the best hospital in state
    } else if (num == "worst") {
      selrows <- which(req_data[, "state"] == state)
      grouped <- req_data[selrows, ]    
      grouped[, outcome] <- as.numeric(grouped[, outcome])
      a <- grouped[, outcome]
      grouped <- grouped[order(a, grouped[, "hospital"], decreasing = TRUE), ] #order decreasing to handle ties
      output <- grouped[1, "hospital"]                      ##to give the first rank hospital which we arranged in decreasing order
    } 
  }
  output
}
