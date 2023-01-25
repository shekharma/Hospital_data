best <- function(state, outcome) {
  ##Read csv for outcome data
  extract <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  req_data <- as.data.frame(cbind(extract[, 2], extract[, 7], extract[, 11], extract[, 17], extract[, 23]))  ##data required for further calculation
  ##here we can't use cbind.data.frame because it's not allow us to give the column names
  colnames(req_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ##Verify given state validity 
  if(!state %in% req_data[, "state"]){    ## here we can't use the && because it's not valid in character equality
    stop('invalid state')                 ## stop function gives Error: invalid outcome instead of text(invalid outcome)
  } 
  ##Verify given outcome validity  
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')          
  } 
  else {
    req_rows <- which(req_data[, "state"] == state)  #which function allows us to select of all row indices,where given state and state from req_data is same
    selset <- req_data[req_rows, ]                        # selection of set of data from our req_data data frame and srows for given state
    outval <- as.numeric(selset[, outcome])               #a numeric vector of all values for the outcome
    Hosp_Name  <- selset[, "hospital"][which.min(outval)] #compute value for the corresponding hospital name
    output<- Hosp_Name[order(Hosp_Name)]                  # ordering in alphabetical manner to handle ties
  }
  output
}
