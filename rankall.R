#To find the best(or given rank) hospital in all states 
rankall <- function(outcome, num = "best"){
  # Read csv for outcome data
  extract <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  req_data <- as.data.frame(cbind(extract[, 2], extract[, 7], extract[, 11], extract[, 17], extract[, 23]))
  colnames(req_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  req_data[, outcome] <- as.numeric(req_data[, outcome])
  
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    req_state <- with(req_data, split(req_data, state))  ##create the statewise list
    ordered  <- list()   #create list
    for (i in seq_along(req_state)){        ##seq_along generate the the index for states like 1 for Ak, 2for AL and so on 
      req_state[[i]] <- req_state[[i]][order(req_state[[i]][, outcome], req_state[[i]][, "hospital"]), ] ##store the data for i^th state if i=1,then AK
      ordered[[i]]  <- c(req_state[[i]][num, "hospital"], req_state[[i]][, "state"][1]) #this gives the asked rank hospital in that state
      #if num=2 and i=1 then it will give the second rank hospital in AK state 
    }
    result <- do.call(rbind, ordered)  #it binds all the ordered data with class "matrix" "array" 
    output <- as.data.frame(result, row.names = result[, 2]) #easy to access the element as compare to "matrix" & "array"
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      req_state <- with(req_data, split(req_data, state))
      ordered  <- list()
      ##for loop create the top ranked hospital list with state 
      for (i in seq_along(req_state)){
        req_state[[i]] <- req_state[[i]][order(req_state[[i]][, outcome], req_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(req_state[[i]][1, c("hospital", "state")])  ##it directly gives the ranked 1 hospital name
      }
      result <- do.call(rbind, ordered)    ##do.call allows to bind the data store in ordered list
      output <- as.data.frame(result)
      rownames(output) <- output[, 2]       
    } else if (num == "worst") {
      req_state <- with(req_data, split(req_data, state))
      ordered  <- list()
      for (i in seq_along(req_state)){
        req_state[[i]] <- req_state[[i]][order(req_state[[i]][, outcome], req_state[[i]][, "hospital"], decreasing = TRUE), ]
        ## above req_state is in descending manner like worst,worst-1,worst-2,....best
        ordered[[i]] <- c(req_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result)
    } 
  }
  output
}
