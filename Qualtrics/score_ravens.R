
##set working directory to where file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master <- read.csv("ravens_edit.csv", stringsAsFactors = F)

##function
scorer <- function(column, answer) {
  
  temp <- as.numeric(master[ , column] == answer)
  
  for (i in 1:length(temp)){
    if(is.na(temp[i])){
      temp[i] = 0
    }
  }
  
  return(temp)
  
  }

#add stuff up
master$total = scorer("Q5", 5) + scorer("Q6", 1) + scorer("Q7", 7)

##print out the useful part

write.csv(master[ , c("Q44", "total", "StartDate", "EndDate") ], "ravens_scores.csv", row.names = F)
