
##set working directory to where file is saved
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("~/Documents/GitHub/Addie-Thesis/Qualtrics/All Data from Qualtrics Before 5:15")

master <- read.csv("Ravens_May15_edited.csv", stringsAsFactors = F)

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
master$total = (scorer("Q5", 5) + scorer("Q6", 1) + scorer("Q7", 7) + scorer("Q8", 4) + scorer("Q9", 3)
                + scorer("Q10",1) + scorer("Q11", 6) + scorer("Q12", 1) + scorer("Q13", 8)  
                + scorer("Q14", 4) + scorer("Q15", 5) + scorer("Q16", 6) + scorer("Q17", 2) 
                + scorer("Q18", 1) + scorer("Q19", 2) + scorer("Q20", 4) + scorer("Q21", 6)
                + scorer("Q22", 7) + scorer("Q23", 3) + scorer("Q24", 8) + scorer("Q25", 8)
                + scorer("Q26", 7) + scorer("Q27", 6) + scorer("Q28", 3) + scorer("Q29", 7)
                + scorer("Q30", 2) + scorer("Q31", 7) + scorer("Q32", 5) + scorer("Q33", 6)
                + scorer("Q34", 5) + scorer("Q35", 4) + scorer("Q36", 8) + scorer("Q37", 5)
                + scorer("Q38", 1) + scorer("Q39", 3) + scorer("Q40", 2))

##print out the useful part

write.csv(master[ , c("Q44", "total", "StartDate", "EndDate") ], "ravens_scores.csv", row.names = F)
View(master)
