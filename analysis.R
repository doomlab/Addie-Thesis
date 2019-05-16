##set working directory to where file is saved
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

master = read.csv("MasterDataset.csv", stringsAsFactors = F)

##summary
summary(master)

##cor.test

##ravens + ospan

##ravens + tt

##ospan + tt

##regression (fix column names) 
model = lm(ravens ~ ospan + math,  data = master)
summary(model)

library(lm.beta)
summary.lm.beta(model) #to get standardized coefficients