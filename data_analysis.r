source("~/ASA-Analysis-/preprocess_Data.R")
dat <- read_data("/Users/anyadecarlo/ctg_studies.csv")

str(dat)
(dat)

#Get Unique Values for Study Status 
print(unique(dat$Study_Status))


