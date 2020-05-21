library("jsonlite")
library("openxlsx")
library("dplyr")

df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv", 
                stringsAsFactors = FALSE)
