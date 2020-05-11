library("openxlsx")
library("jsonlite")

result <- read.xlsx("IPEDS_data.xlsx")
result2 <- data.frame(fromJSON(txt = "schoolInfo.json"))