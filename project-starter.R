library("openxlsx")
library("jsonlite")
library("dplyr")
library("shiny")
library("leaflet")
library("ggplot2")
library("plotly")

df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv", 
                stringsAsFactors = FALSE)
