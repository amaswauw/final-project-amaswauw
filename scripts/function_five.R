library("jsonlite")
library("openxlsx")
library("dplyr")
library("plotly")


df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv", 
                stringsAsFactors = FALSE)
join_result <- inner_join(df3, df1, by = c("INSTNM" = "Name"))
join_result2 <- left_join(join_result, df2, by = c("INSTNM" = "displayName"))
summary_table <- join_result2 %>%
  select("INSTNM", "sat.avg", "act.avg", "hs.gpa.avg",
         "State.abbreviation",
         "Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native",
         "Percent.of.total.enrollment.that.are.Asian",
         "Percent.of.total.enrollment.that.are.Black.or.African.American",
         "Percent.of.total.enrollment.that.are.Hispanic/Latino",
         "Percent.of.total.enrollment.that.are.White",
         "Percent.of.total.enrollment.that.are.two.or.more.races",
         "Percent.of.total.enrollment.that.are.Race/ethnicity.unknown",
         "Percent.of.total.enrollment.that.are.Nonresident.Alien",
         "Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander",
         "Percent.of.total.enrollment.that.are.women",
         "overallRank",
         "acceptance.rate")
test <- summary_table %>%
  group_by(State.abbreviation) %>%
  summarize(mean_SAT = mean(sat.avg, na.rm = TRUE),
            mean_ACT = mean(act.avg, na.rm = TRUE),
            mean_hs_gpa = mean(hs.gpa.avg, na.rm = TRUE),
            mean_acceptance_rate = mean(acceptance.rate, na.rm = TRUE),
            mean_ranking = mean(overallRank, na.rm = TRUE))

plot_graph <- function(column_name) {
  plot_ly(data = test,
          x = ~State.abbreviation,
          y = ~column_name)
}
