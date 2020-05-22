library("jsonlite")
library("openxlsx")
library("dplyr")
library("ggplot2")

df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv", 
                stringsAsFactors = FALSE)

join_results1 <- inner_join(df3, df1, by = c("INSTNM" = "Name"))
result_df <- left_join(join_results1, df2, by = c("INSTNM" = "displayName"))

chart_df <- result_df %>% 
  select("Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native",
    "Percent.of.total.enrollment.that.are.Asian",
    "Percent.of.total.enrollment.that.are.Black.or.African.American",
    "Percent.of.total.enrollment.that.are.Hispanic/Latino",
    "Percent.of.total.enrollment.that.are.White",
    "Percent.of.total.enrollment.that.are.two.or.more.races",
    "Percent.of.total.enrollment.that.are.Race/ethnicity.unknown",
    "Percent.of.total.enrollment.that.are.Nonresident.Alien",
    "Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander",
    "Percent.of.total.enrollment.that.are.women"
  )

ave_chart_df <- chart_df %>% 
  summarize(
    ave_American_Indian = mean(Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native, na.rm = TRUE), 
    ave_Asian = mean(Percent.of.total.enrollment.that.are.Asian, na.rm = TRUE), 
    ave_African_American = mean(Percent.of.total.enrollment.that.are.Black.or.African.American, na.rm = TRUE), 
    ave_Latino = mean(`Percent.of.total.enrollment.that.are.Hispanic/Latino`, na.rm = TRUE), 
    ave_White = mean(Percent.of.total.enrollment.that.are.White, na.rm = TRUE), 
    ave_more_race = mean(Percent.of.total.enrollment.that.are.two.or.more.races, na.rm = TRUE), 
    ave_unknown = mean(`Percent.of.total.enrollment.that.are.Race/ethnicity.unknown`, na.rm = TRUE),
    ave_Islander = mean(`Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander`, na.rm = TRUE)
  )

ethnicity_percentages <- c(ave_chart_df$ave_American_Indian,
                           ave_chart_df$ave_Asian,
                           ave_chart_df$ave_African_American,
                           ave_chart_df$ave_Latino,
                           ave_chart_df$ave_White, 
                           ave_chart_df$ave_more_race,
                           ave_chart_df$ave_unknown,
                           ave_chart_df$ave_Islander)

ethnicity_categories <- c("American Indian / Alaska Native", 
                          "Asian",
                          "African American / Black",
                          "Hispanic / Latino",
                          "White",
                          "Two or more race", 
                          "Unknown Ethnicity", 
                          "Asian / Native Hawaiian / Pacific Islander")

pie_char_df <- data.frame(category = ethnicity_categories, percentage = ethnicity_percentages)

bar_plot <- ggplot(pie_char_df, aes(x = "", y = percentage, fill = category)) + geom_bar(width = 1, stat = "identity")

pie_plot <- bar_plot + 
  coord_polar("y") + 
  scale_fill_brewer(palette = "RdBu") + 
  labs(title = "Average ethnicity distribution of universities", x = NULL, y = NULL) + 
  geom_text(aes(label = paste(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.55)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 

