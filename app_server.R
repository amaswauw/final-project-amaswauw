
library("jsonlite")
library("openxlsx")
library("dplyr")
library(knitr)
library(leaflet)
library("shiny")
library("plotly")
library("ggplot2")
library("DT")
library("knitr")


# Introduction (if necessary)

df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE,
                na.strings = "NULL")
join_result <- inner_join(df3, df1, by = c("INSTNM" = "Name"))

join_result2 <- left_join(join_result, df2,
                             by = c("INSTNM" = "displayName"))
ethnicity_categories <- c("American Indian / Alaska Native",
                          "Asian",
                          "African American / Black",
                          "Hispanic / Latino",
                          "White",
                          "Two or more race",
                          "Unknown Ethnicity",
                          "Asian / Native Hawaiian / Pacific Islander")



server <- shinyServer(function(input, output) {
  #render the introduction page
  output$introduction <- renderUI({
    HTML(markdown::markdownToHTML(knit("introduction.Rmd", quiet = TRUE)))
  })
  output$map <- renderLeaflet({
    return(draw_map(join_result2))
  })

  data <- final_tuition_table
  graph_var <- "In_state"


  output$scatter_plot <- renderPlotly({
    return(draw_scatter(final_tuition_table, input$yaxis))
  })

  # Academics
  output$gpa_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "hs.gpa.avg"))
  })

  output$sat_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "SAT_AVG_ALL"))
  })

  output$act_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "ACT.Composite.75th.percentile.score"))
  })

  output$acceptance_rate_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "ADM_RATE"))
  })

  output$ranking_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "overallRank"))
  })

  output$summary <- renderText({
    return(text_summary(join_result2, input$academicInput))
  })

  output$takeaways <- renderUI({
    HTML(markdown::markdownToHTML(knit("takeaways.Rmd", quiet = TRUE)))
  })

#uni summary table
  output$summarystates <- DT::renderDataTable({
    all_states_summary
  })
  
  output$pie_chart <- renderPlotly({
    data_chart <- join_result2 %>% 
      group_by(STABBR) %>% 
      summarize(ave_American_Indian =
                  mean(
                    Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native,
                    na.rm = TRUE),
                ave_Asian = mean(Percent.of.total.enrollment.that.are.Asian,
                                 na.rm = TRUE),
                ave_African_American =
                  mean(
                    Percent.of.total.enrollment.that.are.Black.or.African.American,
                    na.rm = TRUE),
                ave_Latino =
                  mean(
                    `Percent.of.total.enrollment.that.are.Hispanic/Latino`,
                    na.rm = TRUE),
                ave_White = mean(Percent.of.total.enrollment.that.are.White, na.rm = TRUE),
                ave_more_race =
                  mean(
                    Percent.of.total.enrollment.that.are.two.or.more.races,
                    na.rm = TRUE),
                ave_unknown =
                  mean(
                    `Percent.of.total.enrollment.that.are.Race/ethnicity.unknown`,
                    na.rm = TRUE),
                ave_Islander =
                  mean(
                    `Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander`,
                    na.rm = TRUE)) %>% 
      filter(STABBR == input$diversityInput)
    return(draw_pie(data_chart, ethnicity_categories))
  })
})
# Ethnicity
ethnicity_categories <- c("American Indian / Alaska Native",
                          "Asian",
                          "African American / Black",
                          "Hispanic / Latino",
                          "White",
                          "Two or more race",
                          "Unknown Ethnicity",
                          "Asian / Native Hawaiian / Pacific Islander")


#Create the tuition scatter plot
draw_scatter <- function(data, graph_var) {
  graph_df <- data %>%
    select("State", graph_var)
  if (graph_var == "In_state") {
  graph <- plot_ly(
    data = graph_df,
    x = ~State,
    y = ~In_state,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 10
    )
  ) %>% 
  layout( 
    title = "In-State Tuition vs State",
    xaxis = list(title = "State"),
    yaxis = list(title = "In-State Tuition")
  ) 
  } else {
    graph <- plot_ly(
      data = graph_df,
      x = ~State,
      y = ~Out_of_State,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 10
      )
    ) %>% 
    layout( 
      title = "Out-of-State Tuition vs State",
      xaxis = list(title = "State"),
      yaxis = list(title = "Out-of-State Tuition")
    )
  }
  return(graph)
}

draw_map <- function(data) {
  map <- leaflet(data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addMarkers(
      lat = ~LATITUDE,
      lng = ~LONGITUDE,
      popup = paste0("School: ", data$INSTNM)
    )
  return(map)
}

text_summary <- function(data, search) {
  filtered_data <- data %>%
    filter(data$STABBR == search)
  df <- filtered_data %>%
    select("INSTNM", "hs.gpa.avg", "SAT_AVG_ALL",
           "ACT.Composite.75th.percentile.score",
           "ADM_RATE", "overallRank")
  most_difficult_school_rate <- df %>%
    select("ADM_RATE") %>%
    min(na.rm = TRUE)
  most_diff_school_rate_school <- df %>%
    filter(ADM_RATE == most_difficult_school_rate) %>%
    select("INSTNM") %>%
    pull()
  easiest_school_rate <- df %>%
    select("ADM_RATE") %>%
    max(na.rm = TRUE)
  easiest_school_rate_school <- df %>%
    filter(ADM_RATE == easiest_school_rate) %>%
    select("INSTNM") %>%
    pull()
  ranked_schools <- df %>%
    select("overallRank") %>%
    na.omit() %>%
    nrow()
  if (ranked_schools > 0) {
    highest_ranked <- df %>%
      select("overallRank") %>%
      max(na.rm = TRUE)
    highest_ranked_school <- df %>%
      filter(overallRank == highest_ranked) %>%
      select("INSTNM") %>%
      pull()
    if (ranked_schools > 1) {
      lowest_ranked <- df %>%
        select("overallRank") %>%
        min(na.rm = TRUE)
      lowest_ranked_school <- df %>%
        filter(overallRank == lowest_ranked) %>%
        select("INSTNM") %>%
        pull()
      return(paste0("The following charts show the results for ",
                    search, ". What we can see is that",
                    "there are a few schools that are ommitted out",
                    "of some of the charts. This is because the data",
                    "was not available for that school.", "The school",
                    "with the lowest admission rate was ",
                    most_diff_school_rate_school, " with ",
                    most_difficult_school_rate, " rate. The school with the ",
                    "easiest admissions rate was ",
                    easiest_school_rate_school, " with ", easiest_school_rate,
                    " rate. The highest ranked school was ",
                    highest_ranked_school, " and ", lowest_ranked_school,
                    " was the lowest ranked school."))
    }
    return(paste0("The following charts show the results for ",
                  search, ". What we can see is that",
                  "there are a few schools that are ommitted out",
                  "of some of the charts. This is because the data",
                  "was not available for that school.", "The school",
                  "with the lowest admission rate was ",
                  most_diff_school_rate_school, " with ",
                  most_difficult_school_rate, " rate. The school with the ",
                  "easiest admissions rate was ",
                  easiest_school_rate_school, " with ", easiest_school_rate,
                  " rate. The highest ranked school was ",
                  highest_ranked_school, "."))
  } else {
    return(paste0("The following charts show the results for ",
                  search, ". What we can see is that",
                  "there are a few schools that are ommitted out",
                  "of some of the charts. This is because the data",
                  "was not available for that school.", "The school",
                  "with the lowest admission rate was ",
                  most_diff_school_rate_school, " with ",
                  most_difficult_school_rate, " rate. The school with the ",
                  "easiest admissions rate was ",
                  easiest_school_rate_school, " with ", easiest_school_rate,
                  " rate. Unfortunately, no ranking information is available ",
                  "for ", search, "."))
  }
}


draw_bar <- function(data, search, graph_var) {
  filtered_data <- data %>%
    filter(data$STABBR == search)
  graph_df <- filtered_data %>%
    select("INSTNM", graph_var)
  graph <- plot_ly(
    data = graph_df,
    x = ~INSTNM,
    y = ~graph_df[, graph_var],
    type = "bar"
  ) %>%
    layout(
      xaxis = list(tickangle = 45, titlefont = list(size = 30)),
      yaxis = list(title = graph_var)
    )
  return(graph)
}


# Ethnicity
draw_pie <- function(data, category){
  div_percentages <- c(data$ave_American_Indian, 
                       data$ave_Asian, 
                       data$ave_African_American,
                       data$ave_Latino,
                       data$ave_White,
                       data$ave_more_race,
                       data$ave_unknown,
                       data$ave_Islander)
  pie_df <- data.frame(percentages = div_percentages, categories = category)
  View(pie_df)
  pie_plot <- plot_ly(
    pie_df,
    labels = ~categories,
    values = ~percentages, 
    type = "pie") %>%
    layout(legend = list(orientation = 'h'))
  return(pie_plot)
}


#all uni summary table
#all states summary table
all_states_summary <- join_result2 %>%
  group_by(INSTNM, STABBR) %>%
  summarise(
    act.avg = mean(act.avg, na.rm = TRUE),
    hs.gpa.avg = mean(hs.gpa.avg, na.rm = TRUE),
    percent_of_american_indian_alaskan_native =
      mean(
        Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native,
           na.rm = TRUE),
    percent_of_asian = mean(Percent.of.total.enrollment.that.are.Asian,
                            na.rm = TRUE),
    percent_of_african_american =
      mean(Percent.of.total.enrollment.that.are.Black.or.African.American,
           na.rm = TRUE),
    percent_of_hispanic_or_latino =
      mean(`Percent.of.total.enrollment.that.are.Hispanic/Latino`,
           na.rm = TRUE),
    percent_of_white =
      mean(Percent.of.total.enrollment.that.are.White, na.rm = TRUE),
    percent_of_two_or_more_races =
      mean(Percent.of.total.enrollment.that.are.two.or.more.races,
           na.rm = TRUE),
    percent_of_race_unknown =
      mean(`Percent.of.total.enrollment.that.are.Race/ethnicity.unknown`,
           na.rm = TRUE),
    percent_of_nonresident_alien =
      mean(Percent.of.total.enrollment.that.are.Nonresident.Alien,
           na.rm = TRUE),
    percent_of_asian_native_pacific_islander =
      mean(
`Percent.of.total.enrollment.that.are.Asian/Native.Hawaiian/Pacific.Islander`,
        na.rm = TRUE),
    percent_of_women = mean(Percent.of.total.enrollment.that.are.women)
  ) %>%
  arrange(act.avg)
