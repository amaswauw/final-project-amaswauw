library("jsonlite")
library("openxlsx")
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(knitr)

df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("data/Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE,
                na.strings = "NULL")
join_result <- inner_join(df3, df1, by = c("INSTNM" = "Name"))
join_result2 <- left_join(join_result, df2, by = c("INSTNM" = "displayName"))


server <- shinyServer(function(input, output) {
  #render the introduction page
  output$introduction <- renderUI({
    HTML(markdown::markdownToHTML(knit('introduction.Rmd', quiet = TRUE)))
  })
  # Introduction (if necessary)
  
  
  # Tuition
  
  
  # Academics
  output$gpa_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "hs.gpa.avg"))
  })
  
  output$SAT_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "SAT_AVG_ALL"))
  })
  
  output$ACT_graph <- renderPlotly({
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
    return(textSummary(join_result2, input$academicInput))
  })
})

textSummary <- function(data, search) {
  filtered_data <- data %>%
    filter(data$STABBR == search)
  df <- filtered_data %>%
    select("INSTNM", "hs.gpa.avg", "SAT_AVG_ALL",
           "ACT.Composite.75th.percentile.score",
           "ADM_RATE", "overallRank")
  most_difficult_school_rate <- df %>%
    select("ADM_RATE") %>%
    min(na.rm = TRUE)
  most_difficult_school_rate_school <- df %>%
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
                    most_difficult_school_rate_school, " with ",
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
                  most_difficult_school_rate_school, " with ",
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
                  most_difficult_school_rate_school, " with ",
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
    x=~INSTNM,
    y=~graph_df[,graph_var],
    kind="bar"
  ) %>%
  layout(
    xaxis = list(tickangle=45, titlefont=list(size=30)),
    yaxis = list(title = graph_var)
  )
  return(graph)
}