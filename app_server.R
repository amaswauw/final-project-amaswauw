library("jsonlite")
library("openxlsx")
library("dplyr")
library("plotly")
library("shiny")

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
})


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