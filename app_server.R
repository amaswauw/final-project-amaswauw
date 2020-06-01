library("jsonlite")
library("openxlsx")
library("dplyr")
library("shiny")
library("plotly")
library("ggplot2")
library("DT")


# Introduction (if necessary)

df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("../Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE)
join_result <- inner_join(df3, df1, by = c("INSTNM" = "Name"))

join_result2 <- left_join(inner_join_df, df2,
                             by = c("INSTNM" = "displayName"))



server <- shinyServer(function(input, output) {
  #render the introduction page
  output$introduction <- renderUI({
    HTML(markdown::markdownToHTML(knit('introduction.Rmd', quiet = TRUE)))
  })
  
  
  # Tuition
  #x <- reactive ({
  #  final_tuition_table[, input$xaxis]
  #})  
  
  #y <- reactive({
  #  final_tuition_table[, input$yaxis]
  #})
  #output$scatter_plot <- renderPlotly (
  #  p <- plot_ly(
  #    x = x(),
  #    y = y(),
  #    type = "scatter"
  #  )
  #)
  data <- final_tuition_table
  #search = "Washington"
  graph_var = "In_state"
  
  
  
  output$scatter_plot <- renderPlotly ({
    return(draw_scatter(final_tuition_table, input$yaxis))
  })
  
  
  
  
  # Academics
  output$gpa_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "hs.gpa.avg"))
  })
  
  output$SAT_graph <- renderPlotly({
    return(draw_bar(join_result2, input$academicInput,
                    "SAT_AVG_ALL"))
  })

  

  
# Academics

  output$summary <- renderText({
    return(textSummary(join_result2, input$academicInput))
  })
  
  output$takeaways <- renderUI({
    HTML(markdown::markdownToHTML(knit('takeaways.Rmd', quiet = TRUE)))
  })
  
#uni summary table
  output$summarystates = DT::renderDataTable({
    all_states_summary
  })
})



# Ethnicity


draw_scatter <- function(data, graph_var) {
  
  #ymax <- max(data[,graph_var]) * 1.5
  
  # filtered_data <- data %>%
  #  filter(data$State == search)
  
  graph_df <- data %>%
    select("State", graph_var)
  #View(graph_df)
  #print(graph_df[,graph_var])
  if (graph_var == "In_state") {
  graph <- plot_ly(
    data = graph_df,
    x=~State,
    y=~In_state,
    type ="scatter",
    mode = "markers",
    marker = list(
      size = 10
    )
  ) 
  } else {
    graph <- plot_ly(
      data = graph_df,
      x=~State,
      y=~Out_of_State,
      type ="scatter",
      mode = "markers",
      marker = list(
        size = 10
      )
    ) 
  }
  #%>% 
  #ayout( 
  #yaxis = list(range = c(0, ymax), title = graph_var)
  #)
  
  return(graph)
}

#all uni summary table
#all states summary table
all_states_summary <- join_result2 %>%
  group_by(INSTNM) %>%
  summarise(
    act.avg = mean(act.avg, na.rm = TRUE),
    hs.gpa.avg = mean(hs.gpa.avg, na.rm = TRUE),
    percent_of_american_indian_alaskan_native =
      mean(Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native,
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






