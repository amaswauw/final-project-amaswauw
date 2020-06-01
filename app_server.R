library("jsonlite")
library("openxlsx")
library("dplyr")
library(shiny)
library(plotly)
library(ggplot2)


# Introduction (if necessary)





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
