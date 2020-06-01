library("jsonlite")
library("openxlsx")
library("dplyr")
library(shiny)
library(plotly)
library(ggplot2)


# Introduction (if necessary)



# Tuition

server <- shinyServer(function(input, output) {
  #render the introduction page
  output$introduction <- renderUI({
    HTML(markdown::markdownToHTML(knit('introduction.Rmd', quiet = TRUE)))
  })
  
  
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
#data <- final_tuition_table
#search = "Washington"
#graph_var = "In_state"
  
  draw_scatter <- function(data, graph_var) {
    
    ymax <- max(data[,graph_var]) * 1.5
    
   # filtered_data <- data %>%
    #  filter(data$State == search)
    graph_df <- data %>%
      select("State", graph_var)
    graph <- plot_ly(
      data = graph_df,
      x=~State,
      y=~graph_df[,graph_var],
      type ="scatter",
      mode = "markers",
      marker = list(
        size = 10
      )
    ) %>% 
      layout( 
             yaxis = list(range = c(0, ymax), title = graph_var)
      )
    
    return(graph)
  }
  
  output$scatter_plot <- renderPlotly ({
    return(draw_scatter(final_tuition_table, input$yaxis))
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

