library("jsonlite")
library("openxlsx")
library("dplyr")
library(shiny)
library(plotly)
library(ggplot2)


server <- function(input, output) {
# Introduction (if necessary)


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


# Ethnicity

}