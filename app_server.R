library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(knitr)


server <- shinyServer(function(input, output) {
  #render the introduction page
  output$introduction <- renderUI({
    HTML(markdown::markdownToHTML(knit('introduction.Rmd', quiet = TRUE)))
  })
})

# Tuition


# Academics


# Ethnicity
