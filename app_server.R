library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(knitr)


# Introduction (if necessary)
#render the introduction page
server <- shinyServer(function(input, output) {
  output$introduction <- renderUI({
    HTML(markdown::markdownToHTML(knit('introduction.Rmd', quiet = TRUE)))
  })
})

# Tuition


# Academics


# Ethnicity