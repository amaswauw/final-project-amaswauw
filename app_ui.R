# Introduction


# Tuition


# Academics


# Ethnicity



ui <- navbarPage(
  #application title
  "University Statistics in the US",
  
  #introduction page of the application
  tabPanel("Introduction",
           mainPanel(uiOutput("introduction")))
)
