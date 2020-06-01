library(shiny)
library(leaflet)

states <- list(
  "Alabama" = "AL",
  "Alaska" = "AK",
  "Arizona" = "AZ",
  "Arkansas" = "AR",
  "California" = "CA",
  "Colorado" = "CO",
  "Connecticut" = "CT",
  "Delaware" = "DE",
  "Washington D.C." = "DC",
  "Florida" = "FL",
  "Georgia" = "GA",
  "Hawaii" = "HI",
  "Idaho" = "ID",
  "Illinois" = "IL",
  "Indiana" = "IN",
  "Iowa" = "IA",
  "Kansas" = "KS",
  "Kentucky" = "KY",
  "Louisiana" = "LA",
  "Maine" = "ME",
  "Maryland" = "MD",
  "Massachusetts" = "MA",
  "Michigan" = "MI",
  "Minnesota" = "MN",
  "Missouri" = "MO",
  "Mississippi" = "MS",
  "Montana" = "MT",
  "Nebraska" = "NE",
  "Nevada" = "NV",
  "New Hampshire" = "NH",
  "New Jersey" = "NJ",
  "New Mexico" = "NM",
  "New York" = "NY",
  "North Carolina" = "NC",
  "North Dakota" = "ND",
  "Ohio" = "OH",
  "Oklahoma" = "OK",
  "Oregon" = "OR",
  "Pennsylvania" = "PA",
  "Rhode Island" = "RI",
  "South Carolina" = "SC",
  "South Dakota" = "SD",
  "Tennessee" = "TN",
  "Texas" = "TX",
  "Utah" = "UT",
  "Virginia" = "VA",
  "Vermont" = "VT",
  "Washington" = "WA",
  "West Virginia" = "WV",
  "Wisconsin" = "WI",
  "Wyoming" = "WY"
)


# Tuition


map_panel <- mainPanel(
  leafletOutput("map")
)

map <- tabPanel(
  "Map",
  titlePanel("Map of All Universities in the United States"),
  map_panel
)


# Academics
sidebar_content <- sidebarPanel(
  selectInput(
    "academicInput",
    label = "Choose a State Whose Academic Performance You Want To See",
    choices = states,
    selected = list("Washington" = "WA")
  )
)

academic_graph <- mainPanel(
  plotlyOutput("gpa_graph"),
  plotlyOutput("SAT_graph"),
  plotlyOutput("ACT_graph"),
  plotlyOutput("acceptance_rate_graph"),
  plotlyOutput("ranking_graph"),
  textOutput("summary")
)

academic <- tabPanel(
  "Academics",
  titlePanel("Academic Breakdown"),
  sidebarLayout(
    sidebar_content,
    academic_graph
  )
)

# Ethnicity



ui <- navbarPage(
  #application title
  "University Statistics in the US",
  
  #introduction page of the application
  tabPanel("Introduction",
           mainPanel(uiOutput("introduction"))),
  map,
  academic,
  tabPanel("Major Takeaways",
           mainPanel(uiOutput("takeaways")))
)
