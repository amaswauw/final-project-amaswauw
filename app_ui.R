library(shiny)
library(plotly)

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
# Introduction


# Tuition


# Academics
sidebar_content <- sidebarPanel(
  selectInput(
    "academicInput",
    label = "Variable to Graph",
    
  )
)

academic <- tabPanel(
  "Academic Breakdown",
  
)

# Ethnicity