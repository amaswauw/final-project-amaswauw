library(shiny)

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
# Loading in the necessary dataframes.
df1 <- read.xlsx("data/IPEDS_data.xlsx")
df2 <- data.frame(fromJSON(txt = "data/schoolInfo.json"))
df3 <- read.csv("../Most-Recent-Cohorts-All-Data-Elements.csv",
                stringsAsFactors = FALSE
)

table_join <- right_join(df1, df3, by = c("Name" = "INSTNM"))
table_join2 <- left_join(
  table_join,
  df2,
  by = c("Total.price.for.in-state.students.living.on.campus.2013-14" = "tuition"))


# Table with tution averages per state.
tuition_table <- table_join2 %>%
  select(Name = "Name",
         State = "STABBR",
         In_State = "Total.price.for.in-state.students.living.on.campus.2013-14",
         Out_of_State = "Total.price.for.out-of-state.students.living.on.campus.2013-14") %>%
  group_by(State) %>%
  summarize(In_state = mean(In_State, na.rm = T), Out_of_State = mean(Out_of_State, na.rm = T)) %>%
  arrange(-In_state)

#Remove na values from tuition table
final_tuition_table <- na.omit(tuition_table)

page_one <- tabPanel(
  "Tuition Visualization",
  titlePanel("Average Tuition per State"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "yaxis",
        label = "Tuition",
        choices = list("In_state", "Out_of_State"),
        selected = "In_state"
      ),
      #selectInput(
      # inputId = "xaxis",
      #label = "State",
      #choices = states,
      #selected = "Washington"
      #)
    ),
    mainPanel(
      h1("Tuition Chart"),
      p("This chart illustrates the average in state tuition and
        average out of state tuition per state"),
      plotlyOutput(outputId = "scatter_plot")
    )
  )
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

#all uni summary table
summary_states <- tabPanel(
  "Summary Table of University Statistics",
  DT::dataTableOutput("summarystates")
)


ui <- navbarPage(
  #application title
  "University Statistics in the US",
  
  #introduction page of the application
  page_one,
  tabPanel("Introduction",
           mainPanel(uiOutput("introduction"))),
  academic,
  tabPanel("Major Takeaways",
           mainPanel(uiOutput("takeaways"))),
  summary_states
)
