library(shiny)
library(ggvis)
library(GGally)
library(ggplot2)

shinyUI(fluidPage(
tabsetPanel(
      tabPanel("Parallel Coordinates", 
               fluidRow(
                 div(h3('Outcomes for Diabetes Patients Admited to the Emergency Room'),
                 p('Hover on a line to highlight. Response may be slow.'),
                 p('Data has been sampled down to 100 cases per outcome.'),
                 style='margin-left:20px;'),
                 column(width=3,
                        checkboxGroupInput("par_vars",
                                           label = "Vars to Include:",
                                           choiceNames = c("Gender", 
                                                           "Age (grouped)", 
                                                           "Length of Stay (days)",
                                                           "# of Lab Procedures during Visit",
                                                           "# of Non-Lan Procedures during Visit",
                                                           "# of Medications Given during Visit",
                                                           "# of Outpatient Visits Last 12 Months",
                                                           "# of Inpatient Visits Last 12 Months",
                                                           "# of Emergency Visits Last 12 Months"),
                                           choiceValues = c("Gender",
                                                            "Age",
                                                            "Stay.Length",
                                                            "Lab.Procedures",
                                                            "Non.Lab.Procedures",
                                                            "Medications",
                                                            "Outpatient",
                                                            "Inpatient",
                                                            "Emergency"),
                                           selected = c('Age',
                                                        'Stay.Length',
                                                        "Lab.Procedures",
                                                        'Emergency'))
                 ),
                 column(width=9,
                        ggvisOutput("parallel")
                 )
               )
               ),
      tabPanel("Multiscatter",
               fluidRow(
                 div(h3('Outcomes for Diabetes Patients Admited to the Emergency Room'),
                 p('Data has been sampled down to 100 cases per outcome.'),
                 style='margin-left:20px;'),
                 column(width=3,
                        checkboxGroupInput("multi_vars",
                                           label = "Vars to Include:",
                                           choiceNames = c("Outcome (1 = discharged home, 0 = died)",
                                                           "Gender", 
                                                           "Age (grouped)", 
                                                           "Length of Stay (days)",
                                                           "# of Lab Procedures during Visit",
                                                           "# of Non-Lan Procedures during Visit",
                                                           "# of Medications Given during Visit",
                                                           "# of Outpatient Visits Last 12 Months",
                                                           "# of Inpatient Visits Last 12 Months",
                                                           "# of Emergency Visits Last 12 Months"),
                                           choiceValues = c("Outcome",
                                                            "Gender",
                                                            "Age",
                                                            "Stay.Length",
                                                            "Lab.Procedures",
                                                            "Non.Lab.Procedures",
                                                            "Medications",
                                                            "Outpatient",
                                                            "Inpatient",
                                                            "Emergency"),
                                           selected = c('Outcome',
                                                        'Gender',
                                                        'Age',
                                                        'Stay.Length'))
                 ),
                 column(width=9,
                        plotOutput('multiscatter', height = "600px")
                 )
               )),
      tabPanel("Heatmap",
               fluidRow(
                 div(h3('Response to Facebook Posts by Company X'),style='margin-left:20px;'),
                 column(width=3,
                        radioButtons("opacity_control",
                                     label = "Heat Map by:",
                                     choiceNames = c("Likes per Post", 
                                                     "Comments per Post", 
                                                     "Shares per Post"),
                                     choiceValues = c("likepp", 
                                                      "commentpp", 
                                                      "sharepp")),
                        sliderInput("months", 
                                    label="Months of the Year",
                                    min=1, max=12, step=1,
                                    value=c(1,12)),
                        checkboxInput("is_split", 
                                      label="Split by content type?",
                                      value=TRUE)
                 ),
                 column(width=9,
                        plotOutput('heatmap')
                 )
               ))
    )
  )
)