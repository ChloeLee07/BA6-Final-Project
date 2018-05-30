library("dplyr")
library("shiny")
library("ggplot2")
library("lintr")
library("ggthemes")

# source in the data
source("script/chart_one_data.R")
source("processData.R")

# choices assign for y_var widget in chart-two (histogram)
# select_value_x <- unique(data[, "offence_type"])
select_value <- unique(chart_one_data[, "Year"])

shinyUI(navbarPage("Crime by Year",
                   # first page: Intro
                   tabPanel("Introduction"),
                   # second page: chart one 
                   tabPanel("Crime occurance from 2010 to 2018",
                            # Add a select input for the y variable
                            selectInput(
                              "year_choice",
                              label = "Year",
                              choices = select_value,
                              selected = "2018"
                            ),
                            # Plot the output with the name "hist"
                            mainPanel(
                              tags$link(rel = "stylesheet",
                                        type = "text/css",
                                        href = "style.css"),
                              tags$h1("Offense Type by Year"),
                              #h2("Interpretation"),
                              tags$p("This chart shows the the number of crime
                                     according to types in a sleceted year.
                                     As we can see from the chart, Theft"),
                              plotOutput("hist")
                            )
                            ),
                   # third page
                   tabPanel(
                     "Chart Two",
                     sidebarLayout(sidebarPanel(
                       radioButtons(
                         "radiovar",
                         label = h2("Choose an option:"),
                         choices = list(
                           "UW Crime Distribution By Year" = "year_crime",
                           "Most Dangerous Place At UW" = "dangerous_place",
                           "UW Crime Occurrances At Nighttime" = "nighttime",
                           "UW Crime Occurrances At Daytime" = "daytime"
                         ),
                         select = 1
                       )
                     ),
                     mainPanel(plotOutput("duy_plot"))
                   )), 
                   # fourth page
                   tabPanel("Chart Three"),
                   # fifth page: basic stats computation
                   tabPanel("Summary"),
                   # Sixth page: Team info
                   tabPanel("About")  
))
