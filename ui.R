library("dplyr")
library("shiny")
library("ggplot2")
library("lintr")
library("ggthemes")
library("plotly")

# source in the data
source("processData.R")
source("script/chart_one_data.R")


# choices assign for y_var widget in chart-two (histogram)
select_value <- unique(chart_one_data[, "Year"])

shinyUI(navbarPage("Seattle Crime vs. Safety",
                   # first page: Intro
                   tags$head(
                     tags$style(HTML(".home_page {background-image: url(policecar.jpg);
                                     overflow: hidden;
                                     background-size: cover;
                                     position: absolute;
                                     left: 0;
                                     height:100%;}"))),
                   tabPanel("Home",
                            tags$h4("Welcome"),
                            class = "home_page",
                            fluidRow(
                              column(4,
                                     wellPanel(
                                       tags$h2("General Information"),
                                       # contents
                                       tags$p(HTML(paste0(tags$strong("Seattle Police Department Police Report Incident "), "retrieved from ", tags$a(href = "https://dev.socrata.com/foundry/data.seattle.gov/y7pv-r3kh", "City of Seattle Open Data Portal"), ", is the dataset our group chose to work with. This dataset contains details about incidents in Seattle according to the police reports filed by responding officers. The information for a particular incident is published to the dataset 6-12 hours after the corresponding police report is filed. ", "The dataset contains over ", tags$em("1,000,000 rows spanning 1990 to 2018. "), "However, the system for publishing data was adopted by the Seattle Police in 2010, and all records prior to 2010 are only added if the incident is revisited. This means that the vast majority of the data is from ", tags$em("2010-2018."))))
                                     )
                              ),

                              column(4,
                                     wellPanel(
                                       tags$h2("Target Audiences"),
                                       # contents
                                       tags$p(HTML(paste0("For a crime report based on a particular area, almost everyone living near the area is likely to be concerned with safety and will, therefore, find the report relevant. We have focused on Seattle in general and highlighting the incidents occurred around the periphery of UW campus. Though the report will be useful to anyone in this area, we will look to serve ", tags$strong(tags$span(style = "color:red", "students ")), "in particular. Our goal is to present insights from the data and thereby provide our audience with a better sense of how to live safely.")))
                                     )
                              ),

                              column(4,
                                     wellPanel(
                                       tags$h2("Analysis"),
                                       # contents
                                       tags$p(HTML(paste0("The specific questions our project will answer for our audience are as following:", tags$br(), tags$br(), "(1) What is the frequency of incidents based on type?", tags$br(), "(2) Is there any location that is particularly dangerous? What type of incident occurred the most in this given location?", tags$br(), "(3) Which times of day are the safest? Which are most dangerous? Is there a correlation between time of day and the types of crime committed?", tags$br(), "(4) Is the safety around UW campus trending up or down?")))
                                     )
                              )
                            )
                   ),
                   # second page: chart one
                   tabPanel("Crime occurance from 2010 to 2018",
                            # Add a select input for the x variable
                            selectInput(
                              "year_choice",
                              label = "Year",
                              choices = select_value,
                              selected = "2018"
                            ),
                            fluidRow(
                              column(9,
                                     tags$h1("Crime Type by Year")
                                     ),
                                column(9,
                                      mainPanel(
                                        tags$link(rel = "stylesheet",
                                                  type = "text/css",
                                                  href = "style.css"),
                                        plotlyOutput("hist", height = 500,
                                                     width = 900)
                                      )
                                ),
                                column(3,
                                      wellPanel(
                                        h2("Interpretation"),
                                        tags$p("This chart shows the number of
                                               crime according to types in a
                                               sleceted year. Using this chart,
                                               we are able to identify what
                                               type of crime is the one with
                                               the higherst number of
                                               occurences in the whole Seattle
                                               area. To make the graph more
                                               condensed, we have disregarded
                                               the details of some types of
                                               crimes. For example, we do not
                                               differentiate what property or
                                               elements are involved in the
                                               THEFT category. Instead, we treat
                                               mail theft and shoplifting as
                                               THEFT altogehter")
                                      )
                                )
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
                         select = "year_crime"
                       )
                     ),
                     mainPanel(plotOutput("duy_plot"))
                   )),
                   # fourth page
                   tabPanel("Crime Shown In Time Series Chart",
                            sidebarLayout(sidebarPanel(
                              selectInput(
                                "choice_three",
                                label = h2("Choose an option:"),
                                choices = list(
                                  "Year" = 1,
                                  "week" = 2,
                                  "hour" = 3
                                ),
                                selected = 1
                              ),
                              selectInput(
                                "year",
                                label = h2("Year since:"),
                                choices = select_value,
                                selected = 1
                              )
                              
                            ),
                            mainPanel(plotlyOutput("chart_three"))
                            )
                   ),
                   # fifth page: basic stats computation
                   tabPanel("Summary"),
                   # Sixth page: Team info
                   tabPanel("About",
                            fluidRow(
                              column(12,
                                     tags$h1("Team Members"),
                                     tags$p(class = "center", HTML(paste0("We are a group of students studying at the University of Washington, and this is our", tags$strong(" final project "), "for the class: INFO201.", tags$br(), "We are especially interested in the safty issue being part of the member of the Seattle community.", tags$br(), "Hence, we have dedicated our time analyzing the data provied by Seatlle PD.")))
                                     ),

                                column(3,
                                       tags$h5("Chloe"),
                                       tags$img(src = "Chloe.jpg",
                                                class = "img1")
                                      ),
                                column(3,
                                       tags$h5("Robin"),
                                       tags$img(src = "Robin.jpg",
                                                class = "img1")
                                ),
                                column(3,
                                       tags$h5("Duy"),
                                       tags$img(src = "Duy.jpg",
                                                class = "img1")
                                ),
                                column(3,
                                       tags$h5("Daniel"),
                                       tags$img(src = "Daniel.jpg",
                                                class = "img1")
                                )
                              )
                   )
))
