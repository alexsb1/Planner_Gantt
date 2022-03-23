# Description: This is an interactive Shiny app that imports an Excel file
# from Microsoft Planner and presents the data as a Gantt chart.
#
#
# Author: Alex Searle-Barnes
# https://http://alexsb1.github.io
# Date: 22 March 2022


# Load libraries
library(shiny)
library(tidyverse)
library(readxl)
library(ganttrify)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Visualise Planner as a Gantt chart"),
  
  # fluid row layout with input and output definitions ----
  fluidRow(
    wellPanel(
      
      # Sidebar panel for inputs ----
      column(4,
             # Input: Select a file ----
             fileInput("file1", "Select your Planner output file",
                       multiple = FALSE,
                       accept = c(".xlsx") # The Planner default output
             )
      ),
      
      column(6,
             dateRangeInput("dateRange",
                            "Select a date range to display",
                            start = Sys.Date() - 60,
                            end = Sys.Date() + 60
             ),
      ),
      
      column(2,
             checkboxInput("showWP", "Hide group headers", value = FALSE),
             actionButton("buttonReset", "Reset view")
      ),
      br(),
      hr()
    ),
  ),
  
  # Main panel for displaying outputs ----
  fluidRow(
    #textOutput("date"), #for testing
    #textOutput("date2"), #for testing
    textOutput("GanttError"),
    plotOutput("Gantt", width = "100%", height = "2000px"),
    hr()
  ),
  
  # Footer
  fluidRow(
    column(6,
           "Built by ",
           tags$a(href="http://alexsb1.github.io/", target = "_blank", "Alex Searle-Barnes")
    ),
    column(6,
           "Source code is ",
           tags$a(href="https://github.com/alexsb1/Planner_Gantt", target = "_blank", "available on GitHub"),
           
    ),
    hr(),
    br()
  )
  
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  plannerData <- reactive({
    req(input$file1)
    df <- read_excel(input$file1$datapath,
                     sheet = "Tasks")
    
    df <- df %>% rename("taskID" = "Task ID",
                        "taskName" = "Task Name",
                        "bucketName" = "Bucket Name",
                        "assignedTo" = "Assigned To",
                        "createdBy" = "Created By",
                        "createdDate" = "Created Date",
                        "startDate" =  "Start Date",
                        "dueDate" = "Due Date",
                        "completedDate" = "Completed Date",
                        "completedBy" = "Completed By",
                        "completedChecklistItems" = "Completed Checklist Items",
                        "checklistItems" = "Checklist Items"
    ) %>% 
      
      separate_rows("checklistItems", sep = ";", convert = FALSE) %>% 
      
      mutate(across(c(startDate, dueDate, completedDate), as.character)) #Prepare for conversion to posix date
    
  })
  
  #This works displaying a table, is reactive
  output$contents <- renderDataTable(plannerData())
  
  
  # Clean Gantt Chart dataframe
  dfGantt <- reactive({
    df2 <- plannerData()
    df2$start_date <- as.Date(df2$createdDate, format = "%m/%d/%Y")
    df2$end_date <- as.Date(df2$dueDate, format = "%m/%d/%Y")
    df2$activity <- df2$checklistItems
    df2$wp <- df2$bucketName
    df2 <- df2 %>% as.data.frame() %>% drop_na(c(start_date, end_date))
    df2 <- df2 %>% filter(between(start_date, input$dateRange[1], input$dateRange[2])) #This is aggressive filtering
  })
  
  # output$contents2 <- renderDataTable(dfGantt()) #for testing
  
  # Test date display output
  # output$date2 <- renderText(as.character(dfGantt()[[1,18]])) #for testing
  # output$date <- renderText(as.character(as.Date(input$dateRange[1], format = "%Y-%m-%d"))) #for testing
  
  Gantt <- reactive({
    ganttrify(
      project = dfGantt(),
      month_number_label = FALSE,
      by_date = TRUE,
      project_start_date = as.character(input$dateRange[1]),
      hide_wp = input$showWP
    )#+
    #ggplot2::coord_cartesian(xlim =c(as.Date(input$dateRange[1], format = "%Y-%m-%d"), as.Date(input$dateRange[1], format = "%Y-%m-%d")))
  })
  
  output$Gantt <- renderPlot(Gantt())
  
  observeEvent(input$buttonReset, {
    updateDateRangeInput(session,
                         "dateRange",
                         start = Sys.Date() - 60,
                         end = Sys.Date() + 60)
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)
