library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(googlesheets)
library(stringr)
library(udpipe)
library(quanteda)
library(plyr)
library(dplyr)
library(data.table)
library(rjson)
library(httr)
library(tidytext)
library(gtools)
library("V8")
source("./collocations_v2.R", local = TRUE)



ui <- dashboardPage(
  skin= "purple",
  dashboardHeader(title = "ReSWOT"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Load data", tabName = "load", icon = icon("database")),
      menuItem("Visualization", tabName = "viz", icon = icon("dashboard")))),
  dashboardBody(
    useShinyjs(),
    tabItems(
      
      tabItem( tabName = "viz",
               
    tags$head(tags$script(src="https://public.tableau.com/javascripts/api/tableau-2.min.js")),
    
    extendShinyjs(text = "shinyjs.initializeViz = function(){
     var containerDiv = document.getElementById('tableauViz');
                  url = 'https://dub01.online.tableau.com/t/relabuu/views/ReSWOT/ReSWOT';
                  var options = {
                  
                  height: '100vh',
                  width:'100%',
onFirstInteractive: function () {

                workbook = viz.getWorkbook();

                activeSheet = workbook.getActiveSheet();

            }
                  }
                  var viz = new tableau.Viz(containerDiv, url, options);
                  }"),
    tags$div(style='width: 100%; height: 100vh;',id = 'tableauViz')
  ),
  tabItem(tabName = "load",
          div(style="display: inline-block;",fileInput('file1', 'Upload a CSV file',
                    accept = c('text/csv',
                               'text/comma-separated-values',
                               '.csv'))),
          div(style="display: inline-block; float:right;", uiOutput("download")),
          htmlOutput("instructions"),
          DT::dataTableOutput('readytable')
          )
  )
    
    
  )
)

server <- function(input, output, session){
  
  values <- reactiveValues(downloadReady = FALSE)
 
  googlesheets::gs_webapp_auth_url(client_id = "YOUR_CLIENT_ID", redirect_uri = "http://127.0.0.1:6456/", access_type = "online", approval_prompt = "auto")
  sheet_key <- "YOUR_SHEET_KEY"
  ss <- googlesheets::gs_key(sheet_key)
  js$initializeViz()  
  
  data <- reactive({
    
    withProgress( message = "Processing reviews", value = 0, {
      inFile <- input$file1
      df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
      apps <- unique(df$app)
      df <- df[,c("review_id", "date", "content", "rating", "app")]
      split <- split(df , f = df$app )
      out <- data.frame()
      for (app in seq(1, length(apps))){
        incProgress(1/6, detail = paste("Processing", apps[app], sep = " "))
        out_app <- ProcessApp(split[[app]], names(split)[app])
        out <- rbind(out, out_app)
      }
      
      incProgress(1/3, detail = "Grouping similar features")
      grouped <- GroupAllCollocations(out)
      incProgress(1/3, detail = "Preparing the data for download")
      final <- ConstructFinalDF(grouped, df)
      
      final_csv <- write.csv(final, "final_csv.csv")
      
      gs_upload("final_csv.csv", "YOUR_SHEET_NAME", overwrite = TRUE)
      
      values$downloadReady <- TRUE
      return(final)
   } )}
  )
  
  output$instructions <- renderText({
    if (is.null(input$file1)) { return(paste(
      "Please submit a csv file with the reviews of 2 or more apps, according to the following format:
      <ul>
      <li> <b>review_id</b>: a string with an unique identifier for the review, e.g., app0001
      <li> <b>date</b>: the day when the review was posted, e.g., 12-01-2018
      <li> <b>content</b>: a string containing the content of the review, e.g., i love this app
      <li> <b>rating</b>: an integer with the number of stars gave by the reviewer, e.g., 4
      <li> <b>app</b>: a string containing the name of the reviewed app, e.g., facebook
      </ul>"
      )) }
    return ()
  })
  
  
  output$downloadData <- downloadHandler(
    
    filename = "collocations.csv",
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$download <- renderUI({
    if(values$downloadReady == FALSE) {return()} else {
      downloadButton("downloadData", "Download data for visualization")
      
    }
  })
  
  output$readytable <- DT::renderDataTable({
    if (is.null(input$file1)) { return() }
    data()
      
  })
  
}
shinyApp(ui, server)
