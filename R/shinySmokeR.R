#' Shiny app to make the application of the smoking score as easy as possible
#'
#' @import shinydashboard
#' @import shiny
#' @import DT
#' @import data.table
#' @return \code{shinySmokeR} to launch shiny app
#' @examples
#' \dontrun{
#'  shinySmokeR()
#'  }
#' @export

shinySmokeR <- function(){
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    dashboardPage(skin = "black",
                  dashboardHeader(title= "shinySmokeR"),
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Dashboard", tabName="dashboard", icon = icon("dashboard")),
                      menuItem("Data upload", tabName = "upload", icon = icon("database"),
                               radioButtons("fileType", "Choose file type for upload", c(".csv", ".rds")), 
                               fileInput("file1", "Choose CpG File",
                                         multiple = FALSE,
                                         accept = c("text/csv/rds",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv", ".rds"))),
                      menuItem("Generate report", tabName="report", icon = icon("table"),
                               textInput("study", "Enter Study Name"),
                               downloadButton('downloadReport', label = "Report")),
                      menuItem("Help", tabName="instructions", icon = icon("map-signs")))),
                  
                  
                  dashboardBody(
                    tabItems(
                      tabItem(tabName="dashboard",
                              fluidRow(          
                                box(title = "Displaying CpG data after upload",
                                    DT::dataTableOutput("contents"), color="black", solidHeader = TRUE, width = 12) #status = "primary"
                              ),
                              
                              fluidRow(
                                box(title="Prediction Results", verbatimTextOutput("ConfusionMatrix"), color="black" , solidHeader = TRUE),
                                box(title="Score Classification",plotOutput("ScoreClass"), color="black", solidHeader = TRUE))),
                      tabItem(tabName="instructions", h2("How to use this app:")))
                  ))) 
  
  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 800*1024^2)
    output$contents <- DT::renderDataTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      #df <- read.csv(input$file1$datapath, header =T)
      if (input$fileType %in% ".rds"){
        df <- readRDS(input$file1$datapath)
      }
      if (input$fileType %in% ".csv"){
        df <- read.csv(input$file1$datapath)
      }
      
      df <- na.omit(df)
      
      #return(head(df))
      return(DT::datatable(head(df), options = list(scrollX = TRUE, lengthMenu = c(5, 10), pageLength = 5, widthMenu= 10)))
    })
    
    
    output$ScoreClass <- renderPlot({
      req(input$file1)
      if (input$fileType %in% ".rds"){
        df <- readRDS(input$file1$datapath)
      }
      if (input$fileType %in% ".csv"){
        df <- read.csv(input$file1$datapath)
      }
      df <- na.omit(df)
      df$predictedScore <- smokeScore(df,ARRAY = "450k", class="class") 
      plot1 <- df %>%
        drop_na() %>%
        ggplot(aes(predictedScore, fill=mat_smk)) + geom_bar() + theme_minimal()
      plot1
    })
    
    
    
    output$ConfusionMatrix <- renderPrint({
      req(input$file1)
      if (input$fileType %in% ".rds"){
        df <- readRDS(input$file1$datapath)
      }
      if (input$fileType %in% ".csv"){
        df <- read.csv(input$file1$datapath)
      }
      df <- na.omit(df)
      df$predictedScore <- smokeScore(df,ARRAY = "450k", class="class")
      confusionMatrix(df$predictedScore, df$mat_smk)
    })
    
    
    
    # Create an rmarkdown report
    output$downloadReport <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(system.file("rmd", "report.Rmd", package = "DNAsmokeR"), tempReport, overwrite = TRUE)
        
        if (input$fileType %in% ".rds"){
          df <- readRDS(input$file1$datapath)
        }
        if (input$fileType %in% ".csv"){
          df <- read.csv(input$file1$datapath)
        }
        df <- na.omit(df)
        df$predictedScore <- smokeScore(df,ARRAY = "450k", class="class")
        
        sampsize <- length(df$predictedScore)
        studyInput <- as.character(input$study)
        
        # Set up parameters to pass to Rmd document
        params <- list(prediction =  confusionMatrix(df$predictedScore, df$mat_smk), 
                       study = studyInput, sampsize = sampsize)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}
