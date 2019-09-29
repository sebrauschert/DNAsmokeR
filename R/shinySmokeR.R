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
                  dashboardHeader(title= "shinySmokeR" #div(img(src="LifeCycle.png",
                                                # title = "shinySmokeR", height = "40px"),
                                             #style = "padding-top:1px; padding-bottom:1px;")),
                  ),
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
                      menuItem("Help", tabName="instructions", icon = icon("map-signs")))),
                  
                  
                  #dashboardBody(),
                  
                  
                  dashboardBody(
                    tabItems(
                      tabItem(tabName="dashboard",
                              fluidRow(          
                                box(title = "Displaying CpG data after upload",
                                    DT::dataTableOutput("contents"), color="black", solidHeader = TRUE, width = 12) #status = "primary"
                              ),
                              
                              fluidRow(
                                box(title="Prediction Results", verbatimTextOutput("ConfusionMatrix"), color="black" , solidHeader = TRUE),
                                box(title="Score Classification",plotOutput("ScoreClass"), color="black", solidHeader = TRUE)
                              )),
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
    
    output$Score <- renderPlot({
      
      req(input$file1)
      
      if (input$fileType %in% ".rds"){
        df <- readRDS(input$file1$datapath)
      }
      if (input$fileType %in% ".csv"){
        df <- read.csv(input$file1$datapath)
      }
      
      df <- na.omit(df)
      
      df$predictedScore <- smokeScore(df,ARRAY = "450k", class="prob")#  predict(smkScore,df[,features], type="prob")[,1]
      
      df %>%
        drop_na() %>%
        ggplot(aes(predictedScore)) + geom_density() + theme_minimal()
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
      
      df$predictedScore <- smokeScore(df,ARRAY = "450k", class="class") #predict(smkScore,df[,features])
      
      
      plot1 <- df %>%
        drop_na() %>%
        ggplot(aes(predictedScore, fill=mat_smk)) + geom_bar() + theme_minimal()
      
      #saveRDS(plot1, "tmp/ScoreClass.rds")
      
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
      
      df$predictedScore <- smokeScore(df,ARRAY = "450k", class="class")#predict(smkScore, newdata=df[,features])
      
      #saveRDS(confusionMatrix(df$predictedScore, df$mat_smk), "tmp/Environment.rds")
      
      confusionMatrix(df$predictedScore, df$mat_smk)
    })
    
    output$downloadReport <- downloadHandler(
      filename = 'my-report.html',
      content = function(file) {
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        #tempReport <- file.path(tempdir(), "report.Rmd")
        #file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        # rmarkdown::render(tempReport, output_file = file,
        #                   envir = new.env(parent = globalenv()))
        
        rmarkdown::render("report.Rmd", output_file = file,
                          envir = new.env(parent = globalenv()))
      }
    )
    
    session$onSessionEnded(function() {
      system(paste("rm -f", "tmp/Environment.rds"))
      system(paste("rm -f", "tmp/ScoreClass.rds"))
    })
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
