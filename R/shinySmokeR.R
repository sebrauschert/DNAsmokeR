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
                               radioButtons("array", "Choose array type", c("450K", "EPIC")),
                               radioButtons("fileType", "Choose file type for upload", c(".csv", ".rds")), 
                               fileInput("file1", "Choose CpG File",
                                         multiple = FALSE,
                                         accept = c("text/csv/rds",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv", ".rds"))),
                      menuItem("Generate report", tabName="report", icon = icon("table"),
                               textInput("study", "Enter Study Name"),
                               textInput("normalization", "Enter Normalization Method"),
                               textInput("email", "Enter Contact Email"),
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
                                box(title="ROC curve and AUC",plotOutput("ScoreClass"), color="black", solidHeader = TRUE))),
                      tabItem(tabName="instructions", h2("For issues and questions about this app, please email Sebastian.Rauschert@telethonkids.org.au")))
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
        df <- as.data.frame(readRDS(input$file1$datapath))
      }
      if (input$fileType %in% ".csv"){
        df <- read.csv(input$file1$datapath)
      }
      
      df <- na.omit(df)
      return(DT::datatable(head(df), options = list(scrollX = TRUE, lengthMenu = c(5, 10), pageLength = 5, widthMenu= 10)))
    })
    
    
    output$ScoreClass <- renderPlot({
      if (input$array %in% "450K"){
        array = "450k"
      }
      if (input$array %in% "EPIC"){
        array = "EPIC"
      }
      
      req(input$file1)
      if (input$fileType %in% ".rds"){
        df <- as.data.frame(readRDS(input$file1$datapath))
      }
      if (input$fileType %in% ".csv"){
        df <- read.csv(input$file1$datapath)
      }
      
      df <- na.omit(df)
      df <- df %>%
        mutate(mat_smk = as.factor(ifelse(preg_smk %in% 0, "not_exp", 
                                          ifelse(preg_smk %in% 1, "smoke_exp", NA)))) %>%
        select(-preg_smk)
      
      df$predictedScore <- smokeScore(df, ARRAY = array, class="prob")
      
      ROC <- df %>%
        ggplot(aes(d=mat_smk, m=predictedScore)) + 
        geom_roc(n.cuts = 0) + 
        geom_abline(intercept=0, slope=1, linetype="dashed", col="black") + 
        style_roc(theme = theme_minimal)
      
      roc_value <- round(calc_auc(ROC)$AUC[1],3)
      
      plot1 <- df %>%
        ggplot(aes(d=mat_smk, m=predictedScore)) + 
        geom_roc(n.cuts = 0) + 
        geom_abline(intercept=0, slope=1, linetype="dashed", col="black") + 
        style_roc(theme = theme_minimal) +
        annotate("text", x=.75, y=.25, label=paste("AUC = ",roc_value), color="#F8766D")
      plot1
      
    })
    
    
    
    output$ConfusionMatrix <- renderPrint({
      if (input$array %in% "450K"){
        array <- "450k"
      }
      if (input$array %in% "EPIC"){
        array <- "EPIC"
      }
      
      req(input$file1)
      if (input$fileType %in% ".rds"){
        df <- as.data.frame(readRDS(input$file1$datapath))
      }
      if (input$fileType %in% ".csv"){
        df <- read.csv(input$file1$datapath)
      }
      df <- na.omit(df)
      df <- df %>%
        mutate(mat_smk = as.factor(ifelse(preg_smk %in% 0, "not_exp", 
                                          ifelse(preg_smk %in% 1, "smoke_exp", NA)))) %>%
        select(-preg_smk)
      
      df$predictedScore <- smokeScore(df, ARRAY = array, class="class")
      confusionMatrix(df$predictedScore, as.factor(df$mat_smk))
    
    })
    
    
    
    # Create an rmarkdown report
    output$downloadReport <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        
        tempReportRmd <- file.path(tempdir(), "report.Rmd")
        
        dir.create(paste0(tempdir(),'/new_folder'))
        tempFolder <- paste0(tempdir(),'/new_folder')
        file.copy(system.file("rmd", "report.Rmd", package = "DNAsmokeR"), tempFolder, overwrite = TRUE)
        file.copy(system.file("rmd", "resources", package = "DNAsmokeR"), tempFolder, recursive=TRUE)
        file.copy(system.file("rmd", "lifecycle-large.jpg", package = "DNAsmokeR"), tempFolder, overwrite = TRUE)
        
        if (input$array %in% "450K"){
          array <- "450k"
        }
        if (input$array %in% "EPIC"){
          array <- "EPIC"
        }
        
        if (input$fileType %in% ".rds"){
          df <- as.data.frame(readRDS(input$file1$datapath))
        }
        if (input$fileType %in% ".csv"){
          df <- read.csv(input$file1$datapath)
        }
        df <- na.omit(df)
        df <- df %>%
          mutate(mat_smk = as.factor(ifelse(preg_smk %in% 0, "not_exp", 
                                            ifelse(preg_smk %in% 1, "smoke_exp", NA)))) %>%
          select(-preg_smk)
        
        df$predictedScore <- smokeScore(df,ARRAY = array, class="class")
        
        # Info on the study
        sampsize    <- length(df$predictedScore)
        studyInput  <- as.character(input$study)
        normInput   <- as.character(input$normalization)
        emailInput  <- as.character(input$email)
        arrayInput  <- as.character(input$array)
        
        # Number of CpGs not in the Study
        numberDIFF <- length(setdiff(I450K$CpG[2:205], names(df)[names(df) != "mat_smk"]))
        CpGmissing <- setdiff(I450K$CpG[2:205], names(df)[names(df) != "mat_smk"])
        df$percentScore <- smokeScore(df, ARRAY = array, class="prob")
        
        # Make Roc Curve available
        ROC <- df %>%
          ggplot(aes(d=mat_smk, m=percentScore)) + 
          geom_roc(n.cuts = 0) + 
          geom_abline(intercept=0, slope=1, linetype="dashed", col="black") + 
          style_roc(theme = theme_minimal)
        
        roc_value <- round(calc_auc(ROC)$AUC[1],3)
        
        AUC_ROC <- df %>%
          ggplot(aes(d=mat_smk, m=percentScore)) + 
          geom_roc(n.cuts = 0) + 
          geom_abline(intercept=0, slope=1, linetype="dashed", col="black") + 
          style_roc(theme = theme_minimal) +
          annotate("text", x=.75, y=.25, label=paste("AUC = ",roc_value), color="#F8766D")
        
        # Set up parameters to pass to Rmd document
        params <- list(prediction =  confusionMatrix(df$predictedScore, as.factor(df$mat_smk)), 
                       study = studyInput, sampsize = sampsize, normalization = normInput, 
                       email=emailInput, rocCurve = AUC_ROC, array = arrayInput,
                       numberCpG = numberDIFF,
                       missingCpG = CpGmissing)
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        tempReportRmd <- paste0(tempFolder,'/report.Rmd')
        rmarkdown::render(tempReportRmd, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
}
