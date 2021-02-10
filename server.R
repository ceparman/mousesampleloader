library(shiny)
library(shinyjs)
library(shinydashboard)

library(DT)
library(readxl)

library(dplyr)


source("src/sourceDir.R")
sourceDir("src/")



options(shiny.maxRequestSize=600*1024^2)
shinyServer(function(input, output, session){
  
  
  #Template Download
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste0("MouseSampleTemplate.xlsx")
    },
    content = function(file) {
      
      file.copy(from = "www/MouseSampleTemplate.xlsx", to = file)
      
    }
    ,contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )  
  
  
  #Validate Credentials
  
  observeEvent(input$validate,{
    
    account <- CoreAPIV2::coreAPI("Credentialsfreq.txt")
    
    account$user <- input$user
    account$pwd<- input$password
    
    output$logmessage <- renderText(
      tryCatch(
        {login <- CoreAPIV2::authBasic(account)
        lo<-CoreAPIV2::logOut(login$coreApi)
        enable("sample_file")
        "Credentials Validated"
        }
        ,
        error =function(cond){
          return("Credentials not accepted")  
        } 
        
      )
    )
  })
    
##Reactive Sample Table data
  
  
sampleTable <- reactive({    
                              if(is.null(input$sample_file)) {
                                
                              readRDS("www/blankTable.RDS") 
                              } else{
                               
                                
                                MouseSampleTemplate <- read_excel(input$sample_file$datapath, col_types = rep("text",12),
                                                                  skip = 1) 
                                
                                
                              }
                                
                              
                               
                               })
  
    

loadReport <- reactiveVal()
tempfile_name <- reactiveVal(tempfile())

## Display Sample Table
  
output$sample_table <- renderDT({sampleTable()})  
  
  
## Load samples

# observeEvent(input$load_samples,{
#   
#   #Alert
#   
#   n_samples_to_load <- nrow(sampleTable())
#   
#  # showModal(modalDialog(title =paste("You are about to add",n_samples_to_load,"samples")),
#   #                       paste("Proceed?, This can not by undone"))
#   showModal(modalDialog(title ="Error Validating Formulations ",
#                         paste("You must select at least one compound")))
#  
  
  dataModal <- function() {
    n_samples_to_load <- nrow(sampleTable())
    modalDialog( title = paste("You are about to add",n_samples_to_load,"samples"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.
  observeEvent(input$load_samples, {
    showModal(dataModal())
  })
  
  
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
   
    progress <- Progress$new(session)
    
    progress$set(message = paste('Loading Samples'),
                 detail = 'This may take a while...')
    
    account <- CoreAPIV2::coreAPI("Credentialsfreq.txt")
    
    account$user <- input$user
    account$pwd<- input$password
    
   creds <- CoreAPIV2::authBasic(account)$coreApi
    
    print(str(sampleTable()) )
    
    for(i in 1 :nrow(sampleTable())) {
      
      
      
      loadReport(load_samples(sampleTable(),creds))
      
      progress$inc(amount = 1/nrow(sampleTable()))
      Sys.sleep(1)
    }
    
    progress$close()
    
    
    
    lo<-CoreAPIV2::logOut(creds) 
    
    
    write.csv(x = loadReport(),file = tempfile_name(),row.names = F,col.names = T)
    
  
    
    enable("report")
    removeModal()
    
  })
  
  
  output$report <- downloadHandler(
 
    filename = function() {
      
      paste0("SampleLoadReport.csv")
    },
    content = function(file) {
      
      file.copy(from = tempfile_name(), to = file)
      
    }
    ,contentType = "text/csv"
 
    )  
  
  
  






})