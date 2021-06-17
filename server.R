

debug <- TRUE

source("src/sourceDir.R")
sourceDir("src/")



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
                               
                                
                                MouseSampleTemplate <- read_excel(input$sample_file$datapath, 
                                                                  col_types = c(rep("text",2),"date",rep("text",11)),  
                                                                  skip = 1) 
                                                                  
                                
                                
                                MouseSampleTemplate$DOB <- as.character(MouseSampleTemplate$DOB)
                                MouseSampleTemplate
                              }
                                
                           
                               
                               })
  
    

loadReport <- reactiveVal()
tempfile_name <- reactiveVal(tempfile())

## Display Sample Table
  
output$sample_table <- renderDT({sampleTable()})  
  
  
## Load samples


  
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
  
    account <- CoreAPIV2::coreAPI("Credentialsfreq.txt")
    
    account$user <- input$user
    account$pwd<- input$password
    
   creds <- CoreAPIV2::authBasic(account)$coreApi
    
    print(str(sampleTable()) )
    
    print(nrow(sampleTable()))
    
    withProgress(message = "Loading Samples",value = 0,{  
   
       report <- load_samples(sampleTable(),creds)
 
   
    })
    print(report)
    
    lo<-CoreAPIV2::logOut(creds,useVerbose = debug)
    
    
    write.csv(x = report,file = tempfile_name(),row.names = F)
    
  
    
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