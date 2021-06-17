
library(shiny)
library(shinyjs)
library(shinydashboard)

library(DT)
library(readxl)

library(dplyr)

library(purrr)



#UI page for Mouse Sample Loader

fluidPage(
  useShinyjs(),
  titlePanel( title = "Mouse Sample Loader"),
  sidebarLayout(
    sidebarPanel (width = 2,

#Login
      textInput("user", "enter user name",""),
      passwordInput("password","enter password"),    
      actionButton("validate", "validate credentials"),
      textOutput(outputId = "logmessage"),
      
      tags$hr(),
#File upload
      
      disabled(fileInput("sample_file", 'Mouse Sample File',   
                accept="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                 )
                ),

#Download Template

 #    downloadButton("downloadData", "Download Sample Template")
        tags$a(href="https://frequencytx.box.com/s/tog25elok1atryi3buujps0qs0rviqty", 
               "Click here! to download a template")
    ),

    mainPanel( 
      
#Table display of uploaded data
      
      
      DTOutput("sample_table"),
       
#Load Samples into LIMS
      actionButton("load_samples","Load Samples into LIMS"),

      
#Download Load Report
   
      disabled(downloadButton("report","Download Load Report"))     
      
      
    )
  )
  
)


