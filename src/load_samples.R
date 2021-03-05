


load_samples <- function(sampleDF,creds)
{

log <- sampleDF

log$barcode <- rep("",nrow(sampleDF))
  
log$loadResult <- rep("",nrow(sampleDF))


loadError <- F 
  
for(i in 1:nrow(sampleDF)){

  
  
  
#Convert NA to ""
  
  sampleDF[i,is.na(sampleDF[i,])[1,] ] <- "" 
  
#NA1
  
  # attributes <- list(
  #   STRAIN = sampleDF$`MOUSE STRAIN`[i],
  #   FREQ_DOB = sampleDF$DOB,
  #   SEX = sampleDF$SEX,
  #   FREQ_AGE = ifelse(sampleDF$`Age (wks)`[i] =="","", paste0(sampleDF$`Age (wks)`[i]," wks")),
  #   FREQ_DEAFENING_METHOD = sampleDF$`Deafening method`[i],
  #   FREQ_IT_INJECTION_TIME_AGE = sampleDF$`IT inj time Age`[i],
  #   FREQ_SAC_TIME_AGE = sampleDF$`sac time Age`[i],
  #   FREQ_FIXATION = sampleDF$FIXATION[i],
  #   FREQ_DELCALCIFICATION = sampleDF$DECAL[i],
  #   FREQ_FORMULATION = sampleDF$`Formulation(FT or Vehicle)`[i],
  #   FREQ_ANIMAL_NUMBER= sampleDF$`Animal #`[i],
  #   FREQ_VENDOR = sampleDF$Vendor[i]
  # )
  
  
#NA1TEST   
  attributes <- list(
    NA_CEP_STRAIN = sampleDF$`MOUSE STRAIN`[i],
    NA_CEP_DOB = paste0(as.character(sampleDF$DOB[i]),"T00:00:00Z"),
    NA_CEP_SEX = sampleDF$SEX[i],
    FREQ_AGE = sampleDF$`Age (wks)`[i],
    FREQ_DEAFENING_METHOD = sampleDF$`Deafening method`[i],
    FREQ_IT_INJECTION_TIME_AGE = sampleDF$`IT inj time Age (wks)`[i],
    FREQ_SAC_TIME_AGE = sampleDF$`sac time Age (wks)`[i],
    FREQ_FIXATION = sampleDF$FIXATION[i],
    FREQ_DECALCIFICATION = sampleDF$DECAL[i],
    FREQ_FORMULATION = sampleDF$`Formulation(FT or Vehicle)`[i],
    NA_CEP_ANIMAL_NUM= sampleDF$`Animal #`[i],
    FREQ_VENDOR = sampleDF$Vendor[i]
  )
  
#Validate that all required fields are not empty  
  
  
all_required <- all(sampleDF[i,c(1,4:12)] != "")  
  

#Validate Uniqueness


isUnique <- checkMouseSampleUniqueness(creds, sampleDF$`Animal #`[i], sampleDF$Vendor[i])  

if(isUnique & all_required ) { 

print(isUnique )  

  
loadResult <- tryCatch(
  
  {   
  
  
    sample <- CoreAPIV2::createEntity(coreApi = creds,entityType = "MOUSE",body = attributes,useVerbose = F)

#record barcode

    log$barcode[i] <- sample$entity$Barcode


#Create lot 1

    lot_att <- list(FREQ_NEXT_STEP = "Dissection")


    lot1 <- createSampleLot(coreApi = creds,sampleType = "MOUSE",body = lot_att, sampleBarcode = sample$entity$Barcode ,useVerbose = F)

#Create lot 2

    lot_att <- list(FREQ_NEXT_STEP = "On Hold")
  
    lot2 <- createSampleLot(coreApi = creds,sampleType = "MOUSE",body = lot_att, sampleBarcode = sample$entity$Barcode,useVerbose = F )

    incProgress( i/nrow(sampleDF))
  
   Sys.sleep(1)
   message("Sample and lots created")
  } ,

error=function(cond) {
  loadError <- TRUE
  
  incProgress( i/nrow(sampleDF))
  
  Sys.sleep(1)
  
   return("Sample and lots failed to be loaded, created")
     
   }


) #end tryCatch

log$loadResult[i] <- loadResult

} else {   log$loadResult[i] <- "SAMPLE NOT LOADED, "
           loadError <- T
          if(!isUnique ) { 
           log$loadResult[i] <- paste(log$loadResult[i],
                                      "Animal Number and Vendor are already in LIMS, ") 
            }
           if(!all_required) { 
             log$loadResult[i] <- paste(log$loadResult[i],
                                        "Sample missing required value") 
             }
            
          
        }

}

if(loadError) {  incProgress( i/nrow(sampleDF)) 
                 showModal(modalDialog( title = "Sample Load Error",
                                          "One or more sample was not loaded.  Download the Load Report for details.",
                                          easyClose = FALSE,
                                          footer =   tagList( modalButton("OK"))
                                        )
                           )
                
                   Sys.sleep(5)
                }

log

}


