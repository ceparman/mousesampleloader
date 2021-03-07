


load_samples <- function(sampleDF,creds)
{

loadlog <- sampleDF

loadlog$barcode <- rep("",nrow(sampleDF))
  
loadlog$loadResult <- rep("",nrow(sampleDF))


print(loadlog)

loadError <- F 
  
for(i in 1:nrow(sampleDF)){

  
  
  
#Convert NA to ""
  
  sampleDF[i ,is.na(sampleDF[i,])[1,] ] <- "" 
  
#NA1
  
  attributes <- list(
    STRAIN = sampleDF$`MOUSE STRAIN`[i],
    FREQ_DOB = paste0(as.character(sampleDF$DOB[i]),"T00:00:00Z"),
    SEX = sampleDF$SEX[i],
    FREQ_AGE = sampleDF$`Age (wks)`[i],
    FREQ_DEAFENING_METHOD = sampleDF$`Deafening method`[i],
    FREQ_IT_INJECTION_TIME_AGE = sampleDF$`IT inj time Age (wks)`[i],
    FREQ_SAC_TIME_AGE = sampleDF$`sac time Age (wks)`[i],
    FREQ_FIXATION = sampleDF$FIXATION[i],
    FREQ_DELCALCIFICATION= sampleDF$DECAL[i],
    FREQ_FORMULATION = sampleDF$`Formulation(FT or Vehicle)`[i],
    FREQ_ANIMAL_NUMBER= sampleDF$`Animal #`[i],
    FREQ_VENDOR = sampleDF$Vendor[i]
  )
  

  
# #NA1TEST
# attributes <- list(
#   NA_CEP_STRAIN = sampleDF$`MOUSE STRAIN`[i],
#   NA_CEP_DOB = paste0(as.character(sampleDF$DOB[i]),"T00:00:00Z"),
#   NA_CEP_SEX = sampleDF$SEX[i],
#   FREQ_AGE = sampleDF$`Age (wks)`[i],
#   FREQ_DEAFENING_METHOD = sampleDF$`Deafening method`[i],
#   FREQ_IT_INJECTION_TIME_AGE = sampleDF$`IT inj time Age (wks)`[i],
#   FREQ_SAC_TIME_AGE = sampleDF$`sac time Age (wks)`[i],
#   FREQ_FIXATION = sampleDF$FIXATION[i],
#   FREQ_DECALCIFICATION = sampleDF$DECAL[i],
#   FREQ_FORMULATION = sampleDF$`Formulation(FT or Vehicle)`[i],
#   NA_CEP_ANIMAL_NUM= sampleDF$`Animal #`[i],
#   FREQ_VENDOR = sampleDF$Vendor[i]
# )
  
#Validate that all required fields are not empty  
  
  
all_required <- all(sampleDF[i,c(1,4:12)] != "")  
  

#Validate Uniqueness

#NA1PROD

isUnique <- checkMouseSampleUniqueness(creds, sampleDF$`Animal #`[i], sampleDF$Vendor[i])  
print(isUnique ) 


if(isUnique & all_required ) { 

 safecreateEntity <- purrr::safely(.f=CoreAPIV2::createEntity)
 safecreateSampleLot <- purrr::safely(.f=local_createSampleLot)

         #Create sample 
   
         sample <- safecreateEntity(coreApi = creds,entityType = "MOUSE",body = attributes,useVerbose = T)

        
        #Create lot 1
        
        lot_att <- list(FREQ_NEXT_STEP = "Dissection")
        
        
        lot1 <- safecreateSampleLot(coreApi = creds,sampleType = "MOUSE",body = lot_att, sampleBarcode = sample$result$entity$Barcode ,useVerbose = T)
        
        #Create lot 2
        
        lot_att <- list(FREQ_NEXT_STEP = "On Hold")
        
        lot2 <- safecreateSampleLot(coreApi = creds,sampleType = "MOUSE",body = lot_att, sampleBarcode = sample$result$entity$Barcode,useVerbose = T )
        
         incProgress( i/nrow(sampleDF))
        
        Sys.sleep(1)
        
        
     if( ( is.null(sample$error) & is.null(lot1$error) & is.null(lot2$error) )) {
       message("Sample and lots created")
       #record barcode
       
       loadlog$barcode[i] <- sample$result$entity$Barcode
       
       loadlog$loadResult[i] <- "Sample and lots created"
       
       
      } else{

   loadError <- TRUE
  
   incProgress( i/nrow(sampleDF))
  
  Sys.sleep(1)
  
  loadlog$loadResult[i] <- "Sample and lots failed to be loaded, created"
     
  if(is.null(sample$error)) loadlog$loadResult[i] <- paste(loadlog$loadResult[i],", sample failed") 
  if(is.null(lot1$error))   loadlog$loadResult[i] <- paste(loadlog$loadResult[i],", lot1 failed") 
  if(is.null(lot2$error))   loadlog$loadResult[i] <- paste(loadlog$loadResult[i],", lot2 failed") 
  
  
   }

} else {   loadlog$loadResult[i] <- "SAMPLE NOT LOADED, "  #faile unique or missing data
           loadError <- T
          if(!isUnique ) { 
           loadlog$loadResult[i] <- paste(loadlog$loadResult[i],
                                      "Animal Number and Vendor are already in LIMS, ") 
            }
           if(!all_required) { 
             loadlog$loadResult[i] <- paste(loadlog$loadResult[i],
                                        "Sample missing required value") 
             }
            
          
        }

}  #End i loop

if(loadError) {  incProgress( i/nrow(sampleDF)) 
                 showModal(modalDialog( title = "Sample Load Error",
                                          "One or more sample was not loaded.  Download the Load Report for details.",
                                          easyClose = FALSE,
                                          footer =   tagList( modalButton("OK"))
                                        )
                           )
                
                   Sys.sleep(3)
                
                   
                }
if(T)(message("Returning log"))


return(loadlog)

}


