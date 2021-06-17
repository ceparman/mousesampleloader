


load_samples <- function(sampleDF,creds)
{

loadlog <- sampleDF

loadlog$barcode <- rep("",nrow(sampleDF))
  
loadlog$loadResult <- rep("",nrow(sampleDF))


print(loadlog)

loadError <- F 
  
for(i in 1:nrow(sampleDF)){

  
  
  
#Convert NA to "" in SEX
  
  sampleDF[i,3] <- ifelse(is.na(sampleDF[i,3]),"",sampleDF[i,3]) 
  

#Fix DOB to proper format if it is not blank.   
    

  char_date <- ifelse(  ( (is.na(sampleDF$DOB[i]) )   | (sampleDF$DOB[i] == "") )  ,"",
                            as.character(paste0(as.character(sampleDF$DOB[i]),"T00:00:00Z") ))

#print(char_date)  
    
 #print(is.na(sampleDF$DOB[i]))
  

  
#NA1
  
if(creds$coreUrl == "na1.platformforscience.com") { 
  
  attributes <- list(
    STRAIN = sampleDF$`MOUSE STRAIN`[i],
    FREQ_STRAIN_GENOTYPE = sampleDF$`MOUSE STRAIN GENOTYPE`[i],
    FREQ_DOB = char_date,
    SEX = sampleDF$SEX[i],
    FREQ_AGE = sampleDF$`Age (wks)`[i],
    FREQ_DEAFENING_METHOD = sampleDF$`Deafening method`[i],
    FREQ_IT_INJECTION_TIME_AGE = sampleDF$`IT inj time Age (wks)`[i],
    FREQ_SAC_TIME_AGE = sampleDF$`sac time Age (wks)`[i],
    FREQ_FIXATION = sampleDF$FIXATION[i],
    FREQ_DELCALCIFICATION= sampleDF$DECAL[i],
    FREQ_FORMULATION = sampleDF$`Formulation(FT, Vehicle, or None)`[i],
    FREQ_ANIMAL_NUMBER= sampleDF$`Animal #`[i],
    FREQ_VENDOR = sampleDF$Vendor[i],
    FREQ_LINEAGE_TRACING_METHOD = sampleDF$`Lineage Tracing Method`[i]
  )
  #Frequency Prod Copy 
} else 
      if(creds$account == "Frequency Prod Copy 4-14-2021"){
      
      attributes <- list(
        STRAIN = sampleDF$`MOUSE STRAIN`[i],
        FREQ_STRAIN_GENOTYPE = sampleDF$`MOUSE STRAIN GENOTYPE`[i],
        FREQ_DOB = char_date,
        SEX = sampleDF$SEX[i],
        FREQ_AGE = sampleDF$`Age (wks)`[i],
        FREQ_DEAFENING_METHOD = sampleDF$`Deafening method`[i],
        FREQ_IT_INJECTION_TIME_AGE = sampleDF$`IT inj time Age (wks)`[i],
        FREQ_SAC_TIME_AGE = sampleDF$`sac time Age (wks)`[i],
        FREQ_FIXATION = sampleDF$FIXATION[i],
        FREQ_DELCALCIFICATION= sampleDF$DECAL[i],
        FREQ_FORMULATION = sampleDF$`Formulation(FT, Vehicle, or None)`[i],
        FREQ_ANIMAL_NUMBER= sampleDF$`Animal #`[i],
        FREQ_VENDOR = sampleDF$Vendor[i],
        FREQ_LINEAGE_TRACING_METHOD = sampleDF$`Lineage Tracing Method`[i]
      ) 
      
      
    } else{
# #NA1TEST
attributes <- list(
  NA_CEP_STRAIN = sampleDF$`MOUSE STRAIN`[i],
  FREQ_STRAIN_GENOTYPE = sampleDF$`MOUSE STRAIN GENOTYPE`[i],
  NA_CEP_DOB = char_date,
  NA_CEP_SEX = sampleDF$SEX[i],
  FREQ_AGE = sampleDF$`Age (wks)`[i],
  FREQ_DEAFENING_METHOD = sampleDF$`Deafening method`[i],
  FREQ_IT_INJECTION_TIME_AGE = sampleDF$`IT inj time Age (wks)`[i],
  FREQ_SAC_TIME_AGE = sampleDF$`sac time Age (wks)`[i],
  FREQ_FIXATION = sampleDF$FIXATION[i],
  FREQ_DECALCIFICATION = sampleDF$DECAL[i],
  FREQ_FORMULATION = sampleDF$`Formulation(FT, Vehicle, or None)`[i],
  NA_CEP_ANIMAL_NUM= sampleDF$`Animal #`[i],
  FREQ_VENDOR = sampleDF$Vendor[i],
  FREQ_LINEAGE_TRACING_METHOD = sampleDF$`Lineage Tracing Method`[i]
)
  
}  
  
 
 #save attributes for debugging
 
# saveRDS(attributes,"attributes.RDS")
 
 
#Validate that all required fields are not empty  
  
required_columns <- which(read_excel("www/MouseSampleTemplate.xlsx", col_names = F,n_max=1) == "Required")   
  
all_required <- all(!(sampleDF[i,required_columns] %in%  c("",NA)))  
  

#Validate Uniqueness

#NA1PROD

isUnique <- checkMouseSampleUniqueness(creds, sampleDF$`Animal #`[i], sampleDF$Vendor[i])  



print("isUnique" )
print(isUnique ) 

print("all_required" )
print(all_required ) 

if(isUnique & all_required ) { 

 safecreateEntity <- purrr::safely(.f=CoreAPIV2::createEntity)
 safecreateSampleLot <- purrr::safely(.f=local_createSampleLot)

         #Create sample 
   
         sample <- safecreateEntity(coreApi = creds,entityType = "MOUSE",body = attributes,useVerbose = F)

        
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
  
   incProgress( 1/nrow(sampleDF))
  
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

if(loadError) {  incProgress( 1/nrow(sampleDF)) 
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


