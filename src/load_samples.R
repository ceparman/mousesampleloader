


load_samples <- function(sampleDF,creds)
{

log <- sampleDF
log$barcode <- rep("",nrow(sampleDF))
  
  
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
    NA_CEP_DOB = sampleDF$DOB[i],
    NA_CEP_SEX = sampleDF$SEX[i],
    FREQ_AGE = ifelse(sampleDF$`Age (wks)`[i] =="","", paste0(sampleDF$`Age (wks)`[i]," wks")),
    FREQ_DEAFENING_METHOD = sampleDF$`Deafening method`[i],
    FREQ_IT_INJECTION_TIME_AGE = sampleDF$`IT inj time Age (wks)`[i],
    FREQ_SAC_TIME_AGE = sampleDF$`sac time Age (wks)`[i],
    FREQ_FIXATION = sampleDF$FIXATION[i],
    FREQ_DECALCIFICATION = sampleDF$DECAL[i],
    FREQ_FORMULATION = sampleDF$`Formulation(FT or Vehicle)`[i],
    NA_CEP_ANIMAL_NUM= sampleDF$`Animal #`[i],
    FREQ_VENDOR = sampleDF$Vendor[i]
  )
  

  
  print(attributes)
sample <- CoreAPIV2::createEntity(coreApi = creds,entityType = "MOUSE",body = attributes,useVerbose = T)

#record barcode

log$barcode[i] <- sample$entity$Barcode


#Create lot 1

lot_att <- list(FREQ_NEXT_STEP = "Dissection")


lot1 <- createSampleLot(coreApi = creds,sampleType = "MOUSE",body = lot_att, sampleBarcode = sample$entity$Barcode ,useVerbose = T)



#Create lot 2

lot_att <- list(FREQ_NEXT_STEP = "On Hold")


lot2 <- createSampleLot(coreApi = creds,sampleType = "MOUSE",body = lot_att, sampleBarcode = sample$entity$Barcode )


incProgress( i/nrow(sampleDF))

Sys.sleep(1)

}

log

}


