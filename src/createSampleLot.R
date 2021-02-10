createSampleLot <-
  function (coreApi,
            sampleType,
            sampleBarcode,
            body = NULL,
            useVerbose = FALSE)
  {
    #clean the name for ODATA
    
    sampleType <- CoreAPIV2::ODATAcleanName(sampleType)
    
    lotName <- paste0(sampleType, "_LOT")
    
    
    
    lotRef <-
      list('IMPL_LOT_SAMPLE@odata.bind' = paste0("/", sampleType, "('", sampleBarcode, "')"))
    
    fullBody <- jsonlite::toJSON(c(body, lotRef), auto_unbox = TRUE)
    
    headers <-
      c('Content-Type' = "application/json;odata.metadata=full", accept = "application/json")
    
    response <-
      CoreAPIV2::apiPOST(
        coreApi,
        resource = lotName,
        body = fullBody,
        encode = "json",
        headers = headers,
        special = NULL,
        useVerbose = useVerbose
      )
    
    
    
    list(entity = httr::content(response), response = response)
    
  }