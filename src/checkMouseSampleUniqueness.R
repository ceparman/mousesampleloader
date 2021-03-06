
# 
# account <- CoreAPIV2::coreAPI("Credentialsfreq.txt")
# 
# 
# account$user <- "Contractor"
# account$pwd<- "Data123"
# 
# 
# login <- CoreAPIV2::authBasic(account)
# 
# creds <- login$coreApi
# 
# vendor <- "TEST"
# 
# animalid <- "1001"
# 
# checkMouseSampleUniqueness(creds, animalid, vendor)

checkMouseSampleUniqueness <- function(creds, animalid, vendor)
{
  
  
#Returns true if animmal id and vendor combination is not in the database.  Otherwise false.
  
  
#query for and samples  

#NA1prod
 
  if(creds$coreUrl == "na1.platformforscience.com") {   
     query = paste0( "?$filter=FREQ_VENDOR eq '",vendor,"' and FREQ_ANIMAL_NUMBER eq '",animalid,"' and Active eq true&$count=true")
  } else 
    if(creds$account == "Frequency Prod Copy 4-14-2021"){
      query = paste0( "?$filter=FREQ_VENDOR eq '",vendor,"' and FREQ_ANIMAL_NUMBER eq '",animalid,"' and Active eq true&$count=true")
    } else {

#NA1test     
    query = paste0( "?$filter=FREQ_VENDOR eq '",vendor,"' and NA_CEP_ANIMAL_NUM eq '",animalid,"' and Active eq true&$count=true")
   }



query<- URLencode(query)


headers <- c('Content-Type' = "application/json;odata.metadata=minimal", accept = "application/json") 



response <-
  CoreAPIV2::apiGET(
    creds,
    query=query,
    resource = "MOUSE",
    headers = headers,
    special = NULL,
    useVerbose = T
  )


count <- httr::content(response$response)$`@odata.count`

if (count == 0) { return(T)} else {return(F)}

}


