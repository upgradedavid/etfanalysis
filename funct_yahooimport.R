yahoo_history <- function(toimport = c("C6E.PA", "ETSZ.DE"),
                          test_nr = 10,
                          startdate = "2021-11-26",
                          enddate = Sys.Date()
) {
  
  startdate <- as.Date(startdate)
  enddate <- as.Date(Sys.Date())

  # First try to get all of the yahoo data
  
  allstock <- tq_get(toimport,
                     complete_cases = T,
                     from = startdate,
                     to = enddate
  )
  
  # Sometimes grab doesn't work > try up to 10 times to fetch it
  
  counter <- 0
  
  # Loop as long as not every ticker is loaded
  while(length(setdiff(toimport,
                       allstock$symbol
               )
        ) != 0 & counter <= (test_nr - 1)) {
    
    allstock <- rbind(allstock,
                      tq_get(setdiff(toimport,
                                     allstock$symbol
                             ),
                             from = as.character(startdate),
                             to = NULL
                      )
    )
  
  # Print the ones that were not found after the available rounds
  
    if(length(setdiff(toimport, allstock$symbol)) != 0){
    
      cat(paste("\n\nFetchround: ",
                  counter + 1,
                  "\n",
                  "Didn't get those: ",
                  paste(setdiff(toimport, allstock$symbol), collapse = ", "),
                  sep = ""
            )
      )
      
      counter <- counter + 1
    
    } else {
      cat("All stock data acquired!")
    }
  
  }
  
  return(allstock)
}
