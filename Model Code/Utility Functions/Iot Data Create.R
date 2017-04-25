Fake_IoT_Data <- function(start, end, mean, sd){
  
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  
  set.seed(8675309)
  TEMP <- rnorm(length(seq.POSIXt(start, end, by = "min")), mean = 50, sd = .5)
  
  TIME <- as.character(seq.POSIXt(from = start, to = end,length.out = length(TEMP)))

  output <- tbl_df(cbind(TIME= TIME, TEMP = TEMP)  ) %>% 
            mutate(TEMP = as.numeric(TEMP),
                   TIME = as.POSIXct(TIME)
                   )
  
  return(output)
}
