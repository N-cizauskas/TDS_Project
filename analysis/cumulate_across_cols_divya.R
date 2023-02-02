# NEW FUNCTION
cumulate_df <- function(Y,X) {
  df <- Y
  output <- as.data.frame(df %>%
                            rowwise %>% 
                            mutate(m = {tmp <- c_across(starts_with(X))
                            tail(na.omit(tmp), 1)[1]}) %>%
                            ungroup)
  names(output)[names(output) == "m"] <- paste0(X,"_cumulative")
  return(output)
}

# STEP 1: IMPORT DATA

# STEP 2: RUN CODE VIA cumulate_df(nameofdataframe, "startnameofcol")
# example: merged_df <- cumulate_df(merged_df, "bmi")


## IGNORE BELOW ##############################################################################################################################
# FUNCTION TO COLLATE ACROSS INSTANCES
foo <- function(Y, X){
  df <- Y
  rows <- nrow(df %>% select(starts_with(X)))
  i <- ncol(df %>% select(starts_with(X))) # get col num
  df[,paste0(X,"_cumulative")] <- df[,i] # final one
  pb <- txtProgressBar(0, rows, style = 3)
  
  for (row in 1:rows){
    setTxtProgressBar(pb, row)
    i <- ncol(df %>% select(starts_with(X))) # get col num
    #print(paste0('current row is ', as.character(row))) # while in row 1
    #print(paste0('col value is ', as.character(df[row, paste0(X,"_cumulative")])))
    while (is.na(df[row, paste0(X,"_cumulative")]) & i>1){
      i <- i-1
      df[row, paste0(X,"_cumulative")] <- df[row, i]
    }
    Sys.sleep(time = 1)
  }
  close(pb)
  return(df)
  
}


