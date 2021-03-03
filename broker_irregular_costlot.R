broker_irregular_cl <- function() {
     
     # load required packages
     
     library(dplyr)
     library(tidyr)
     
     # set working directory
     
     source('~/Github/reconcile-investing/resource_year_path.R')
     
     setwd(paste(resource_year_path(), 
                 "2 schwab/irregular positions", sep = ""))
     
     # create vector of saved files
     
     vector_files <- list.files()
     
     # rbind an lapply of read.csv on vector_files, a list of downloaded files
     
     df_irregularcl <- do.call("rbind", lapply
                            (vector_files, 
                                 read.csv, 
                                 header = FALSE))
     
     # drop unneeded columns
     
     df_irregularcl <- df_irregularcl[, c(1:2, 6)]
     
     # create df of logical tests to find "" in column 2
     
     vector_logical <- df_irregularcl$V2 == ""
     df_logical <- as.data.frame(vector_logical)
     
     # cbind df_irregularcl and df_logical for further mutation
     
     df_irregularcl <- cbind(df_irregularcl, df_logical)
     
     # mutate df_irregularcl to create a symbol column
     
     df_irregularcl <- mutate(df_irregularcl, 
                              b.symbol = 
                                   ifelse(df_irregularcl$vector_logical == TRUE,
                                          df_irregularcl$V1, ""))
     
     # fix b.symbol column to copy down symbol text into blank fields
     
     df_irregularcl <- (df_irregularcl
                     %>% mutate(grp = cumsum( "" != b.symbol ) )
                     %>% group_by(grp)
                     %>% mutate(b.symbol = b.symbol[1])
                     %>% ungroup()
                     %>% select(-grp))
     
     # subset garbage lines out of df_irregularcl
     
     df_irregularcl <- df_irregularcl[!df_irregularcl$V1 == "Total", ]
     df_irregularcl <- df_irregularcl[!df_irregularcl$V1 == "Open Date", ]
     df_irregularcl <- df_irregularcl[!df_irregularcl$V2 == "", ]
     
     # split b.symbol by space to isolate symbol for row
     
     df_irregularcl <- separate(df_irregularcl, 
                                b.symbol, 
                                into = c("b.symbols", "dropstring"), 
                                sep = " ")

     # split v1 into character string containing date, then droptime
     
     df_irregularcl <- separate(df_irregularcl, 
                                V1, 
                                into = c("b.date", "droptime"), 
                                sep = " ")

     # convert character date string into data class column
     
     b.dates <- as.Date(df_irregularcl$b.date, "%m/%d/%Y")
     df_dates <- as.data.frame(b.dates)
     
     # cbind correct date class column 
     
     df_irregularcl <- cbind(df_irregularcl, df_dates)
     
     # remove white spaces, dollar signs, commas, forward slashes
     
     df_irregularcl <- as.data.frame(apply(df_irregularcl, 2, function(x)
          gsub("\\s+", "", x)), stringsAsFactors = FALSE)
     df_irregularcl <- as.data.frame(apply(df_irregularcl, 2, function(x)
          gsub("\\$", "", x)), stringsAsFactors = FALSE)
     df_irregularcl <- as.data.frame(apply(df_irregularcl, 2, function(x)
          gsub("\\,", "", x)), stringsAsFactors = FALSE)
     # df_irregularcl <- as.data.frame(apply(df_irregularcl, 2, function(x)
     #        gsub("\\/", "", x)), stringsAsFactors = FALSE)
     
     
          
     # create b_shares vector of 0.00000 formatted shares, convert to df
     
     b.shares <- as.numeric(df_irregularcl$V2)
     b.shares <- format(b.shares, nsmall = 5)
     b.shares <- trimws(b.shares)
     df_shares <- as.data.frame(b.shares)
     
     # cbind 0.00000 formatted shares df to df_irregularcl
     
     df_irregularcl <- cbind(df_irregularcl, df_shares)
     
     # drop columns no longer needed
     
     df_irregularcl <- df_irregularcl[, c(8, 6, 9, 4)]
     
     # rename columns
     
     colnames(df_irregularcl) <- c("b.dates", 
                                   "b.symbols", 
                                   "b.shares", 
                                   "b.cost")
     
     # create keytrans vector, convert to df
     
     keytrans <- paste(df_irregularcl$b.dates,
                       df_irregularcl$b.symbols,
                       df_irregularcl$b.shares,
                       df_irregularcl$b.cost)
     df_keytrans <- as.data.frame(keytrans)
     
     # cbind df_bcostlot and df_keytrans
     
     df_irregularcl <- cbind(df_irregularcl, df_keytrans)
     
}