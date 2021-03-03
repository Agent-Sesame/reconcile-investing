quicken_transactions <- function() {

     # source required functions
     
     source('~/Github/reconcile-investing/resource_year_path.R')
     
     # set function working directory
     
     setwd(paste(resource_year_path(), "1 quicken", sep = ""))
     
     # import quicken transaction file
     
     df_qtransaction <- read.csv(list.files(pattern = "*export*"),
                                 header = FALSE,
                                 skip = 8,
                                 stringsAsFactors = FALSE)
     
     # subset out "Payment/Deposit" and "Interest Income" values from v5 type
     
     df_qtransaction <- df_qtransaction[
             !df_qtransaction$V5 == "Payment/Deposit", ]
     df_qtransaction <- df_qtransaction[
             !df_qtransaction$V5 == "Interest Income", ]
     
     # drop unneeded columns
     
     df_qtransaction <- df_qtransaction[, c(4, 7, 11, 13, 15)]

     # rename columns
     
     colnames(df_qtransaction) <- c("q.date",
                                    "q.symbol",
                                    "q.shares",
                                    "q.costbasis",
                                    "q.account")
     
     # convert character class date to date class date, vector to df
     
     q.dates <- as.Date(df_qtransaction$q.date, "%m/%d/%Y")
     df_dates <- as.data.frame(q.dates)
     
     # create q.shares vector of 0.00000 formatted shares, convert to df
     
     q.shares <- gsub(",", "", df_qtransaction$q.shares) # remove commas
     q.shares <- as.numeric(q.shares)
     q.shares <- format(q.shares, nsmall = 5)
     q.shares <- trimws(q.shares)
     df_shares <- as.data.frame(q.shares)
     
     # create cleaned up df of reformatted data
     
     df_qtransaction <- cbind(df_dates,
                              df_qtransaction$q.symbol,
                              df_shares,
                              df_qtransaction$q.costbasis,
                              df_qtransaction$q.account)
     
     # remove white spaces, dollar signs, commas, forward slashes
          
     df_qtransaction <- as.data.frame(apply(df_qtransaction, 2, function(x)
          gsub("\\s+", "", x)), stringsAsFactors = FALSE)
     df_qtransaction <- as.data.frame(apply(df_qtransaction, 2, function(x)
          gsub("\\$", "", x)), stringsAsFactors = FALSE)
     df_qtransaction <- as.data.frame(apply(df_qtransaction, 2, function(x)
          gsub("\\,", "", x)), stringsAsFactors = FALSE)
     
     # fix column names
     
     colnames(df_qtransaction) <- c("q.dates",
                                    "q.symbol",
                                    "q.shares",
                                    "q.costbasis",
                                    "q.account")
     
     # create keytrans

     keytrans <- trimws(paste(df_qtransaction$q.date,
                       df_qtransaction$q.symbol,
                       df_qtransaction$q.shares,
                       df_qtransaction$q.costbasis), which = "right")
     df_keytrans <- as.data.frame(keytrans)
     
     # cbind df_qtransaction and df_keytrans
     
     df_qtransaction <- cbind(df_qtransaction, df_keytrans)
     
     # subset out "NA  NA" values from keytrans
     
     df_qtransaction <- df_qtransaction[!df_qtransaction$keytrans == "NA  NA", ]
     
}