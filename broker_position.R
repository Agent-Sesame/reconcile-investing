broker_position <- function() {
     
     # set working directory
     
     source('~/Github/reconcile-investing/resource_year_path.R')
     
     setwd(paste(resource_year_path(), "2 schwab/accounts positions", sep = ""))
     
     # import quicken portfolio data
     
     df_bposition <- read.csv(list.files(), 
                              header = FALSE, 
                              skip = 4, stringsAsFactors = FALSE)
     
     # drop unneeded columns. v2 kept for subsetting purposes
     
     df_bposition <- df_bposition[, c(1, 2, 3, 10)]
     
     # exclude junk rows, remove v2
     
     df_bposition <- df_bposition[!df_bposition$V2 == "--", ]
     df_bposition <- df_bposition[!df_bposition$V2 == "Description", ]
     df_bposition <- df_bposition[!df_bposition$V2 == "", ]
     df_bposition <- df_bposition[, c(1, 3, 4)]
     
     # rename columns
     
     colnames(df_bposition) <- c("b.sym",
                                 "b.share",
                                 "b.cost")
     
     # remove white spaces, dollar signs, commas, then forward slash
     
     df_bposition <- as.data.frame(apply(df_bposition, 2, function(x)
             gsub("\\s+", "", x)), stringsAsFactors = FALSE)
     df_bposition <- as.data.frame(apply(df_bposition, 2, function(x)
             gsub("\\$", "", x)), stringsAsFactors = FALSE)
     df_bposition <- as.data.frame(apply(df_bposition, 2, function(x)
             gsub("\\,", "", x)), stringsAsFactors = FALSE)
     df_bposition <- as.data.frame(apply(df_bposition, 2, function(x)
             gsub("\\/", "", x)), stringsAsFactors = FALSE)
     
     # create b.shares vector of 0.00000 formatted shares
     
     b.shares <- as.numeric(df_bposition$b.share)
     b.shares <- format(b.shares, nsmall = 5)
     b.shares <- trimws(b.shares)
     
     # create keytrans vector, convert to df
     
     keytrans <- paste(df_bposition$b.sym,
                       b.shares,
                       df_bposition$b.cost)
     df_keytrans <- as.data.frame(keytrans)
     
     # cbind df_bposition and df_keytrans
     
     df_bposition <- cbind(df_bposition, df_keytrans)
     
}