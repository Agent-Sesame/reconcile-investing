broker_costlot <- function() {
     
     # load required packages
     
     library(dplyr)
        
     # set working directory
     
     source('~/Github/reconcile-investing/resource_year_path.R')
     
     setwd(paste(resource_year_path(), "2 schwab/trading tools", sep = ""))
     
     # create vector of saved files
     
     vector_files <- list.files()
     
     # rbind an lapply of read.csv on vector_files, a list of downloaded files
     
     df_bcostlot <- do.call("rbind", lapply
                            (vector_files, 
                                    read.csv, 
                                    header = FALSE))
     
     # drop unneeded columns
     
     df_bcostlot <- df_bcostlot[, c(1:3, 5)]
     
     # fix symbol column to copy down symbol text into blank fields
     
     df_bcostlot <- (df_bcostlot
                    %>% mutate(grp = cumsum( "" != V1 ) )
                    %>% group_by(grp)
                    %>% mutate(V1 = V1[1])
                    %>% ungroup()
                    %>% select(-grp))
     
     # exclude junk rows
     
     df_bcostlot <- df_bcostlot[!df_bcostlot$V3 == "Security Type", ]
     df_bcostlot <- df_bcostlot[!df_bcostlot$V3 == "Eqty", ]
     df_bcostlot <- df_bcostlot[!df_bcostlot$V3 == "Quantity", ]
     df_bcostlot <- df_bcostlot[!df_bcostlot$V1 == "SWVXX", ]
     df_bcostlot <- df_bcostlot[!df_bcostlot$V1 == "Totals:", ]
     df_bcostlot <- df_bcostlot[!df_bcostlot$V1 == "SHRAX", ]

     # rename columns
     
     colnames(df_bcostlot) <- c("b.sym", "b.date", "b.share", "b.cost")

     # remove white spaces, dollar signs, commas, forward slashes
     
     df_bcostlot <- as.data.frame(apply(df_bcostlot, 2, function(x)
             gsub("\\s+", "", x)), stringsAsFactors = FALSE)
     df_bcostlot <- as.data.frame(apply(df_bcostlot, 2, function(x)
             gsub("\\$", "", x)), stringsAsFactors = FALSE)
     df_bcostlot <- as.data.frame(apply(df_bcostlot, 2, function(x)
             gsub("\\,", "", x)), stringsAsFactors = FALSE)

     # create df of symbols to remove forward slashes
     
     df_symbols <- as.data.frame(df_bcostlot$b.sym)
     df_symbols <- as.data.frame(apply(df_symbols, 2, function(x) 
             gsub("\\/", "", x)), stringsAsFactors = FALSE)
     colnames(df_symbols) <- "b.symbols"
     
     # create b_shares vector of 0.00000 formatted shares, convert to df
     
     b.shares <- as.numeric(df_bcostlot$b.share)
     b.shares <- format(b.shares, nsmall = 5)
     b.shares <- trimws(b.shares)
     df_shares <- as.data.frame(b.shares)

     # create b.dates of clean dates, convert to df
     
     b.dates <- as.Date(df_bcostlot$b.date, "%m/%d/%Y")
     df_dates <- as.data.frame(b.dates)
     
     # cbind base df with cleaned up shares and dates dfs
     
     df_bcostlot <- cbind(df_bcostlot, df_dates, df_symbols, df_shares)

     # drop junk data columns 
     
     df_bcostlot <- df_bcostlot[, c(5, 6, 7, 4)]

     # create keytrans vector, convert to df
     
     keytrans <- paste(df_bcostlot$b.dates,
                       df_bcostlot$b.symbols,
                       df_bcostlot$b.shares,
                       df_bcostlot$b.cost)
     df_keytrans <- as.data.frame(keytrans)
     
     # cbind df_bcostlot and df_keytrans
     
     df_bcostlot <- cbind(df_bcostlot, df_keytrans)
     
}