quicken_portfolio <- function() {
     
     # set working directory
     
     source('~/Github/reconcile-investing/resource_year_path.R')
     
     setwd(paste(resource_year_path(), "1 quicken", sep = ""))
     
     # import quicken portfolio data

     df_qportfolio <- read.csv(list.files(pattern = "*Portfolio*"),
                               header = FALSE,
                               skip = 6,
                               stringsAsFactors = FALSE)

     # drop unneeded columns. note keeping V3 (qpf.lotvpos) temporarily to use
     # later to make a position data frame that excludes lot data
         
     df_qportfolio <- df_qportfolio[, c(1, 2, 3, 6, 7, 11)]
     
     # rename columns

     colnames(df_qportfolio) <- c("q.date",
                                  "q.sym",
                                  "q.lotvpos",
                                  "q.share",
                                  "q.cost",
                                  "q.account")

     # remove white spaces, dollar signs, then commas
     
     df_qportfolio <- as.data.frame(apply(df_qportfolio, 2, function(x)
          gsub("\\s+", "", x)), stringsAsFactors = FALSE)
     df_qportfolio <- as.data.frame(apply(df_qportfolio, 2, function(x)
          gsub("\\$", "", x)), stringsAsFactors = FALSE)
     df_qportfolio <- as.data.frame(apply(df_qportfolio, 2, function(x) 
          gsub("\\,", "", x)), stringsAsFactors = FALSE)

     # create q.shares vector of 0.00000 formatted shares, convert to df
     
     q.shares <- as.numeric(df_qportfolio$q.share)
     q.shares <- format(q.shares, nsmall = 5)
     q.shares <- trimws(q.shares)
     df_shares <- as.data.frame(q.shares)
     
     # create vector called q.dates. first column q.date has a zero width 
     # invisible character at the head of values that interferes with the 
     # as.Date function. this next chunk of code creates a clean date vector.
     # convert q.dates to df.
     
     q.dates <- as.Date(gsub("^\\s+|\\s+$", "", substr(df_qportfolio$q.date, 2, 
                                    nchar(df_qportfolio$q.date))), "%m/%d/%Y")
     df_dates <- as.data.frame(q.dates)

     # create a position level postrans field and convert to df
     
     postrans <- paste(df_qportfolio$q.sym, 
                       q.shares, 
                       df_qportfolio$q.cost)
     df_postrans <- as.data.frame(postrans)
     
     # create a lot level lottrans field and convert to df
     
     lottrans <- paste(q.dates, 
                       df_qportfolio$q.sym, 
                       q.shares, 
                       df_qportfolio$q.cost)
     df_lottrans <- as.data.frame(lottrans)
     
     # cbind df_qportfolio and df_shares with properly formatted shares
     
     df_qportfolio <- cbind(df_qportfolio, 
                            df_dates,
                            df_shares,
                            df_postrans,
                            df_lottrans)
     
     # create two copies of df_qportfolio for further modification into 
     # df_qpositions and df_qcostlots data frames, 
     
     df_qcostlots <- df_qportfolio[, c(7, 2, 8, 5, 6, 10)]
     
     df_qpositions <- df_qportfolio[, c(7, 2, 8, 5, 6, 9)]
     
     # subset exclude df_qpositions NA dates, rename temporary postrans to 
     # keytrans, exclude NA shares, drop first column
     
     df_qpositions <- df_qpositions[is.na(df_qpositions$q.dates), ]
     colnames(df_qpositions)[6] <- "keytrans"
     df_qpositions <- df_qpositions[!df_qpositions$q.shares == "NA", ]
     df_qpositions <- df_qpositions[, c(2:6)]
     
     # rename temporary lottrans to keytrans, subset df_qcostlots to remove 
     # NA date values
     
     colnames(df_qcostlots)[6] <- "keytrans"
     df_qcostlots <- df_qcostlots[!is.na(df_qcostlots$q.dates), ]
     
     return(list(df_qpositions, df_qcostlots))

}