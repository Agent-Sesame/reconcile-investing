mac_transactions <- function() {
     
     # source required functions
     
     source('~/Github/reconcile-investing/resource_year_path.R')

     # set working directory for retail account data import
     
     setwd(paste(resource_year_path(), "3 transactions/B-3848", sep = ""))
     
     # list directories in cost basis transaction directory  
     
     df_3848 <- as.data.frame(list.files(recursive = FALSE))
     
     # set working directory for retail account data import
     
     setwd(paste(resource_year_path(), "3 transactions/R-1293", sep = ""))
     
     # list directories in cost basis transaction directory  
     
     df_1293 <- as.data.frame(list.files(recursive = FALSE))
     
     # cbind df_1293 and df_3848
     
     keytrans <- rbind(df_1293, df_3848)
     
     # create placeholder vector the lenth of keytrans
     
     columnv2 <- rep("mac data", dim(keytrans)[1])
     df_columnv2 <- as.data.frame(columnv2)
     
     # cbind columnv2 and keytrans into a single data frame
     
     keytrans <- cbind(keytrans, df_columnv2)
     
     # fix column names
     
     colnames(keytrans) <- c("keytrans", "source")
     
     # return data frame keytrans
     
     return(keytrans)
     
}
