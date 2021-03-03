reconcile_directories <- function() {

     # load required packages
     
     library(dplyr)
     library(tidyr)
     
     # set working directory
     
     source('~/Github/reconcile-investing/resource_year_path.R')
     
     setwd(paste(resource_year_path(), "3 transactions", sep = ""))
     
     # create vector of transaction directory, list all files, all files and 
     # folders under target directory and include all directories
     
     vector_dandf <- list.files(all.files = TRUE, 
                                recursive = TRUE, 
                                include.dirs = TRUE)
     
     # create logical 
     pdf <- grepl(".pdf",vector_dandf)
     
     # create data frame of directory print plus the logical comparison
     
     data <- data.frame(vector_dandf, pdf)
     
     # split directory folder / file names by / separator
     
     data <- data %>% separate(vector_dandf,
                            c("account", "folder","file"), 
                            sep = "/")
     
     # create separate condo df for separate evaluation
     
     df_condo <- data[data$account == "condo", ]
     
     # subset out unwanted values
     
     df_condo <- df_condo[!df_condo$folder == "_reconciliations", ]
     df_condo <- df_condo[!df_condo$folder == ".DS_Store", ]
     df_condo <- df_condo[!df_condo$folder == ".ts", ]

     # subset data to exclude rows from reconciliations
     
     data <- data[!data$account == "reconciliations", ]
     data <- data[!data$account == "condo", ]

     # subset out pdf FALSE rows
     
     data <- data[data$pdf == TRUE, ]
     
     # compare folder name to file name
     
     dir_in_name <- paste(data[, 2], ".pdf", sep = "") == data[, 3]
     
     # bind dir_in_name to data
     
     data <- cbind(data, as.data.frame(dir_in_name))
     
     # subset out dir_in_name rows 
     
     data <- data[data$dir_in_name == FALSE, ]
     
     # split up folder data elements on space separator
     
     data <- data %>% separate(folder,
                               c("folder date", 
                                 "folder symbol",
                                 "folder shares",
                                 "folder cost"), 
                               sep = " ")
     
     # split up file data elements on space separator
     
     data <- data %>% separate(file,
                               c("file date", 
                                 "file symbol",
                                 "file shares",
                                 "file cost"), 
                               sep = " ")

     # compare folder date versus file date 
     
     compare_date <- data[, 2] == data[, 6]
     
     # compare folder date versus file symbol
     
     compare_sym <- data[, 3] == data[, 7]

     # compare folder date versus file quantity
     
     compare_quantity <- data[, 4] == data[, 8]
     
     # compare folder date versus file costbasis
     
     compare_cost <- paste(data[, 5], ".pdf", sep = "") == data[, 9]

     # create data frame of logicals
     
     df_logical <- data.frame(compare_date, 
                              compare_sym, 
                              compare_quantity, 
                              compare_cost)
     
     # cbind original data df with df_logical
     
     data <- cbind(data, df_logical)
     
     # subset out rows that have a date of 2020-05-26 with equal symbols, 
     # quantity, and cost
     
     data <- data[!(data$`folder date` == "2020-05-26" & 
                         data$compare_sym == TRUE & 
                         data$compare_quantity == TRUE & 
                         data$compare_cost == TRUE), ]
     
     setwd("~/Documents/finances/2021-R02/COSTBASIS/4 reconciliation")
     write.csv(data, "reconciliation_directories.csv")
     write.csv(df_condo, "reconciliation_condo_directories.csv")
     
}