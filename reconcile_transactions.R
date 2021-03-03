reconcile_transactions <- function() {
     
     # load required package
     
     library(dplyr)
     
     # source broker and quicken data import functions

     source('~/Github/reconcile-investing/mac_transactions.R')
     source('~/Github/reconcile-investing/quicken_transactions.R')
     source('~/Github/reconcile-investing/resource_proj_path.R')
     source('~/Github/reconcile-investing/resource_year_path.R')
     
     # compare broker df_bposition to quicken df_qpositions joining keytrans
     
     compare_transactions <- full_join(quicken_transactions(),
                                       mac_transactions(),
                                       by = "keytrans")
     
     # create record mismatch logical vector, convert to df
     
     vector_mismatch <- is.na(compare_transactions$q.shares) |
          is.na(compare_transactions$source)
     df_mismatch <- as.data.frame(vector_mismatch)

     # cbind compare_transactions and df_mismatch
     
     compare_transactions <- cbind(compare_transactions, df_mismatch)
     
     # set working directory for writing reconciliation output
     
     setwd(paste(resource_year_path(), "4 reconciliation", sep = ""))
     
     # write reconciliation output
     
     write.csv(compare_transactions, "reconciliation_transactions.csv")
     
     # return working directory back to project path
     
     setwd(resource_proj_path())
     
}