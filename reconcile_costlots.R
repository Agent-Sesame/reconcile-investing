reconcile_costlots <- function() {
     
     # load required package
     
     library(dplyr)
     
     # source broker and quicken data import functions
     
     source('~/Github/reconcile-investing/broker_costlot.R')
     source('~/Github/reconcile-investing/broker_irregular_costlot.R')
     source('~/Github/reconcile-investing/quicken_portfolio.R')
     source('~/Github/reconcile-investing/resource_proj_path.R')
     source('~/Github/reconcile-investing/resource_year_path.R')
     
     # rbind broker_costlot and broker_irregular_costlot
     
     broker_lots <- rbind(broker_costlot(), broker_irregular_cl())
     
     # compare broker df_bposition to quicken df_qpositions joining keytrans
     
     compare_lots <- full_join(broker_lots,
                                    quicken_portfolio()[[2]],
                                    by = "keytrans")
     
     # create record mismatch logical vector, convert to df
     
     vector_mismatch <- is.na(compare_lots$b.sym) |
          is.na(compare_lots$q.sym)
     
     df_mismatch <- as.data.frame(vector_mismatch)
     
     # cbind compare_lots and df_mismatch
     
     compare_lots <- cbind(compare_lots, df_mismatch)
     
     # set working directory for writing reconciliation output
     
     setwd(paste(resource_year_path(), "4 reconciliation", sep = ""))
     
     # write reconciliation output
     
     write.csv(compare_lots, "reconciliation_costlots.csv")
     
     # return working directory back to project path
     
     setwd(resource_proj_path())
     
}