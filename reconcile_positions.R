reconcile_positions <- function() {
     
     # load required package
     
     library(dplyr)
     
     # source broker and quicken data import functions
     
     source('~/Github/reconcile-investing/broker_position.R')
     source('~/Github/reconcile-investing/quicken_portfolio.R')
     source('~/Github/reconcile-investing/resource_proj_path.R')
     source('~/Github/reconcile-investing/resource_year_path.R')

     # run broker and quicken data import functions
     
     broker_position()
     quicken_portfolio()

     # compare broker df_bposition to quicken df_qpositions joining keytrans
     
     compare_positions <- full_join(broker_position(),
                                    quicken_portfolio()[[1]],
                                    by = "keytrans")
     
     # create record mismatch logical vector, convert to df
     
     vector_mismatch <- is.na(compare_positions$b.sym) |
                         is.na(compare_positions$q.sym)
     
     df_mismatch <- as.data.frame(vector_mismatch)
     
     # cbind compare_positions and df_mismatch
     
     compare_positions <- cbind(compare_positions, df_mismatch)
     
     # set working directory for writing reconciliation output

     setwd(paste(resource_year_path(), "4 reconciliation", sep = ""))
     
     # write reconciliation output
     
     write.csv(compare_positions, "reconciliation_positions.csv")
     
     # return working directory back to project path
     
     setwd(resource_proj_path())
     
}