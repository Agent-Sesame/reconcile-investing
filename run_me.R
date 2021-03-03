run_me <- function() {
     
     # load required functions
     
     source('~/Github/reconcile-investing/reconcile_costlots.R');
     source('~/Github/reconcile-investing/reconcile_positions.R');
     source('~/Github/reconcile-investing/reconcile_transactions.R');
     source('~/Github/reconcile-investing/reconcile_directories.R');
     
     # run reconciliation functions
     
     reconcile_costlots();
     reconcile_positions();
     reconcile_transactions();
     reconcile_directories()

}