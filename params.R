#'
#' ----------------
#' PARAMETERS
#' ----------------
#' 
#' 
#' 
#' ----------------
#' INPUTS
#' ----------------
#' 
#'     state: matrix of the health state names with indicators for the attributes
#'     
#'     
#'     
#'----------------
#' OUTPUT
#' ---------------
#' 
#'  
# CD4 Levels Names

# TO DO LATER: add functionality to make these utilities probabilistic
uAcute <- 0.690     # se: 0.058; unknown utility, assume same as <350
u500   <- 0.730     # se: 0.052 
u350   <- 0.710     # se: 0.059
u200   <- 0.690     # se: 0.058
uAIDS  <- 0.690     # se: 0.058
# Sources: Review by Whitham 2016 & CDC Report 16 2014 (CDC Medical Monitoring Project)




# UTILITY --------------------------------------------------------------------

# COSTS are in the file "costs.R"

#uState$utility[uState$states] <- 1

# OTHER ----------------------------------------------------------------------

discount <- 0.03



#' 
#' 