##########################################################
# Running the pipeline with and without parallelization
##########################################################
require("ClustAll")
data("BreastCancerWisconsin", package = "ClustAll") # load example data
data("BreastCancerWisconsinMISSING", package = "ClustAll") # load example data

test_pipeline <- function() {
  obj <- createClustAll(wdbcNA, nImputation = 3,  colValidation = "Diagnosis")
  runClustAll(obj, threads = 2, simplify = TRUE) # parallelization
  runClustAll(obj, threads = 1, simplify = FALSE)
}
