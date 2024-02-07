##########################################################
# Running the pipeline with parallelization
##########################################################
require("ClustAll")
data("BreastCancerWisconsin", package = "ClustAll") # load example data

test_pipeline <- function() {
  obj <- createClustAll(data = wdbc, colValidation = "Diagnosis",
                        nImputation = NULL, dataImputed = NULL)
  runClustAll(obj, threads = 2, simplify = TRUE) # parallelization
}
