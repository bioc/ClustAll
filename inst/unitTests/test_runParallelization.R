##########################################################
# Running the pipeline with parallelization
##########################################################
require("ClustAll")
data("BreastCancerWisconsin", package = "ClustAll") # load example data
wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
wdbc <- wdbc[1:15,1:8]

test_pipeline <- function() {
  obj <- createClustAll(data = wdbc, colValidation = NULL,
                        nImputation = NULL, dataImputed = NULL)
  runClustAll(obj, threads = 2, simplify = TRUE) # parallelization
}
