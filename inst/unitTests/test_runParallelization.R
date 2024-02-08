##########################################################
# Running the pipeline with parallelization
##########################################################
require("ClustAll")
data("BreastCancerWisconsin", package = "ClustAll") # load example data
wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))

# This part of the code is commented out as it exceeds the R CMD check time on
# the macOS 12.7.1 Monterey/x86_64 machine.
# test_pipeline <- function() {
#   obj <- createClustAll(data = wdbc, colValidation = NULL,
#                         nImputation = NULL, dataImputed = NULL)
#   runClustAll(obj, threads = 2, simplify = TRUE) # parallelization
# }

