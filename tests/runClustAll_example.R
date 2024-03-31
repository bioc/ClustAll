##########################################################
# Running the pipeline with parallelization
##########################################################
require("ClustAll")
data("BreastCancerWisconsin", package = "ClustAll") # load example data
wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
wdbc <- wdbc[1:15,1:8] # reduce the number of variables for a less computational time
obj <- createClustAll(data = wdbc, colValidation = NULL,
                      nImputation = NULL, dataImputed = NULL)


test_pipeline1 <- function() {
  # 1. No parallelization, 2. YES simplified (use all depth)
  runClustAll(obj, threads = 1, simplify = TRUE)
}
#
test_pipeline2 <- function() {
  # 1. YES parallelization, 2. YES simplified (one depth every four)
  runClustAll(obj, threads = 2, simplify = TRUE)
}


