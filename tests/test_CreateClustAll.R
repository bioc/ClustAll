###########################################
# EXAMPLE OF CREATING ClustAll
###########################################

data("BreastCancerWisconsin", package = "ClustAll") # load example data
data("BreastCancerWisconsinMISSING", package = "ClustAll") # load example data

test_creatingClustAll <- function() {
  createClustAll(data = wdbc, colValidation = "Diagnosis",
                 nImputation = NULL, dataImputed = NULL)
  createClustAll(wdbcNA, nImputation = 5,  colValidation = "Diagnosis")
  createClustAll(wdbcNA, dataImputed = wdbcMIDS,
                 colValidation = "Diagnosis")
}

test_pipeline <- function() {
  obj <- createClustAll(wdbcNA, nImputation = 3,  colValidation = "Diagnosis")
  runClustAll(obj, threads = 2, simplify = TRUE)
  runClustAll(obj, threads = 1, simplify = FALSE)
}
