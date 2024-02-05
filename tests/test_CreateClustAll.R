###########################################
# All possibilities to create the object
###########################################
require("ClustAll")
data("BreastCancerWisconsin", package = "ClustAll") # load example data
data("BreastCancerWisconsinMISSING", package = "ClustAll") # load example data

test_creatingClustAll <- function() {
  createClustAll(data = wdbc, colValidation = "Diagnosis",
                 nImputation = NULL, dataImputed = NULL)
  createClustAll(wdbcNA, nImputation = 5,  colValidation = "Diagnosis")
  createClustAll(wdbcNA, dataImputed = wdbcMIDS,
                 colValidation = "Diagnosis")
}
