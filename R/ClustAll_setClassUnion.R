# setClassUnion includes defined new classes -----------------------------------
#' @import mice
setClassUnion("matrixOrNULL", c("matrix", "NULL")) 
setClassUnion("midsOrNULL", c("mids", "NULL")) 
setClassUnion("midsOrNA", c("mids", "missing")) 
setClassUnion("listOrNULL", c("list", "NULL")) 
setClassUnion("numericOrNA", c("numeric", "missing")) 
setClassUnion("logicalOrNA", c("logical", "missing"))