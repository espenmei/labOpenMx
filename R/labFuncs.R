#' @title Create labels
#' @description Labels all elements of lower triangular matrices
#' @param lab Prefix character
#' @param i Number of rows/columns
#' @return String vector arranged by column
#' @export
#' @examples
#' library(OpenMx)
#'
#' i = 5
#' mxMatrix("Lower", i, i, labels = labL("a", i))
labL = function(lab, i, sep = "_") {
  unlist(lapply(1:i, function(x) paste0(lab, x:i, sep, x)))
}

#' @title Create labels
#' @description Labels all elements below the diaqonal of square matrices
#' @param lab Prefix character
#' @param i Number of rows/columns
#' @return String vector arranged by column
#' @export
#' @examples
#' library(OpenMx)
#'
#' i = 5
#' mxMatrix("Stand", i, i, labels = labL("a", i))
labLL = function(lab, i, sep = "_") {
  unlist(lapply(1:(i - 1), function(x) paste0(lab, (x + 1):i, sep, x)))
}

#' @title Create labels
#' @description Labels all elements of matrices
#' @param lab Prefix character
#' @param i Number of rows
#' @param j Number of columns
#' @return String vector arranged by column
#' @export
#' @examples
#' library(OpenMx)
#'
#' i = 5
#' j = 3
#' mxMatrix("Full", i, j, labels = labF("a", i, j))
labF = function(lab, i, j, sep = "_") {
  unlist(lapply(1:j, function(x) paste0(lab, 1:i, sep, x)))
}

#' @title Create labels
#' @description Labels all diagonal elements of symmetric matrices
#' @param lab Prefix character
#' @param i Number of rows/columns
#' @return String vector arranged by column
#' @export
#' @examples
#' library(OpenMx)
#'
#' i = 5
#' mxMatrix("Diag", i, i, labels = labD("a", i))
labD = function(lab, i, sep = "_") {
  paste0(lab, 1:i, sep, 1:i)
}
