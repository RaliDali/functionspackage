#' myread
#'
#' @param csv
#'
#' @return Reads a CSV file
#' @export
#' @importFrom utils  read.csv
#'
#' @examples
#' \dontrun{mpg.df=myread("EPAGAS.csv")}
myread=function(csv){
  read.csv(file.choose())
}
