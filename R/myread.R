#' myread
#'
#' @param csv
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{mpg.df=myread("EPAGAS.csv")}
myread=function(csv){
  read.csv(file.choose())
}
