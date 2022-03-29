library(ggplot2)
library(dplyr)
library(Intro2R)


#' @title myddt
#'
#' @param df The data file to pull from (must be a .csv file)
#' @param SPECIES The species you wish to filter by
#' @param col The color you wish to make the plot's points (variable)
#'
#' @return Produces a plot of length vs weight for declared species, table of relative frequency values per river, and tables of species-filtered and unfiltered data
#' @export
#' @import ggplot2
#' @import dplyr
#' @import Intro2R
#'
#' @examples
#' \dontrun{myddt(df = ddt, SPECIES = "CCATFISH")}

myddt = function(df, SPECIES, col) {
  df1 = df %>% filter(SPECIES == {{SPECIES}}) #This was the solution I needed, thank you!
  g = ggplot(df1, aes_string(x = "WEIGHT", y = "LENGTH")) +
    geom_point(aes_string(color = "RIVER")) +
    geom_smooth(formula = y~x + I(x^2), method = "lm") +
    labs(title = "Jay Leger")
  print(g)
  #head(df1)
  write.csv(df1, paste0("LvsWfor", SPECIES, ".csv"), row.names = FALSE)
  dflist = df
  df1list = df1
  RFtab = table(df$RIVER)/length(df$RIVER)
  print(dflist)
  print(df1list)
  print(RFtab)
}
