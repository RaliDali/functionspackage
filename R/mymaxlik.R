#' @title mymaxlik
#'
#' @param lfun The function to use (binomial, poisson, etc)
#' @param x Vector of values
#' @param param Parameter to calculate (prob, theta, lambda, etc)
#' @param ... Additional arguments, if needed
#' @import graphics
#'
#' @return Provides index values for different variables, and a graph illustrating the maximum likelihood value
#' @export
#'
#' @examples
#' \dontrun{mymaxlik(x = c(3, 3, 4, 3, 4, 5, 5, 4), param = seq(0, 1, length = 1000), lfun = function(x, param) log(dbinom(x,prob=param,size=10)), xlab = expression(pi), main = "Binomial, n = 20")}

x = NULL
mymaxlik=function(lfun,x,param,...){
  np=length(param)
  z=outer(x,param,lfun) # This creates an ordered matrix. The x-value is the first, left value (running from x to xn), while the param value is the second, right value
  y=apply(z,2,sum)

  plot(param,y,col="Blue",type="l",lwd=2,...)
  i=max(which(y==max(y))) # Calculates the maximum likelihood value of the two parameters
  abline(v=param[i],lwd=2,col="Red")

  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
