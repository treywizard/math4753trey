#' myncurve function
#'
#' prints out a graph, region of the area we are looking for, and area itself
#'
#' @param mu Mean of the normal function
#' @param sigma Standard deviation of the normal function
#' @param le Left endpoint
#' @param re Right endpoint
#' @param lee Does the line go on the left endpoint?
#' @param ree Does the line go on the right endpoint?
#'
#' @return
#' @export
#'
#' @examples
myncurve = function(mu, sigma, le, re, lee, ree){
  if(lee == TRUE)
  {
    le = le - 0.5
  }
  else
  {
    le = le + 0.5
  }
  if(ree == TRUE)
  {
    re = re + 0.5
  }
  else
  {
    re = re - 0.5
  }
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(le, re, length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(le,seq(le, re, length=1000),re),c(0,ycurve,0),col="Red")
  text(x = (mu-3*sigma) * 0.8, y = (mu + 3*sigma) * 0.8, paste("Area = ", round(pnorm(re, mu, sigma) - pnorm(le, mu,   sigma)), sep=""))
}
