#' Reflect landmarks
#'
#' @param data landarks to reflect
#' @param a a
#'
#' @return Reflected data.
#' @export
#'
#'

reflect <- function(data, a) {
        x <- data[1]
        y <- data[2]
        x.2 <- (((1-(a^2))/(1+(a^2))))*x + (((2*a)/(1+(a^2))))*y
        y.2 <- (2*a/(1+a^2))*x - ((1-a^2)/(1+a^2))*y
        return(c(x.2,y.2))
}
