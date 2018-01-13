#' Deform landmark monster
#'
#' @param data landamrk monster
#' @param a a
#' @param theta theta
#' @param noise.a.sd standard deviation of a
#' @param noise.theta.sd standard deviation of theta
#' @param deform.seed deformation seed
#' @param angle angle of deformation
#'
#' @import morphoutils
#' @export
#'
deform.with.noise <- function(data,a,theta,noise.a.sd = 0.01 ,noise.theta.sd = 0.01, deform.seed = NA, angle = NULL) {
        n.landmarks <- nrow(data)
        output <- matrix(0,nrow = n.landmarks,ncol = 2)
        if (!is.na(deform.seed)) {
                set.seed(deform.seed)
                seed.matrix <- matrix(sample(1:100000,size = 2*n.landmarks),ncol = 2)
        }
        if (!is.null(angle)) {
                angle.matrix <- matrix(data = c(cos(angle), sin(angle),-sin(angle),cos(angle)), nrow = 2,ncol = 2)
                data <- angle.matrix %*% t(data)
                data <- t(data)
        }
        for (i in 1:n.landmarks) {
                if (!is.na(deform.seed)) {
                        set.seed(seed = seed.matrix[i,1])
                }
                tmp.a <- a + rnorm(n = 1,mean = 0,sd = noise.a.sd)
                if (!is.na(deform.seed)) {
                        set.seed(seed = seed.matrix[i,2])
                }
                tmp.theta <- theta + rnorm(n = 1,mean = 0,sd = noise.theta.sd)
                strain <- morphoutils::strain.matrix(a = tmp.a, theta = tmp.theta)
                output[i,] <- morphoutils::deformacja(data = t(as.matrix(data[i,])),strain.matrix = strain)
        }
        return(output)
}
