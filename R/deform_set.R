#' Deform landmark set
#'
#' @param data data
#' @param a.interval values of a
#' @param theta.interval values of theta
#' @param noise.a.sd standard deviation of a
#' @param noise.theta.sd standard deviation of theta
#' @param deform.set.seed deformation seed
#'
#' @export
#'
deform.set <- function(data, a.interval, theta.interval,noise.a.sd = 0.01 ,noise.theta.sd = 0.01, deform.set.seed = NA, angle.interval = NULL, nondeformed = 0) {
        if (class(data)=='list') {
                landmarks <- data$landmarks
        } else {
                landmarks <- data
        }
        n.specimens <- dim(landmarks)[3]
        if(nondeformed > 0 & nondeformed < 1) {
                nondeformed <- round(n.specimens*nondeformed, digits = 0)
        }
        nondeformed.specimens <- sample(1:n.specimens, size = nondeformed)
        output <- array(dim = dim(landmarks))
        if (!is.na(deform.set.seed)) {
                set.seed(deform.set.seed)
                tmp.seed <- sample(1:1000000, 2)
                set.seed(tmp.seed[1])
                seed.matrix <- matrix(sample(1:100000,size = 2*n.specimens),ncol = 2)
                set.seed(tmp.seed[2])
                seed.vector <- sample(1:100000, size = n.specimens)
        }
        for (i in 1:n.specimens) {
                if (i %in% nondeformed.specimens) {
                        output[,,i] <- landmarks[,,i]
                } else {
                        if (!is.na(deform.set.seed)) {
                                set.seed(seed = seed.matrix[i,1])
                        }
                        a.tmp <- runif(n = 1, min = a.interval[1],max = a.interval[2])
                        if (!is.na(deform.set.seed)) {
                                set.seed(seed = seed.matrix[i,2])
                        }
                        theta.tmp <- runif(n = 1,min = theta.interval[1],max = theta.interval[2])
                        if (!is.null(angle.interval)) {
                                angle <- runif(n = 1, min = angle.interval[1], max = angle.interval[2])
                        } else {
                                angle <- NULL
                        }
                        ifelse(!is.na(deform.set.seed), yes = deform.seed <- NA, no = deform.seed <- seed.vector[i])
                        output[,,i] <- deform.with.noise(data = landmarks[,,i],a = a.tmp,theta = theta.tmp,
                                                         noise.a.sd = noise.a.sd,noise.theta.sd = noise.theta.sd,
                                                         deform.seed = deform.seed, angle = angle)
                }
        }
        return(output)
}
