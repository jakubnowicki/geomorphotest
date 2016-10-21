expand.deformation <- function(data, deformation.type = 'static', deformation.amount = 0.2, deformation.sd = NULL) {
        proportions.vector <- morphoutils::shape.proportions(data)
        mean.proportion <- mean(proportions.vector)
        n.specimens <- dim(data)[3]
        if (deformation.type == 'static') {
                a.vector <- rep(deformation.amount, n.specimens)
        } else {
                if (deformation.type == 'normal' & !is.null(deformation.sd)) {
                        a.vector <- rnorm(n = n.specimens, mean = deformation.amount, sd =  deformation.sd)
                } else {
                        if (deformation.type == 'uniform' & length(deformation.amount)==2) {
                                a.vector <- runif(n = n.specimens, min = deformation.amount[1],
                                                  max = deformation.amount[2])
                        } else {
                                stop('Wrong deformation type!')
                        }
                }
        }
        output <- array(data = NA, dim = dim(data))
        for (i in 1:n.specimens) {
                ifelse(proportions.vector[i]>mean.proportion, yes = a <- 1 - a.vector[i], no = a <- 1 + a.vector[i])
                output[,,i] <- deformacja(data[,,i], strain.matrix = strain.matrix(a = a, theta = 0))
        }
        return(output)
}
