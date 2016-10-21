#' Create random set of landmark monsters
#'
#' @param base.monster initial monster
#' @param n.monsters number of monsters
#' @param sd standard deviation of randomization
#' @param reflect reflect
#' @param random.seed seed
#'
#' @return Set of landmark monsters
#' @export
#'


randomize.monster <- function(base.monster, n.monsters, sd = 0.1, reflect = FALSE, random.seed = NA) {
        if (class(base.monster)!='list') {
                n.landmarks <- nrow(base.monster)
                output <- array(data = 0,dim = c(n.landmarks,2,n.monsters))
                output[,,1] <- base.monster
                base.monster.l <- base.monster
        } else {
                n.landmarks <- nrow(base.monster$landmarks)
                output <- array(data = 0,dim = c(n.landmarks,2,n.monsters))
                output[,,1] <- base.monster$landmarks
                base.monster.l <- base.monster$landmarks
        }
        if (!is.na(random.seed)) {
                set.seed(random.seed)
                seed.vector <- sample(1:100000, size = n.monsters-1)
        }
        if (reflect == FALSE) {
                for (i in 2:n.monsters) {
                        if (!is.na(random.seed)) {
                                set.seed(seed.vector[i-1])
                        }
                        output[,,i] <- base.monster.l + rnorm(n = 2*n.landmarks, mean = 0, sd = sd)
                }
                return(output)
        } else {
                left <- base.monster$symmetry.parameters$left
                mid <- base.monster$symmetry.parameters$mid
                right <- base.monster$symmetry.parameters$right
                n.base <- ((n.landmarks+2)/2)
                base.landmarks <- base.monster.l[1:n.base,]
                for (i in 2:n.monsters) {
                        if (!is.na(random.seed)) {
                                set.seed(seed.vector[i-1])
                        }
                        new.monster <- base.landmarks + rnorm(n = 2*n.base, mean = 0, sd = sd)
                        tmp.s <- data.frame(y = new.monster[mid,2], x = new.monster[mid,1])
                        tmp.lm <- lm(y ~ x, data = tmp.s)
                        inter <- coef(tmp.lm)[1]
                        a <- coef(tmp.lm)[2]
                        new.monster[,2] <- new.monster[,2] - inter
                        sym.matrix <- matrix(0,ncol=2,nrow = n.base-2)
                        tmp.matrix <- new.monster[-mid,]
                        for (j in 1:(n.base-2)) {
                                sym.matrix[j,] <- reflect(tmp.matrix[j,],a = a)
                        }
                        tmp <- as.matrix(rbind(new.monster,sym.matrix))
                        output[,,i] <- tmp
                }
                return(output)
        }
}
