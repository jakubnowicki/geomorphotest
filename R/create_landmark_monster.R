#' Create landmark monster
#'
#' @param n.landmarks number of landmarks
#' @param reflect reflect
#' @param base.seed seed
#'
#' @export
#'

create.landmark.set <- function(n.landmarks = 5, reflect = FALSE, base.seed = NA) {
        if (!is.na(base.seed)) {
                set.seed(base.seed)
        }
        output <- matrix(rnorm(n.landmarks*2),ncol=2)
        if (reflect == FALSE) {
                return(output)
        } else {
                symmetry.line <- sample(1:n.landmarks,2)
                tmp <- data.frame(y = output[symmetry.line,2], x = output[symmetry.line,1])
                tmp.lm <- lm(y ~ x, data = tmp)
                inter <- coef(tmp.lm)[1]
                a <- coef(tmp.lm)[2]
                output[,2] <- output[,2] - inter
                sym.matrix <- matrix(0,ncol=2,nrow = n.landmarks-2)
                tmp.matrix <- output[-symmetry.line,]
                for (i in 1:(n.landmarks-2)) {
                        sym.matrix[i,] <- reflect(tmp.matrix[i,],a = a)
                }
                tmp <- as.data.frame(rbind(output,sym.matrix))
                colnames(tmp) <- c('x','y')
                tmp.s <- tmp[symmetry.line,]
                lm.2 <- lm(y ~ x, tmp.s)
                pred <- predict(lm.2, newdata = tmp)
                d <- cbind(tmp,pred)
                left <- as.numeric(rownames(d[d$y<d$pred,]))
                left <- left[!(left %in% symmetry.line)]
                l.vector <- 1:n.landmarks
                l.vector <- l.vector[!(l.vector %in% symmetry.line)]
                l.matrix <- cbind(l.vector,((n.landmarks+1):(n.landmarks*2-2)))
                l.matrix <- rbind(l.matrix,l.matrix[,c(2,1)])
                right <- l.matrix[l.matrix[,1] %in% left,2]
                return(list(landmarks = rbind(output,sym.matrix),
                            symmetry.parameters = list(mid = symmetry.line, left = left,right = right)))
        }
}
