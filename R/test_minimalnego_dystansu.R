test.minimalnego.dystansu <- function(iterations = 1000, n.landmarks = 10, reflect = FALSE, n.monsters = 20,
                                      sd.monsters = 0.1, distance = "full", a.min = 0.1, a.max = 1.9, a.skok = 0.1,
                                      theta.min = -1.6, theta.max = 1.6, theta.skok = 0.1, iteracje = 10,
                                      istotne.cyfry = 10, a.interval = c(0.7,1.3), theta.interval = c(-0.4,0.4),
                                      noise.a.sd = 0.01, noise.theta.sd = 0.01, test.seed = NA) {
        output <- matrix(NA, ncol = 3, nrow = iterations)
        if (!is.na(test.seed)) {
                set.seed(test.seed)
                seed.matrix <- matrix(sample(1:1000000, size = 3*iterations), ncol = 3)
        }
        tmp.seed.1 <- NA
        tmp.seed.2 <- NA
        tmp.seed.3 <- NA
        if (reflect == FALSE) {
                for (i in 1:iterations) {
                        # stworzenie zestawu testowego
                        if (!is.na(test.seed)) {
                                tmp.seed.1 <- seed.matrix[i,1]
                                tmp.seed.2 <- seed.matrix[i,2]
                                tmp.seed.3 <- seed.matrix[i,3]
                        }
                        test <- create.test.landmark.set(n.landmarks = n.landmarks, reflect = FALSE, 
                                                         n.monsters = n.monsters,sd = sd.monsters,
                                                         base.seed = tmp.seed.1, random.seed = tmp.seed.2)
                        # superimpozycja
                        gpg.test <- gpagen(test)
                        # analiza dystansu (zwykła i RetroGeoMorph)
                        test.dist <- distance.matrix(gpg.test, distance = distance)
                        test.retro.dist <- minimal.distance.matrix(data = gpg.test, a.min = a.min, a.max = a.max,
                                                                   a.skok = a.skok, theta.min = theta.min,
                                                                   theta.max = theta.max, theta.skok = theta.skok,
                                                                   iteracje = iteracje, istotne.cyfry = istotne.cyfry,
                                                                   distance = distance)
                        # deformacja zestawu testowego
                        deformed.test <- deform.set(data = test, a.interval = a.interval,
                                                    theta.interval = theta.interval, noise.a.sd = noise.a.sd,
                                                    noise.theta.sd = noise.theta.sd, deform.set.seed = tmp.seed.3)
                        # superimpozycja
                        gpg.deformed.test <- gpagen(deformed.test)
                        # analiza dystansu (zwykła i RetroGeoMorph)
                        test.deformed.dist <- distance.matrix(gpg.deformed.test, distance = distance)
                        test.deformed.retro.dist <- minimal.distance.matrix(data = gpg.deformed.test, a.min = a.min,
                                                                            a.max = a.max, a.skok = a.skok,
                                                                            theta.min = theta.min,theta.max = theta.max,
                                                                            theta.skok = theta.skok, iteracje = iteracje,
                                                                            istotne.cyfry = istotne.cyfry,
                                                                            distance = distance)
                        # porównanie dystansów (korelacja rangowa Spearmana)
                        td <- as.vector(as.dist(test.dist))
                        trd <- as.vector(as.dist(test.retro.dist$distance))
                        tdd <- as.vector(as.dist(test.deformed.dist))
                        tdrd <- as.vector(as.dist(test.deformed.retro.dist$distance))
                        td_trd <- cor(td,trd,method = 'spearman')
                        td_tdd <- cor(td,tdd,method = 'spearman')
                        td_tdrd <- cor(td,tdrd,method = 'spearman')
                        output[i,] <- c(td_trd,td_tdd,td_tdrd)
                }
        } else {
                for (i in 1:iterations) {
                        if (!is.na(test.seed)) {
                                tmp.seed.1 <- seed.matrix[i,1]
                                tmp.seed.2 <- seed.matrix[i,2]
                                tmp.seed.3 <- seed.matrix[i,3]
                        }
                        # stworzenie zestawu testowego
                        test <- create.test.landmark.set(n.landmarks = n.landmarks, reflect = TRUE, 
                                                         n.monsters = n.monsters,sd = sd.monsters,
                                                         base.seed = tmp.seed.1, random.seed = tmp.seed.2)
                        # superimpozycja
                        test.sym <- symetria.2(dane = test$landmarks, mid = test$symmetry.parameters$mid,
                                               left = test$symmetry.parameters$left, 
                                               right = test$symmetry.parameters$right)
                        gpg.test <- gpagen(test.sym)
                        # analiza dystansu (zwykła i RetroGeoMorph)
                        test.dist <- distance.matrix(gpg.test, distance = distance)
                        test.retro.dist <- minimal.distance.matrix(data = gpg.test, a.min = a.min, a.max = a.max,
                                                                   a.skok = a.skok, theta.min = theta.min,
                                                                   theta.max = theta.max, theta.skok = theta.skok,
                                                                   iteracje = iteracje, istotne.cyfry = istotne.cyfry,
                                                                   distance = distance)
                        # deformacja zestawu testowego
                        deformed.test <- deform.set(data = test, a.interval = a.interval,
                                                    theta.interval = theta.interval, noise.a.sd = noise.a.sd,
                                                    noise.theta.sd = noise.theta.sd, deform.set.seed = tmp.seed.3)
                        # superimpozycja
                        test.deformed.sym <- symetria.2(dane = deformed.test, mid = test$symmetry.parameters$mid,
                                                        left = test$symmetry.parameters$left, 
                                                        right = test$symmetry.parameters$right)
                        gpg.deformed.test <- gpagen(test.deformed.sym)
                        # analiza dystansu (zwykła i RetroGeoMorph)
                        test.deformed.dist <- distance.matrix(gpg.deformed.test, distance = distance)
                        test.deformed.retro.dist <- minimal.distance.matrix(data = gpg.deformed.test, a.min = a.min,
                                                                            a.max = a.max, a.skok = a.skok,
                                                                            theta.min = theta.min,theta.max = theta.max,
                                                                            theta.skok = theta.skok, iteracje = iteracje,
                                                                            istotne.cyfry = istotne.cyfry,
                                                                            distance = distance)
                        # porównanie dystansów (korelacja rangowa Spearmana)
                        td <- as.vector(as.dist(test.dist))
                        trd <- as.vector(as.dist(test.retro.dist$distance))
                        tdd <- as.vector(as.dist(test.deformed.dist))
                        tdrd <- as.vector(as.dist(test.deformed.retro.dist$distance))
                        td_trd <- cor(td,trd,method = 'spearman')
                        td_tdd <- cor(td,tdd,method = 'spearman')
                        td_tdrd <- cor(td,tdrd,method = 'spearman')
                        output[i,] <- c(td_trd,td_tdd,td_tdrd)
                }
        }
        output <- as.data.frame(output)
        colnames(output) <- c('td_trd','td_tdd','td_tdrd')
        return(output)
}