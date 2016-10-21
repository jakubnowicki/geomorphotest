#' Create test landmark set
#'
#' @param n.landmarks number of landmarks
#' @param reflect reflect
#' @param n.monsters number of monsters
#' @param sd randomization standard deviation
#' @param base.seed base monster seed
#' @param random.seed randomization seed
#'
#' @export
#'

create.test.landmark.set <- function(n.landmarks = 5, reflect = FALSE, n.monsters = 10, sd = 0.1, base.seed = NA,
                                     random.seed = NA) {
        base.monster <- create.landmark.set(n.landmarks = n.landmarks,reflect = reflect, base.seed = base.seed)
        output <- randomize.monster(base.monster = base.monster, n.monsters = n.monsters, sd = sd,reflect = reflect,
                                    random.seed = random.seed)
        ifelse(class(base.monster) == "list", yes = return(list(landmarks = output,
                symmetry.parameters = base.monster$symmetry.parameters)),
                no = return(output))
}
