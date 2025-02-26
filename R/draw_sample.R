#' Title
#'
#' @param config_list A list of configurations
#' @param N_pop Number of subpopulations
#'
#' @returns A random sample drawn from the distribution specified by the dist component
#'
#' @examples
draw_sample <- function(config_list, N_pop){

  if(class(config_list) == "list"){

    if(config_list$dist == "lognormal")
      x <- rlnorm(1, meanlog = config_list$mu, sdlog = config_list$sd)

    if(config_list$dist == "gamma")
      x <- rgamma(1, shape = config_list$shape, rate = config_list$rate)

    if(config_list$dist == "uniform")
      x <- runif(1, min = config_list$min, max = config_list$max)

    if(config_list$dist == "beta")
      x <- rbeta(1, shape1 = config_list$shape1, shape2 = config_list$shape2)

    return(rep(x, N_pop))
  } else return(config_list)
}
