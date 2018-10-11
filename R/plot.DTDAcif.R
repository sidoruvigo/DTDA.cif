#' S3 method to plot a DTDAcif object by using the generic plot function.
#'
#' @title plot.DTDAcif
#' @param x DTDAcif object.
#'
#'
#' @section Acknowledgements:
#' \itemize{
#' \item{Jacobo de Uña-Álvarez was supported by Grant MTM2014-55966-P, Spanish Ministry of Economy and Competitiveness.}
#' \item{José Carlos Soage was supported by Red Tecnológica de Matemática Industrial (Red TMATI), Cons. de Cultura, Educación e OU, Xunta de Galicia (ED341D R2016/051) and by Grupos de Referencia Competitiva, Consolidación y Estructuración de Unidades de Investigación Competitivas del SUG, Cons. de Cultura, Educación e OU, Xunta de Galicia (GRC ED431C 2016/040).}
#' }
#'
#' @references TODO
#'
#' @author
#' \itemize{
#' \item{de Uña-Álvarez, Jacobo.}
#' \item{Soage González, José Carlos.}
#' \item{Maintainer: José Carlos Soage González. \email{jsoage@@uvigo.es}}
#' }
#'
#'
#' @export
plot.DTDAcif <-  function(x, ...) {

  res <- x

    if (res$method == "dep" & res$ties == TRUE) {

      graphics::plot(unlist(res$x[1])[order(unlist(res$x[1]))], cumsum(unlist(res$w[[1]])),
           type = "s", ylim = c(0, 1), xlim = c(min(unlist(res$data)), max(unlist(res$data))))

      for(i in 2:length(unique(res$z))){
        graphics::lines(unlist(unlist(res$x[i]))[order(unlist(unlist(res$x[i])))],
              cumsum(unlist(res$w[[i]])), type = "s", col = i)
      }

    }

  if (res$method == "dep" & res$ties == FALSE){

    graphics::plot(res$x[[1]]$time, cumsum(unlist(res$w[[1]])), type = "s",
           ylim = c(0, 1), xlim = c(min(unlist(res$data)), max(unlist(res$data))))

      for(i in 2:length(unique(res$z))){
        graphics::lines(unlist(res$x[[i]]$time), cumsum(unlist(res$w[[i]])),
              type = "s", col = i)
      }
    }



  if (res$method == "indep" & res$ties == TRUE) {

    graphics::plot(unlist(res$x)[order(unlist(res$x))], cumsum(unlist(res$w[1])),
           type = "s", ylim = c(0, 1), xlim = c(min(unlist(res$x)), max(unlist(res$x))))

      for(i in 2:length(unique(res$z))){
        graphics::lines(unlist(res$x)[order(unlist(res$x))], cumsum(unlist(res$w[i])),
              type = "s", col = i)
      }


    }

  if (res$method == "indep" & res$ties == FALSE){

    graphics::plot(res$x$time, cumsum(unlist(res$w[[1]])), type = "s", ylim = c(0, 1),
           xlim = c(min(unlist(res$data)), max(unlist(res$data))))
      for(i in 2:length(unique(res$z))){
        x <- unlist(res$data)
        graphics::lines(x[order(x)], cumsum(unlist(res$w[[i]])),
              type = "s", col = i)
      }
    }
  }





#-------------------------------------------------------------------------------
# Ejemplos #
#-------------------------------------------------------------------------------

# Descomentar para ejecutar

#~~~~~~~~~~~~~
# Sin empates
#~~~~~~~~~~~~~

# set.seed(06062018)
#
# n <- 5000
#
# r1 <- 1
# r2 <- 3
# r <- r1 + r2
# x <- rexp(n, rate = r)
# z <- rbinom(n, 2, r1 / r)
# u <- 0.75 * rbeta(n, 1 + z, 1) - 0.25    #runif(n,-.25,.5)
# v <- u + 0.75
#
# for (i in 1:n) {
#   while (u[i] > x[i] | v[i] < x[i]) {
#     x[i] <- rexp(1, rate = r)
#     z[i] <- rbinom(1, 1, r1 / r)
#     u[i] <- 0.75 * rbeta(1, 1 + z, 1) - 0.25     #runif(1,-.25,.5)
#     v[i] <- u[i] + 0.75
#   }
# }
#
#
#
#
# res <- DTDAcif(x, u, v, comp.event = z, method = "dep")
# plot(res)
#
# res1 <- DTDAcif(x, u, v, comp.event = z, method = "indep")
# plot(res1)


#~~~~~~~~~~~~~
# Con empates
#~~~~~~~~~~~~~


# set.seed(06062018)
#
# n <- 5000
#
# r1 <- 1
# r2 <- 3
# r <- r1 + r2
# x <- rexp(n, rate = r)
# z <- rbinom(n, 2, r1 / r)
# u <- 0.75 * rbeta(n, 1 + z, 1) - 0.25    #runif(n,-.25,.5)
# v <- u + 0.75
#
# for (i in 1:n) {
#   while (u[i] > x[i] | v[i] < x[i]) {
#     x[i] <- rexp(1, rate = r)
#     z[i] <- rbinom(1, 1, r1 / r)
#     u[i] <- 0.75 * rbeta(1, 1 + z, 1) - 0.25     #runif(1,-.25,.5)
#     v[i] <- u[i] + 0.75
#   }
# }
#
#
# x <- round(x, 1)
# u <- pmin(round(u, 1), x)
# v <- pmax(round(v, 1), x)
#
#
# res2 <- DTDAcif(x, u, v, comp.event = z, method = "dep")
# plot(res2)
#
# res3 <- DTDAcif(x, u, v, comp.event = z, method = "indep")
# plot(res3)

