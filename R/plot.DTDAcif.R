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
plot.DTDAcif <-  function(x, main = "", xlab = "", ylab = "", ...) {

  res <- x
  if (is.null(res$method)) {

    graphics::plot(res2$time[order(res$time)], cumsum(res$cif),
                   type = "s", ylim = c(0, 1), xlim = c(min(res$time), max(res$time)),
                   main = main, xlab = xlab, ylab = ylab)

  } else {

    if (res$method == "dep") {

      graphics::plot(unlist(res$time[1])[order(unlist(res$time[1]))], cumsum(unlist(res$cif[[1]])),
                     type = "s", ylim = c(0, 1), xlim = c(min(unlist(res$time)), max(unlist(res$time))),
                     main = main, xlab = xlab, ylab = ylab)

      for(i in 2:length(res$cif)){
        graphics::lines(unlist(unlist(res$time[i]))[order(unlist(unlist(res$time[i])))],
              cumsum(unlist(res$cif[[i]])), type = "s", col = i)
      }
    }


    if (res$method == "indep") {

      graphics::plot(res$time[order(res$time)], cumsum(unlist(res$cif[1])),
                     type = "s", ylim = c(0, 1), xlim = c(min(res$time), max(res$time)),
                     main =  main, xlab = xlab, ylab = ylab)

      for(i in 2:length(res$cif)){
        graphics::lines(res$time[order(res$time)], cumsum(unlist(res$cif[i])),
                        type = "s", col = i)
      }
    }
  }
}





#-------------------------------------------------------------------------------
# Ejemplos #
#-------------------------------------------------------------------------------

# Descomentar para ejecutar

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
#
# res2 <- DTDAcif(x, u, v)
# plot(res2)
#
#
# x <- round(x, 1)
# u <- pmin(round(u, 1), x)
# v <- pmax(round(v, 1), x)
#
#
# res <- DTDAcif(x, u, v, comp.event = z, method = "dep")
# plot(res, ylab = "resR")
#
# res1 <- DTDAcif(x, u, v, comp.event = z, method = "indep")
# plot(res1, xlab = "asd")
#
# res2 <- DTDAcif(x, u, v)
# plot(res2)
