#' S3 method to plot a DTDAcif object by using the generic plot function.
#'
#' @title plot.DTDAcif
#' @param x DTDAcif object.
#' @param intervals TODO
#' @param level TODO
#' @param main TODO
#' @param xlab TODO
#' @param ylab TODO
#' @param ... TODO
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
plot.DTDAcif <- function(x, intervals = TRUE, level = 0.95,  main = "", xlab = "", ylab = "", ...) {

  if (!inherits(x, "DTDAcif")) {
    stop("'x' must be of class 'DTDAcif'")
  }

  r <- res
  if (is.null(r$method)) {

    graphics::plot(c(min(r$data$x), r$data$x[order(r$data$x)]), c(0, cumsum(r$cif)),
         type = "s", ylim = c(0, 1), xlim = c(min(r$data$x),
                                              max(r$data$x[order(r$data$x)])),
         main = main, xlab = xlab, ylab = ylab)

    if(intervals == T) {
      cif <- unlist(lapply(1:length(unique(r$data$x)),
                           function(i) stats::approx(r$data$x[order(r$data$x)], cumsum(r$cif),
                                              xout = unique(r$data$x[order(r$data$x)])[i],
                                              ties = max, yleft = stats::approx(r$data$x[order(r$data$x)], cumsum(r$cif),
                                                                         xout = min(r$data$x[order(r$data$x)]), ties = min)$y,
                                              method = "constant", rule = 2)$y))


      lim.sup <- cif + stats::qnorm((1 - level) / 2,  lower.tail = F) * r$sd.boot
      lim.sup <- unlist(lapply(1:length(lim.sup), function(i) if(lim.sup[i] > 1) lim.sup[i] = 1 else lim.sup[i] = lim.sup[i]))

      lim.inf <- cif - stats::qnorm((1 - level) / 2, lower.tail = F ) * r$sd.boot
      lim.inf <- unlist(lapply(1:length(lim.inf), function(i) if(lim.inf[i] < 0) lim.inf[i] = 0  else lim.inf[i] = lim.inf[i]))


      graphics::lines(unique(r$data$x)[order(unique(r$data$x))], lim.sup, type = "s", col = "lightgray", lty = 2)
      graphics::lines(unique(r$data$x)[order(unique(r$data$x))], lim.inf, type = "s", col = "lightgray", lty = 2)
    }

  } else {

    if (r$method == "dep") {

      nz <- length(unique(r$data$z))
      cif <- vector("list", nz)

      for(j in 1:nz){
        cif[[j]] <- unlist(lapply(1:length(unique(unlist(r$time[j]))),
                                  function(i) stats::approx(unlist(r$time[j])[order(unlist(r$time[j]))],
                                                     cumsum(unlist(r$cif[[j]])),
                                                     xout = unique(unlist(r$time[j])[order(unlist(r$time[j]))])[i],
                                                     ties = max, yleft = stats::approx(unlist(r$time[j])[order(unlist(r$time[j]))],
                                                                                cumsum(unlist(r$cif[[j]])),
                                                                                xout = min(unlist(r$time[j])[order(unlist(r$time[j]))]),
                                                                                ties = min)$y,
                                                     method = "constant", rule = 2)$y))
        # pointsb[[j]] <- na.omit(unlist(pointsb[[j]]))
      }



      lim.sup <- vector("list", nz)
      lim.inf <- vector("list", nz)

      for(p in 1:nz){
        lim.sup1 <- cif[[p]] + stats::qnorm((1-level )/2, lower.tail = F) * r$sd.boot[[p]]
        lim.sup[[p]] <- unlist(lapply(1:length(lim.sup1), function(i) if(lim.sup1[i] > 1) lim.sup1[i] = 1 else lim.sup1[i] = lim.sup1[i]))

        lim.inf1 <- cif[[p]] - stats::qnorm((1-level )/2, lower.tail = F ) * r$sd.boot[[p]]
        lim.inf[[p]] <- unlist(lapply(1:length(lim.inf1), function(i) if(lim.inf1[i] < 0) lim.inf1[i] = 0  else lim.inf1[i] = lim.inf1[i]))

      }

      invisible(lapply(1:length(r$cif),
                       function(i){
                         if  (i == 1) {
                           graphics::plot(unlist(r$time[i])[order(unlist(r$time[i]))],
                                          cumsum(unlist(r$cif[i])), type = "s",
                                          ylim = c(0, 1),
                                          xlim = c(min(unlist(r$time[i])),
                                                   max(unlist(r$time[i]))),
                                          main = main, xlab = xlab, ylab = ylab)
                         } else {
                           graphics::lines(unlist(r$time[i])[order(unlist(r$time[i]))], cumsum(unlist(r$cif[[i]])),
                                           type = "s", col = i)
                           if (intervals == T) {
                             for(u in 1:nz) {
                               graphics::lines(unique(unlist(r$time[u])[order(unlist(r$time[u]))]), lim.sup[[u]], type = "s",
                                     col = "lightgray", lty = 2)
                               graphics::lines(unique(unlist(r$time[u])[order(unlist(r$time[u]))]), lim.inf[[u]], type = "s",
                                     col = "lightgray", lty = 2)
                             }
                           }
                         }
                       }))
    }


    if (r$method == "indep") {

      nz <- length(unique(r$data$z))
      cif <- vector("list", nz)

      data <- data.frame(cbind(r$data$x, r$data$z))
      colnames(data) <- c("x", "z")
      cif <- lapply(1:nz, function(j) unlist(lapply(1:length(unique(subset(data.frame(data),
                                                                           data.frame(data)$z == j))$x),
                                                    function(i) stats::approx(unlist(unlist(r$time))[order(unlist(unlist(r$time)))],
                                                                       cumsum(unlist(r$cif[[j]])),
                                                                       xout = unique(subset(data.frame(data),
                                                                                            data.frame(data)$z == j)$x[order(subset(data.frame(data), data.frame(data)$z == j)$x)])[i],
                                                                       ties = max,
                                                                       yleft = stats::approx(r$data$x[order(r$data$x)],
                                                                                      cumsum(unlist(r$cif[[j]])),
                                                                                      xout = min(r$data$x[order(r$data$x)]),
                                                                                      ties = min)$y,
                                                                       method = "constant", rule = 2)$y)))

      for(w in 1:nz){
        cif[[w]] <- stats::na.omit(cif[[w]])
      }

      lim.sup <- vector("list", nz)
      lim.inf <- vector("list", nz)

      for(p in 1:nz){
        lim.sup1 <- cif[[p]] + stats::qnorm((1-level )/2, lower.tail = F) * r$sd.boot[[p]]
        lim.sup[[p]] <- unlist(lapply(1:length(lim.sup1), function(i) if(lim.sup1[i] > 1) lim.sup1[i] = 1 else lim.sup1[i] = lim.sup1[i]))

        lim.inf1 <- cif[[p]]- stats::qnorm((1-level )/2, lower.tail = F ) * r$sd.boot[[p]]
        lim.inf[[p]] <- unlist(lapply(1:length(lim.inf1), function(i) if(lim.inf1[i] < 0) lim.inf1[i] = 0  else lim.inf1[i] = lim.inf1[i]))

      }

      invisible(lapply(1:length(r$cif),
                       function(i){
                         if (i == 1) {
                           graphics::plot(c(min(r$time[order(r$time)]), r$time[order(r$time)]),
                                          c(0, cumsum(unlist(r$cif[i]))), type = "s",
                                          ylim = c(0, 1), xlim = c(min(r$time), max(r$time)),
                                          main = main, xlab = xlab, ylab = ylab)
                         } else {
                           graphics::lines(r$time[order(r$time)], cumsum(unlist(r$cif[[i]])),
                                           type = "s", col = i)
                           if(intervals == T) {
                             for(i in 1:nz){

                               graphics::lines(unique(subset(data.frame(data), data.frame(data)$z == i)$x[order(subset(data.frame(data), data.frame(data)$z == i)$x)]), lim.sup[[i]], type = "s", col = "lightgray")
                               graphics::lines(unique(subset(data.frame(data), data.frame(data)$z == i)$x[order(subset(data.frame(data), data.frame(data)$z == i)$x)]), lim.inf[[i]], type = "s", col = "lightgray")
                             }
                           }
                         }
                       }))
    }
  }
}





#-------------------------------------------------------------------------------
# Ejemplos
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
