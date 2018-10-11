#' @title Doubly Truncated Data Analysis, Cumulative Incidence Function
#'
#' @description TODO
#'
#' @param x Numeric vector corresponding the variable of ultimate interest.
#' @param u Numeric vector corresponding to the left truncation variable.
#' @param v TODO
#' @param comp.event TODO
#' @param method TODO
#' @param boot TODO
#' @param B TODO
#' @param level TODO
#'
#' @details
#  TODO

#' @return  A list containing:
#'
#' @section Acknowledgements:
#' \itemize{
#' \item{Jacobo de Uña-Álvarez was supported by Grant MTM2014-55966-P, Spanish Ministry of Economy and Competitiveness.}
#' \item{José Carlos Soage was supported by Red Tecnológica de Matemática Industrial (Red TMATI), Cons. de Cultura, Educación e OU, Xunta de Galicia (ED341D R2016/051) and by Grupos de Referencia Competitiva, Consolidación y Estructuración de Unidades de Investigación Competitivas del SUG, Cons. de Cultura, Educación e OU, Xunta de Galicia (GRC ED431C 2016/040).}
#' }
#'
#' @author
#' \itemize{
#' \item{de Uña-Álvarez, Jacobo.}
#' \item{Soage González, José Carlos.}
#' \item{Maintainer: José Carlos Soage González. \email{jsoage@@uvigo.es}}
#' }
#'
#' @references
#' TODO
#'
#' @examples
#'
#'
#' @export
DTDAcif <- function(x, u, v, comp.event, method = c("indep", "dep")
                    #,boot = TRUE, B = 300, level = 0.95
 ) {

  cat("Call:", "\n")
  print(match.call())

  if (any(!is.numeric(c(x, u, v)))) {
    stop("Arguments 'x', 'u' and 'v' must be numeric")
  }

  if (missing(x) && missing(u) && missing(v)){
    stop("Arguments 'x', 'u' and 'v' are missing, with no default")
  }

  if (missing(x) && missing(v)){
    stop("Arguments 'x' and 'v' are missing")
  }

  if(missing(method)){
    method <- "dep"
  }


  z <- comp.event


  if(is.integer(z) & any(z == 0)){
    z <- z + 1
  }

  if(is.character(z)) {
    z <- factor(z)
    z <- as.integer(z)
  }


  # Order w.r.t. x:
  u <- u[order(x)]
  v <- v[order(x)]
  z <- z[order(x)]
  x <- x[order(x)]

  ties <- ifelse(length(x) == length(unique(x)), F, T)

  `shen` <- function(X, U = NA, V = NA, wt = NA, error = NA, nmaxit = NA) {

    D <- cbind(X, U, V)

    ord <- order(D[, 1])
    C <- matrix(0, nrow = nrow(D), ncol = ncol(D))
    EE <- matrix(0, nrow = nrow(D), ncol = ncol(D))
    C[, 1] <- sort(D[, 1])
    C[, 2:ncol(D)] <- D[ord, 2:ncol(D)]

    if(is.na(error) == TRUE){
      error <- 1e-6
    }

    au <- outer(C[, 1], C[, 2], ">=")
    av <- outer(C[, 1], C[, 3], "<=")
    auu <- outer(C[, 2], C[, 2], "<=") * 1L

    J <- matrix(data = 0,
                ncol = nrow(C),
                nrow = nrow(C))
    J <- au * av

    JI <- t(J)
    f0 <- matrix(data = 1 / nrow(C),
                 ncol = 1,
                 nrow = nrow(C))
    f <- f0
    k <- rep(1, times = length(f))
    S0 <- 1
    S1 <- 1

    if(is.na(nmaxit) == TRUE){
      nmaxit <- 100000000000000000
    }

    iter <- 0

    while((S0 > error | S1 > error ) | iter > nmaxit){
      iter <- iter + 1

      if (iter > nmaxit) {
        stop("Default number of iterations not enough for convergence")
      }

      F0 <- JI %*% f
      k0 <- ((sum(1 / F0)) ^ (-1)) * (1 / F0)

      if (sum(k0) != 1) {
        k0 <- k0 / sum(k0)
      }

      K0 <- J %*% k0
      f <- ((sum(1 / K0)) ^ (-1)) * (1 / K0)

      if (sum(f) != 1){
        f <- f / sum(f)
      }

      S0 <- max(abs(f - f0))
      f0 <- f
      S1 <- max(abs(k - k0))
      k <- k0
    }

    F0 <- JI %*% f
    K0 <- J %*% k0
    k <- k0
    mult <- tabulate(match(C[, 1], unique(C[, 1])))

    if (sum(mult) == length(unique(C[, 1]))) {
      Fval <- (f * mult)
    }

    if (sum(mult) > length(unique(C[, 1]))) {
      weigth <- f[!duplicated(C[, 1])]
      Fval <- (weigth * mult)
    }

    x <- unique(C[, 1])
    events <- sum(mult)
    n.event <- mult


    FF <- cumsum(Fval)
    FFF <- cumsum(f)
    Sob <- 1 - FF + Fval
    Sob0 <- 1 - FFF
    Sob[Sob < 1e-12] <- 0
    Sob0[Sob0 < 1e-12] <- 0

    multk <- tabulate(match(C[, 2], unique(C[, 2])))

    if (sum(multk) == length(unique(C[, 2]))) {
      Fvalk <- (k * multk)
    }

    if (sum(multk) > length(unique(C[, 2]))) {
      weigthk <- k[!duplicated(C[, 2])]
      Fvalk <- (weigthk * multk)
    }

    uu <- unique(C[, 2])
    KK <- apply(auu * as.vector(k), 2, "sum")
    kMUV <- cbind(C[, 2], C[, 3], k)

    kuv <- numeric(nrow(C))
    pb <- utils::txtProgressBar(min = 0, max = nrow(kMUV) , style = 3)

    for(i in 1:nrow(kMUV)) {
      utils::setTxtProgressBar(pb, i)
      indbbb <- ((kMUV[, 1] == kMUV[i, 1]) & (kMUV[, 2] == kMUV[i, 2]))
      pos1 <- min(which(indbbb == TRUE))

      if(pos1 == 1) {
        kuv[indbbb] <- sum(k[indbbb])
      }

      if (pos1 > 1) {
        kuv[indbbb] <- sum(k[indbbb])
      }
    }

    Gf <- matrix(data = k, ncol = ncol(J), nrow = nrow(J), byrow = T)
    Gff <- J * Gf
    biasf <- apply(Gff, 1, "sum")

    summary <- cbind("time" = x, "n.event" = mult,
                     "density" = round(as.vector(Fval), 5),
                     "cumulative.df" = round(FF, 5),
                     "survival" = round(Sob, 5))

    colnames(summary) <- c("time", "n.event", "density",
                           "cumulative.df", "survival")
    rownames(summary) <- rep("", times = length(x))

    return(invisible(list(n.iterations = iter, events = events, time = x,
                          n.event = mult, density = as.vector(f),
                          cumulative.df = round(FF, 5),
                          survival = round(as.vector(Sob), 5),
                          truncation.probs = round(as.vector(F0), 5),
                          biasf = biasf,
                          density.joint = round(as.vector(kuv), 5), k = k ,
                          cumulative.dk = round(KK, 5),
                          survival0 = round(as.vector(Sob0), 5))))
  }

  res <- shen(x, u, v)

  if(method == "dep") {

    x1 <- vector("list", length(unique(z)))
    u1 <- vector("list", length(unique(z)))
    v1 <- vector("list", length(unique(z)))
    res1 <- vector("list", length(unique(z)))
    w1 <- vector("list", length(unique(z)))

    for (i in  length(unique(z)):1) {

      x1[i] <- list(x[z == i])
      u1[i] <- list(u[z == i])
      v1[i] <- list(v[z == i])
      res1[i] <- list(shen(unlist(x1[[i]]), unlist(u1[[i]]), unlist(v1[[i]])))
      w1[i] <- list(1 / res1[[i]]$biasf)
    }

    ww1 <- vector("list", length(unique(z)))

    for(i in 1:length(unique(z))){
      ww1[i] <- list(unlist(w1[i]) / sum(sum(unlist(w1))))
    }

    if (ties) {

      r <- list(x = x1, w = ww1)

    plot(unlist(x1[1])[order(unlist(x1[1]))], cumsum(unlist(ww1[[1]])),
         type = "s", ylim = c(0, 1), xlim = c(min(x), max(x)))

      for(i in 2:length(unique(z))){
    lines(unlist(unlist(x1[i]))[order(unlist(unlist(x1[i])))],
          cumsum(unlist(ww1[[i]])), type = "s", col = i)
      }

    } else {

      # time <- data.frame(matrix(ncol = length(res1), nrow = length(res1[[1]]$time)))
      # for(w in 1:length(res1)){
      #   time[, w] <- res1[[w]]$time
      # }
      #
      #
      # r <- list(x = x1, w = ww1)

    plot(unlist(res1[[1]]$time), cumsum(unlist(ww1[[1]])), type = "s",
         ylim = c(0, 1), xlim = c(min(x), max(x)))

      for(i in 2:length(unique(z))){
        lines(unlist(res1[[i]]$time), cumsum(unlist(ww1[[i]])),
              type = "s", col = i)
      }
  }

  } else {

    w <- 1 / res$biasf
    w <- w / sum(w)   # Weights for independence (biased estimator if (U,V) depends on Z)

    w0 <- vector("list", length(unique(z)))

    for(i in 1:length(unique(z))){
      w0[i] <- list(w * ifelse(z == i, 1, 0))
    }

    # w01 <- w * z
    # w02 <- w * (1 - z)

    if(ties) {
     plot(x[order(x)], cumsum(unlist(w0[1])), type = "s", ylim = c(0, 1), xlim = c(min(x), max(x)))

      for(i in 2:length(unique(z))){
        lines(x[order(x)], cumsum(unlist(w0[i])),
              type = "s", col = i)
      }


     } else {

    plot(res$time, cumsum(unlist(w0[1])), type = "s", ylim = c(0, 1), xlim = c(min(x), max(x)))
       for(i in 2:length(unique(z))){
         lines(x[order(x)], cumsum(unlist(w0[i])),
               type = "s", col = i)
       }
  }
}


  # True cifs:

  # s <- seq(0, max(x), length = 100)
  # lines(s, r1 * (1 - exp(-r * s)) / r, type = "l", lty = 2)
  # lines(s, r2 * (1 - exp(-r * s)) / r, type = "l", col = 2, lty = 2)

  # case of ties:
  # we cannot do as above for the plots since res$biasf is a vector with dim = n
  # while res$time has dim = length(unique(x))
  # the following will work (take care! res$biasf corresponds to x[order(x)],
  # this is important unless x is ordered from the very beginning):



  # x <- switch(method, indep = ifelse(ties, x[order(x)], res$time),
  #             dep = ifelse(ties, unlist(x1[1])[order(unlist(x1[1]))], unlist(res1[[1]]$time)))
  #
  # w <- switch(method, indep = data.frame(matrix(cumsum(unlist(w0)), byrow=T)),
  # dep = data.frame(matrix(cumsum(unlist(ww1)), byrow=T)))



  # if(method == "dep"){
  # r <- list(x = x1, w = ww1, time = res1)
  #
  # } else{
  #   r <- list(x = x, w = w0, time = res)
  #
  # }





  # class(r) <- c('list', 'DTDAcif')
  # r

}


#-------------------------------------------------------------------------------
# Ejemplo #
#-------------------------------------------------------------------------------

set.seed(06062018)

n <- 5000

r1 <- 1
r2 <- 3
r <- r1 + r2
x <- rexp(n, rate = r)
z <- rbinom(n, 2, r1 / r)
u <- 0.75 * rbeta(n, 1 + z, 1) - 0.25    #runif(n,-.25,.5)
v <- u + 0.75

for (i in 1:n) {
  while (u[i] > x[i] | v[i] < x[i]) {
    x[i] <- rexp(1, rate = r)
    z[i] <- rbinom(1, 1, r1 / r)
    u[i] <- 0.75 * rbeta(1, 1 + z, 1) - 0.25     #runif(1,-.25,.5)
    v[i] <- u[i] + 0.75
  }
}


# x <- round(x, 1)
# u <- pmin(round(u, 1), x)
# v <- pmax(round(v, 1), x)

res <- DTDAcif(x, u, v, comp.event = z, method = "dep")
# DTDAcif(x, u, v, comp.event = z, method = "indep")
