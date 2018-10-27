#' S3 method to summary a DTDAcif object by using the generic summary function.
#'
#' @title summary.DTDAcif
#' @param object DTDAcif object.
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
summary.DTDAcif <- function(object, ...){

  res <- object
  utils::globalVariables(res)
  nz <- length(unique(res$data$z))

  if (is.null(res$method)) {

    if(is.null(res$sd.boot)){
      df <- data.frame(res$time, res$cif)
      colnames(df) <- c("time", "cif")
      print(df)
    } else {

      df <- data.frame(res$time, res$cif, res$sd.boot)
      colnames(df) <- c("time", "cif", "sd.boot")
      print(df)
     }

  } else {

    if (res$method == "dep") {
      df <- list()
      if(is.null(res$sd.boot)) {

      for(i in 1:length(res$cif)) {

        df[[i]] <-  cbind(data.frame(res$time[i]), data.frame(res$cif[i]))
      }

      names(df) <- paste0("Comp_event_", 1:length(res$cif))

      for(i in 1:length(res$cif)) {
        print(df[i], sep = "\n")
      }

      } else {
        elem <- vector("list", nz)
        for(w in 1:nz) {
          elem[[w]] <- which(unlist(res$cif[w]) != 0)
        }

        boot <- vector("list", nz)
        for(w in 1:nz) {
          boot[[w]] <- data.frame(matrix(0, ncol = 1, nrow = length(res$time[[w]])))
          colnames(boot[[w]]) <- "sd.boot"
        }

        for(w in 1:nz) {
          boot[[w]][unlist(elem[[w]]),] <- res$sd.boot[[w]]
        }

        for(i in 1:length(res$cif)) {
          df[[i]] <-  cbind(data.frame(res$time[i]), data.frame(res$cif[i]), boot[[i]])
        }

        names(df) <- paste0("Comp_event_", 1:length(res$cif))

        for(i in 1:length(res$cif)) {
          print(df[i], sep = "\n")
        }
    }
}

    if (res$method == "indep") {

      df <- list()

      if(is.null(res$sd.boot)){

      for(i in 1:length(res$cif)){

        df[[i]] <-  cbind(res$time, data.frame(res$cif[i]))
      }

      names(df) <- paste0("Comp_event_", 1:length(res$cif))

      for(i in 1:length(res$cif)){
        print(df[i], sep = "\n")
      }

      }else{

        elem <- vector("list", nz)
        for(w in 1:nz){
          elem[[w]] <- which(unlist(res$cif[w]) != 0)
        }

        boot <- vector("list", nz)
        for(w in 1:nz){
          boot[[w]] <- data.frame(matrix(0, ncol = 1, nrow = length(data$z)))
          colnames(boot[[w]]) <- "sd.boot"
        }


          for(w in 1:nz){
          boot[[w]][unlist(elem[[w]]),] <- res$sd.boot[[w]]
          }

        for(i in 1:length(res$cif)){

          df[[i]] <-  cbind(res$time, data.frame(res$cif[i]), boot[[i]])
        }


        names(df) <- paste0("Comp_event_", 1:length(res$cif))

        for(i in 1:length(res$cif)){
          print(df[i], sep = "\n")
        }
      }
    }
  }
}

# summary(res)

