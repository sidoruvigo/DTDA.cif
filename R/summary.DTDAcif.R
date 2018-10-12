summary.DTDAcif <- function(x){


  res <- x

  if (is.null(res$method)) {
    df <- data.frame(res$time, res$cif)
    colnames(df) <- c("time", "cif")
    print(df)


  } else {

    if (res$method == "dep") {

      df <- list()

      for(i in 1:length(res$cif)){

        df[[i]] <-  cbind(data.frame(res$time[i]), data.frame(res$cif[i]))
      }

      names(df) <- paste0("Comp_event_", 1:length(res$cif))

      for(i in 1:length(res$cif)){
        print(df[i], sep = "\n")
      }
}

    if (res$method == "indep") {

      df <- list()

      for(i in 1:length(res$cif)){

        df[[i]] <-  cbind(res$time, data.frame(res$cif[i]))
      }

      names(df) <- paste0("Comp_event_", 1:length(res$cif))

      for(i in 1:length(res$cif)){
        print(df[i], sep = "\n")
      }

    }
  }


 #  # Empty lists
 #  data_time <- list()
 #  data_cif <- list()
 #
 #  for(i in 1:length(res$time)){
 #
 #    data[[i]] <-  cbind(x$time[i])
 #  }
 #
 #  # Turn list into a dataframe
 #  df <- data.frame(data_time)
 #
 #  for(j in 1:length(res$cif)){
 #
 #    data_cif[[j]] <-  cbind(x$cif[j])
 #  }
 #
 #
 #  # turn your list into a dataframe
 #  df <- data.frame(mydata)
 #
 # print(df)

}

summary(res)

