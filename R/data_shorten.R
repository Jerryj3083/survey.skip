data_shorten <-
function(x, verbose = FALSE) {
  sm <- x$sm
  sm <- tst2$sm
  rows_rm <- numeric() # Vector stores the rows removed
  
  for (i in 1:nrow(sm)){
    if (all(sm[i,] == "N_ASK") == TRUE){
      rows_rm <- c(rows_rm, i) # Add to vector
    }
  }
  if (verbose == TRUE){
    ls = list(data = x$data[-(rows_rm),], sm = x$sm[-(rows_rm),], rows_removed = rows_rm)
  } else {
    ls = list(data = x$data[-(rows_rm),], sm = x$sm[-(rows_rm),])
  }
  ls
}
