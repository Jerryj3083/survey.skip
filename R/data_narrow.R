data_narrow <-
function(x, verbose = FALSE){
  
  sm <- x$sm
  sm <- tst$sm
  
  cols_rm <- numeric()
  qs_rm <- numeric()
  
  for (i in 1:ncol(sm)){
    if (all(sm[,i] =="N_ASK")) {
      
      cols_rm <- c(cols_rm,i) # columns to removei
      qs_rm <- c(qs_rm, sm_cols[i]) # Questions removed
    }
  }
  if (verbose == TRUE) {
    ls <- list(data = x$data[,-(cols_rm)], sm = sm[,-(cols_rm)], questions_removed = qs_rm)
  } else {
    ls <- list(data = x$data[,-(cols_rm)], sm = sm[,-(cols_rm)])
  }
 ls
}
