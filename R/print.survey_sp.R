
print.survey_sp <- function(x, n=5){
  cat("data:\n")
  print(head(x$data, n))
  
  cat("shadow matrix:\n")
  print((head(x$sm, n)))
  
  
}