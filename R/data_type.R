data_type <-
function(row, question, data){
  sm_cols <- data$col_names
  d <- data$data
  if (!(row %in% 1:nrow(d)) || !(question %in% sm_cols))
    stop("Invalid input")
  sm <- data$sm
  column <- which(sm_cols == question)
  sm[row, column]
}
