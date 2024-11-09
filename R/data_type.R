data_type <-
function(row, question, data){
  sm_cols <- data$col_names
  if (!(row %in% 1:nrow(test_A)) || !(question %in% sm_cols))
    stop("Invalid input")
  sm <- data$sm
  column <- which(sm_cols == question)
  sm[row, column]
}
