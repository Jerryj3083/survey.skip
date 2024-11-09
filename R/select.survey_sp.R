select.survey_sp <-
function(x, vals){
  d <- select(x$data, vals)
  m <- as.matrix(select(as.data.frame(x$sm), vals))
  list(data = d, sm = m)
}
