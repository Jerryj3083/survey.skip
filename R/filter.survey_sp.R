filter.survey_sp <-
function(x, cond){
  rows <- as.numeric(rownames(eval.parent(substitute(subset(x$data, cond)))))
  d <- x$data[rows,]
  m <- x$sm[rows,]
  list(data = d, sm = m)
}
