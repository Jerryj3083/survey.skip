sm_plot <-
function(data){ # Make the input a data matrix
  
  sm <- data$sm
  sm_cols <- data$col_names
  
  sm_d <- as.data.frame(sm)
  sm_d <- cbind(Rows=1:nrow(sm), sm_d)
  sm_d2 <- pivot_longer(sm_d, cols = sm_cols, names_to = "Columns", values_to = "Type") 
  sm_d2$Columns <- factor(sm_d2$Columns, levels = sm_cols) # ordering columns 
  
  type_colrs <- c(ANS = "#1ca607", N_ANS = "#990522", N_ASK = "#1d07ad", CMPTD = "orange")
  
  vis <- ggplot(sm_d2, aes(x=Columns, y= Rows, fill= Type)) +
    geom_tile() +
    scale_y_reverse() +
    scale_fill_manual(values = type_colrs) +
    theme_minimal()
    
  return(vis)
}
