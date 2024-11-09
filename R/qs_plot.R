qs_plot <-
function(x, y, data){
  d <- select(data, c(x,y))
  
  d2 <- d$data
  new_m <- d$sm
  # add column for tyoe
  new_d <- cbind(d2, Type=NA)
  
  for (j in 1:2){
    if (!is.numeric(new_d[[j]])){
      new_d[[j]] <- as.character(new_d[[j]])
    }
  }
  
  
  
  for (i in 1:nrow(new_d)){
    
    if (all(new_m[i, ] == "ANS") == TRUE) { # Both are answered
      new_d[i,3] <- "ANS"
    } else if (new_m[i,1] == "ANS"){ # Only X is answered
      new_d[i,3] <- new_m[i,2]
      new_d[i, 2] <- -0.5
    } else if (new_m[i,2] == "ANS"){ # Only Y is answered
      new_d[i,3] <- new_m[i,1]
      new_d[i, 1] <- -0.5
      
      # Thr remaining have no answered
    } else if (any(new_m[i, ] == "CMPTD")){ # One is a not answered
      new_d[i,3] <- "CMPTD"
      new_d[i, 1] <- -0.5
      new_d[i, 2] <- -0.5
    } else if (any(new_m[i, ] == "N_ASK")){ # One is a not answered
      new_d[i,3] <- "N_ASK"
      new_d[i, 1] <- -0.5
      new_d[i, 2] <- -0.5
    } else {
      new_d[i,3] <- "N_ANS"
      new_d[i, 1] <- -0.5
      new_d[i, 2] <- -0.5
    }
  }
  
  for (k in 1:2){
    if (!is.numeric(new_d[[k]]) & any(new_d[[k]]=="-0.5")){
      lvls <- levels(factor(new_d[[k]]))
      new_d[[k]] <- factor(new_d[[k]], levels = c("NA", lvls[-1]), ordered = TRUE)
    }
  }
  
  if (!is.numeric(d2[[1]]) & !is.numeric(d2[[2]])){ # two categorical
    vis <- ggplot(new_d, aes(x = new_d[,1], y = new_d[,2], colour=Type)) +
      geom_jitter(width = 0.1, height = 0.1) +
      scale_x_discrete(labels=levels(new_d[,1])) +
      scale_y_discrete(labels=levels(new_d[,2])) +
      labs(x=x,y=y, title = paste(x, "vs", y)) +
      geom_hline(yintercept =  0) +
      geom_vline(xintercept = 0)
    
  } else if (!is.numeric(d2[[1]]) & is.numeric(d2[[2]])){ # y numeric x categorical
    
    vis <- ggplot(new_d, aes(x = new_d[,1], y = new_d[,2], colour=Type)) +
      geom_jitter(width = 0.1, height = 0.1) +
      labs(x=x,y=y, title = paste(x, "vs", y))+
      scale_x_discrete(labels=levels(new_d[,1])) +
      geom_hline(yintercept =  0) 

  } else if (is.numeric(d2[[1]]) & !is.numeric(d2[[2]])){ # x numeric y categorical
    
    vis <- ggplot(new_d, aes(x = new_d[,1], y = new_d[,2], colour=Type)) +
      geom_jitter(width = 0.1, height = 0.1) +
      labs(x=x,y=y, title = paste(x, "vs", y))+
      scale_y_discrete(labels=levels(new_d[,2])) +
      geom_vline(xintercept = 0)
    
  } else { # both numeric
    vis <- ggplot(new_d, aes(x = new_d[,1], y = new_d[,2], colour=Type)) +
      geom_jitter(width = 0.1, height = 0.1) +
      labs(x=x,y=y, title = paste(x, "vs", y)) +
      geom_hline(yintercept =  0) +
      geom_vline(xintercept = 0)
  }
  return(vis)
  
  }
