{
  set.seed(1)
  sport_data <- data.frame(matrix(sample(1:8,1700, replace=TRUE), ncol = 17)) 
  # colnames
  colnames(sport_data)= c("What is your age?" ,"Do you play any sports?",
                          "How tall are you?" ,"Do you play any sport competitively?",
                          "How many times a week do you play?" ,"how long do you play during each session?",
                          "How much is the fee per session?" ,"Do you play any sport for fun?" ,"How many times a week do you play?",
                          "how long do you play during each session?","How much is the fee per session?" 
                          ,"Do you watch any sports?","How/where do you watch sports from?" 
                          ,"Do you pay to enter or is it free?","Do you pay for a subscription or is it free?" 
                          ,"What platform do you watch on?" ,"How much money do you pay for a game?")
  
  
  sport_data[,1] <- sample(16:30, 100, replace=TRUE)
  sport_data[,2] <- ifelse(sport_data[,2]==1, "No", "Yes")
  sport_data[,3] <- sample(150:220, 100, replace=TRUE)
  sport_data[,4] <- ifelse(sport_data[,4]>=4, "Yes", "No")
  sport_data[,5] <- sample(1:7, 100, replace=TRUE)
  sport_data[,6] <- sample(1:5, 100, replace=TRUE)
  sport_data[,7] <- sample(0:50, 100, replace=TRUE)
  sport_data[,8] <- ifelse(sport_data[,8]>5, "No", "Yes")
  sport_data[,9] <- sample(1:7, 100, replace=TRUE)
  sport_data[,10] <- sample(1:6, 100, replace=TRUE)
  sport_data[,11] <- sample(0:50, 100, replace=TRUE)
  sport_data[,12] <- ifelse(sport_data[,12] > 2, "Yes", "No")
  
  v <- c("In-person", "TV", "Online")
  v2 <- c("Pay", "Free")
  
  sport_data[,13] <- sample(v, 100, replace=TRUE)
  sport_data[,14] <- sample(v2, 100, replace=TRUE)
  sport_data[,15] <- sample(v2, 100, replace=TRUE)
  
  v3 <- c("Youtube", "Facebook", "Instagram", "Twitter", "Tiktok", "News")
  
  sport_data[,16] <- sample(v3, 100, replace=TRUE)
  sport_data[,17] <- sample(1:200, 100, replace=TRUE)
  
  
  # skip depending on Q5.1 answer
  for (i in 1:100){ # for Q3.1 skip if less than 3
    if (sport_data[i,13] == "In-person"){
      sport_data[i,15:16] = NA
    } else if (sport_data[i,13] == "TV"){
      sport_data[i,c(14,16)] = NA
    } else if (sport_data[i,13] == "Online") {
      sport_data[i,14:15] = NA
    }
  }
  
  for (i in 1:100){ # for Q5 skip and finish if no
    if (all(is.na(sport_data[i,14:15])))
      sport_data[i,17] = NA
  } 
  
  for (i in 1:100){ # for Q5 skip and finish if no
    if (sport_data[i,12] =="No")
      sport_data[i,13:17] = NA
  }  
  
  
  # Q4
  
  for (i in 1:100){ # for Q4.2 skip if less than or equal to 1
    if (sport_data[i,10] <= 1)
      sport_data[i,11] = NA
  } 
  
  for (i in 1:100){ # for Q4.1 skip if less than 3
    if (sport_data[i,9] < 3)
      sport_data[i,10:11] = NA
  } 
  
  for (i in 1:100){ # for Q4 skip if no
    if (sport_data[i,8] =="No")
      sport_data[i,9:11] = NA
  } 
  
  
  #Q3  
  
  for (i in 1:100){ # for Q3.2 skip if less than or equal to 1
    if (sport_data[i,6] <= 1)
      sport_data[i,7] = NA
  }  
  
  for (i in 1:100){ # for Q3.1 skip if less than 3
    if (sport_data[i,5] < 3)
      sport_data[i,6:7] = NA
  } 
  
  for (i in 1:100){ # for Q3 skip if no
    if (sport_data[i,4] == "No")
      sport_data[i,5:7] = NA
  } 
  
  
  #Q2  
  
  for (i in 1:100){ # for Q2.1 skip if between 160 and 180
    if (sport_data[i,3] <= 160){
      sport_data[i,4:11] = NA
    } else if (sport_data[i,3] < 180){
      sport_data[i,4:7] = NA
    }
    
  } 
  
  
  for (i in 1:100){ # for Q2 skip if no
    if (sport_data[i,2] == "No")
      sport_data[i,3:11] = NA
  }
  
  set.seed(1)
  m_dim <- dim(sport_data)
  m_size <- m_dim[1]*m_dim[2] * 0.01
  random_row <- sample(1:m_dim[1], m_size, replace=TRUE)
  random_col <- ceiling(sample(1:m_dim[2], m_size, replace=TRUE)/2)
  
  for (i in 1:length(random_row)){
    sport_data[random_row[i], random_col[i]] <- NA}
  
  
  
}  # sports data

sport_survey = sport_data

usethis::use_data(sport_survey, compress = "xz")
