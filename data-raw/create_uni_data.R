{
  set.seed(1)
  uni_data <- data.frame(matrix(sample(1:5,3000, replace=TRUE), ncol = 30)) 
  # colnames
  
  colnames(uni_data)= c("Do you study at the University of Auckland?", "How many courses are you taking this semester?", 
                        rep(c("Do you enjoy the course?", "Do you like your lecturer?", "Are you coping with the course?"),6),
                        "Do you live on/near campus?","How long does it take to commute?" ,"Do you enjoy all your classes?","Do you dislike any lecturer?",
                        "How many times a week do you come to campus?" ,rep("How long do you stay on campus for?",5))
  
  
  
  
  
  
  
  
  ans <- c(rep("Yes",5), "No", "Somewhat")
  ans2 <- c("Yes", rep("No", 5), "Somewhat")
  
  for (i in 1:100){
    uni_data[i,3:20] <- sample(ans, 18, replace=T)
  }
  
  for (i in 1:100){
    uni_data[i,c(4, 7, 10, 13, 16 ,19)] <- sample(ans2, 6, replace=T)
  }
  
  
  for (i in 1:100){
    amount = uni_data[i,2] * 3
    uni_data[i,(3+amount):20] = NA
    
  }
  
  
  uni_data[,21] <- ifelse(uni_data[,21] > 2, "Yes", "No") # Q3
  uni_data[,22] <- sample(1:120, 100, replace=T) # Q3.1
  for (i in 1:100){ # Q3.2 and 3.3
    uni_data[i,23] <- ifelse(all(uni_data[i,c(3, 6, 9, 12, 15 ,18)]=="Yes"), "Yes", "No")
    uni_data[i,24] <- ifelse(any(uni_data[i,c(4, 7, 10, 13, 16 ,19)]=="Yes"), "Yes", "No")
  }
  
  for (i in 1:100){
    uni_data[ which(is.na(uni_data[,23])),23] <- "Yes"
  }
  for (i in 1:100){
    uni_data[ which(is.na(uni_data[,24])),24] <- "No"
  }
  
  uni_data[,1] <- ifelse(uni_data[,1] ==1, "No", "Yes") # Q1
  uni_data[,2] <- ifelse(uni_data[,1] =="No", NA, uni_data[,2]) # Q2
  
  uni_data[,26:30] <- sample(c(rep(1:6,5),7:12), 4, replace=T)
  
  
  for (i in 1:100){
    if (uni_data[i, 25]==1){
      uni_data[i,27:30]=NA
    } else if (uni_data[i, 25]==2){
      uni_data[i,28:30]=NA
    } else if (uni_data[i, 25]==3){
      uni_data[i,29:30]=NA
    } else if (uni_data[i, 25]==4){
      uni_data[i,30]=NA  
    }
    
  }
  
  for (i in 1:100) {
    if (uni_data[i,1] == "No")
      
      uni_data[i,2:24]=NA
    
    for (i in 1:100){
      if (!is.na(uni_data[i,21])){
        if (uni_data[i,21] == "Yes")
          uni_data[i,22:24] = NA
        
      } else uni_data[i,22:24] = NA
    }
    
  }
} # uni data
uni_survey = uni_data

usethis::use_data(uni_survey, compress = "xz")

