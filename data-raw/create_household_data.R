{set.seed(1)
  
  #generate data frame
  {
    test4=data.frame(matrix(sample(1:5,2000, replace = T), ncol=20)) #100x20
    # fill answer types
    
    # col 1 name
    test4[,1] = paste0(sample(LETTERS,20, replace = T),".",sample(LETTERS,20, replace = T))
    
    
    # col 4 and 6 pet names
    
    # test4[,4] <- sample(c("Cat", "Dog", "Fish", "Hamster", "Other"), 20, replace=T)
    
    s=sample(1:4, 100, replace=T)
    
    empt_l <- list()
    for (i in 1:100){
      v = sample(c("Cat", "Dog", "Fish", "Hamster"), s[i])
      empt_l[[i]] <- paste0(v, rep(", ", length(v)), collapse = "")
      test4[i, 4] <- substr(empt_l[[i]], 1, nchar(empt_l[[i]])-2)
    }
    test4[,6] <- sample(c("Cat", "Dog", "Fish", "Hamster", "Other"), 20, replace=T)
    test4[,8] <- sample(c("Cat", "Dog", "Fish", "Hamster", "Other"), 20, replace=T)
    test4[,10] <- sample(c("Cat", "Dog", "Fish", "Hamster", "Other"), 20, replace=T)
    test4[,12] <- sample(c("Cat", "Dog", "Fish", "Hamster", "Other"), 20, replace=T)
    test4[,14] <- sample(c("Cat", "Dog", "Fish", "Hamster", "Other"), 20, replace=T)
    
    # col 2 T/F
    
    test4[,2] <- ifelse(test4[,2]==1, FALSE, TRUE)
    test4[,3] <- ifelse(test4[,3]==1, FALSE, TRUE)
    test4[,16] <- ifelse(test4[,16]==1, FALSE, TRUE)
    test4[,17] <- ifelse(test4[,17]==1, FALSE, TRUE)
    test4[,18] <- test4[,18]-1
    
    
    #skip pattern for Q2.1 No
    
    for (i in 1:100){
      if (test4[i,3]==FALSE){
        test4[i,4]=NA
        
      }
    }
    
    #skip pattern for Q2 No and aswering computed question
    for (i in 1:100){
      if (test4[i,2]==FALSE){
        test4[i,5:15]=NA
      }
    }
    
    
    #skip pattern for Q2 Yes
    
    for (i in 1:100){
      if (test4[i,2]==TRUE){
        test4[i,3:4]=NA
      }
    }
    
    
    
    #skip pattern for Q4.1
    for (i in 1:100){
      if (test4[i,18]==0){
        test4[i,19]=NA
        
      }
    }
    
    #Make sure Q5 makes sense
    for (i in 1:100){
      if (test4[i,20]<test4[i,18]){
        test4[i,20]=test4[i,18]
      }
    }
    #skip pattern for Q4 No
    for (i in 1:100){
      if (test4[i,17]==FALSE){
        test4[i,18]=NA
        test4[i,19]=NA
        
      }
    }
    
    #skip pattern for Q2.3 repeat question
    for (i in 1:100){
      if (!is.na(test4[i,5])){
        num_rep = (test4[i,5])
        
        if (num_rep == 1){
          test4[i,8:15]=NA
          
        } else if ( num_rep == 2) {
          test4[i,10:15]=NA
          
        } else if ( num_rep == 3) {
          test4[i,12:15]=NA
          
        } else if ( num_rep == 4) {
          test4[i,14:15]=NA
          
        }
      }}
    
    
    
    set.seed(1)
    m_dim <- dim(test4)
    m_size <- m_dim[1]*m_dim[2] * 0.01
    random_row <- sample(1:m_dim[1], m_size, replace=TRUE)
    random_col <- ceiling(sample(1:m_dim[2], m_size, replace=TRUE)/2)
    
    for (i in 1:length(random_row)){
      test4[random_row[i], random_col[i]] <- NA}
    
    colnames(test4) = c("What is your full name?", 
                        "Do you have a pet?",
                        "Would you consider/want to own a pet?", 
                        "What type of pet would you want?", 
                        "How many pets do you have?", 
                        rep(c("What type of pet do you have?", "How long have you had them for?"),5),
                        "Did your parents have any pets?", 
                        "Does your household have a car?",
                        "How many people in your household can drive?" , 
                        "How many cars are in your household?" , 
                        "How many people are in your household?") 
    
    
}
  household_survey = test4
} # household data


usethis::use_data(household_survey, compress = "xz")

