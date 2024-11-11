data_sm <-
function(test4, skip_logic2){
  if (!is.data.frame(test4) | !is.list(skip_logic2))
    stop("Invalid input parameters")
  
  ############################## Information from the JSON file ##########################
  {
    # Survey questions
    rep_qs_max <- lapply(skip_logic2$SurveyQuestions, function(x) x$MaxRepeats) # Max repeats 
    rep_qs_repeat <- lapply(skip_logic2$SurveyQuestions, function(x) x$QuestionsToRepeat) # Questions to repeat
    all_Qnames <- unlist(lapply(skip_logic2$SurveyQuestions, function(x) x$Question)) # All question names
    all_Qnums <- unname(unlist(lapply(skip_logic2$SurveyQuestions, function(x) x$QuestionNumber))) # All question numbers
    all_Qtypes <- unlist(lapply(skip_logic2$SurveyQuestions, function(x) x$QuestionType)) # All question types
    all_next_questions <- lapply(skip_logic2$SurveyQuestions, function(x) x$NextQuestion) # All possible next questions
    all_cols <- unlist(lapply(skip_logic2$SurveyQuestions, function(x) x$ColumnNumber)) # All column numbers
    # Computed questions
    computed_col <- unlist(lapply(skip_logic2$ComputedQuestions, function(x) x$ColumnNumber)) # Computed question columns
    computed_from <- unlist(lapply(skip_logic2$ComputedQuestions, function(x) x$ComputedFrom)) # Questions theyre computed from 
    computed_Qnums <- unlist(lapply(skip_logic2$ComputedQuestions, function(x) x$QuestionNumber)) # Computed question number
    computed_vals <- lapply(skip_logic2$ComputedQuestions, function(x) x$Computation) # Computed question number 
    computed_type <- unlist(lapply(skip_logic2$ComputedQuestions, function(x) x$ComputeType)) # Computed type
    rep_cond <- lapply(skip_logic2$ComputedQuestions, function(x) x$RepCond) # Condition for rep
  }
  
  
  
  ############################## Getting all column numbers for matrix #############################
  
  sm_cols <- numeric() # Empty vector to store the shadow matrix column names/numbers
  sm_cols2 <- numeric() # stores the questions names
  
  for (i in 1:length(all_Qnums)) { # Iterate through all question numbers
    tp <- all_Qtypes[which(names(all_Qtypes)==all_Qnums[i])] # the question type
    if (tp == "Repeat") { # We will do some extra steps for repeat questions since they have multiple cols
      # We want to duplicate repeat questions the maximum amount of times
      
      rep_add <- unname(rep(unlist(rep_qs_repeat[i]), unlist(rep_qs_max[i])-1))
      rep_names <- paste0(rep_add, "-", rep(1:(unlist(rep_qs_max[i])), each=length(rep_qs_repeat[[i]]))) # combining hyphen with number
      add <- c(all_Qnums[i], rep_names)
      add2 <- c(all_Qnums[i], rep_add)
      
    } else { # Otherwise just add the normal question number
      add <- all_Qnums[i]
      add2 <- all_Qnums[i]
    }
    sm_cols <- c(sm_cols, add) # append the question number to the vector
    sm_cols2 <- c(sm_cols2, add2)
    #print(add)
  }
  sm_cols <- sm_cols[! sm_cols %in% unlist(rep_qs_repeat)]
  
  
  
  
  
  
  ############################################ Shadow matrix ###############################################
  
  
  colnames(test4) <- sm_cols
  sm <- matrix(nrow = dim(test4)[1], ncol=dim(test4)[2]) # Creating the shadow matrix
  colnames(sm) <- sm_cols
  list_of_skip_reason1 <- list() # Empty vector to store reasons for skip
  list_of_cols2 <- list()
  list_of_qs2 <- list()
  
  ####################### Iterating through each row ###################
  
  for (i in 1:nrow(test4)){
    list_of_qs <- all_Qnums[1] # Vector for all questions visited 
    list_of_skip <- numeric() # Vector for the questions skipped
    list_of_cols <- numeric() # Vector for all columns visited
    
    df_skip_reason1 <- data.frame() # Data frame to store the possible questions skipped and their reason
    next_q <- all_Qnums[1] # Start with the first question
    
    ####################### Iterating through all question numbers/columns #########################
    
    for (j in 1:length(all_Qnums)){
      mcol <- all_cols[which(names(all_cols) == all_Qnums[j])] # The column this question is in
      tp <- all_Qtypes[which(names(all_Qtypes)==all_Qnums[j])] # Question type
      
      # skip iteration if next_q is not corresponding next question 
      if (next_q != all_Qnums[j]) { 
        list_of_skip <- append(list_of_skip, all_Qnums[j]) # Add question to the skipped questions vector
        next 
      }
      
      if (tp == "Standard"){ 
        # Use the current question to get find the index of the next question
        next_q <- all_next_questions[[which(names(all_next_questions)==all_Qnums[j])]] 
        current_col <- unname(mcol) # current question column
        
        
      } else if (is.na(test4[i,mcol]) & tp == "Repeat"){ # If value is NA and a repeat question
        replen <- length(rep_qs_repeat[[j]]) # Number of questions to repeat
        max_len <- replen * unname(rep_qs_max[[j]]) # Max length of rep q
        
        which(!is.na(test4[i,(mcol+1):(mcol+max_len)])) # Shows which of the qs are answered
        
        if (any(!is.na(test4[i,(mcol+1):(mcol+max_len)])) == FALSE) { # if there are no values being repeated
          next_q <- all_next_questions[[which(names(all_next_questions)==last)]]
          current_col <- unname(mcol)
        } else {
          
          last_rep <- max(which(!is.na(test4[i,(mcol+1):(mcol+max_len)]))) # last q
          est_reps <- ceiling(solve(replen,last_rep)) # estimated repeats
          rep_cols <- replen * est_reps # number of cols this q takes
          current_col <- mcol:(mcol+rep_cols)
          
          for (k in 1:est_reps){ # iterate through number of repeats wanted
            # Iterate through the questions to repeat, minus the last one since its next question is the actual next question
            for (m in c(all_Qnums[j],rep_qs_repeat[[j]][-length(rep_qs_repeat[[j]])])){ 
              next_q <- all_next_questions[[which(names(all_next_questions)==m)]]
              list_of_qs <- append(list_of_qs, next_q) # vector of questions visited
            }
          }
          last <- rep_qs_repeat[[j]][length(rep_qs_repeat[[j]])] # Last question in rep_qs_repeat
          next_q <- all_next_questions[[which(names(all_next_questions)==last)]] # Use the next question of the last question
        }  
        
      } else if (is.na(test4[i,mcol])){# If the entry is just NA
        all_opts <- all_next_questions[[which(names(all_next_questions)==all_Qnums[j])]] # all next_q options
        
        test_val <- test4[i, match(all_opts, sm_cols)] # The answered values of those options
        s_check <- all_next_questions[[which(names(all_next_questions)==names(test_val)[1])]] # next_q for sanity check
        s_val <- test4[i, match(s_check, sm_cols)]
        any_in <- any(s_check %in% names(test_val)) # checks if one of the test_val qs are in s_check
        
        if (!is.na(test_val[1])) { # if the first next_qs is answered then value goes to first
          next_q <- names(test_val[1])
          
          # Rest of these options mean first options was NA
        } else if (any_in & length(s_check)==1) { # If only one s_check when any_in is true
          next_q <- names(test_val[2])
        } else if (all(!is.na(s_val))){ # All sanity check options answered
          next_q <- names(test_val[1])
          # } else if (any_in & all(!is.na(s_val))) { # any_in and all s_checks answered
          #   next_q <- names(test_val[1])
        } else if (any(is.na(s_val))) { # if one s_check value is NA
          next_q <- names(test_val[2])
          #  } else if (!is.na(test_val[2])) {
          #   next_q <- names(test_val[2])
        }
        
        current_col <- unname(mcol)
        
      } else if (tp == "Cut-point"){
        # all cut-point options for this question
        cp_opts <- unlist(unname(all_next_questions[which(names(all_next_questions)==all_Qnums[j])]))
        # evaluation cut-point
        cut_opts2 <- numeric()
        
        for (o in 1:length(names(cp_opts))) {
          nm <- names(cp_opts)[o]
          if (length(grep("&", nm)) == 1){ # if there is an &
            #  all_pos <- unlist(gregexpr("&", nm)) # index of the &
            cut_opts2[o] <- gsub("&", paste("&", test4[i,mcol]), nm)
          }
          else # else if the is just one cut-point
            cut_opts2[o] <- names(cp_opts)[o]
        }
        
        cut_eval <- paste(test4[i,mcol],cut_opts2) # paste full evaluation of current question answer and cut-point
        final_eval <- unlist(lapply(cut_eval, function(x) eval(parse(text= x)))) # Evaluate the expressions
        next_q <- unname(cp_opts[which(final_eval==TRUE)]) # Select the next_q that is true as the next question
        current_col <- unname(mcol) # current question column
        
      } else if (tp == "List"){
        # All possible options
        ls_opts <- unlist(unname(all_next_questions[which(names(all_next_questions)==all_Qnums[j])]))
        ans <- test4[i, mcol]
        if (length(grep(",", ans))==1){
          ans <- unlist(strsplit(ans, ",")) # splits the multi answers into vector
        }
        
        for (l in ans){
          if (l %in% names(ls_opts)){
            next_q <- unname(ls_opts[which(names(ls_opts)==l)])
          }
          
        }
        
        
        
        # next_q <- unname(ls_opts[which(names(ls_opts)==test4[i, mcol])]) # see which option is equal to the real answer
        current_col <- unname(mcol) # current question column
        
      } 
      else if (tp == "Repeat"){
        
        all_Qnums[j] # question number eg Q2.3
        test4[i,mcol] # the number of times we want to repeat the given questions
        rep_qs_repeat[[j]] # the questions we want to repeat
        unname(rep_qs_max[[j]]) # the maximum times we can repeat
        
        # Make sure the number of times to repeat is not over the max
        num_rep <- ifelse(test4[i,mcol] > unname(rep_qs_max[[j]]), unname(rep_qs_max[[j]]), test4[i,mcol]) 
        # Number of columns this repeat section takes
        rep_cols <- num_rep * length(rep_qs_repeat[[j]]) # Multiply number of repeat by the amount of questions to repeat
        current_col <- mcol:(mcol+rep_cols) # current question column
        
        for (k in 1:test4[i,mcol]){ # iterate through number of repeats wanted
          # Iterate through the questions to repeat, minus the last one since its next question is the actual next question
          for (m in c(all_Qnums[j],rep_qs_repeat[[j]][-length(rep_qs_repeat[[j]])])){ 
            next_q <- all_next_questions[[which(names(all_next_questions)==m)]]
            list_of_qs <- append(list_of_qs, next_q) # vector of questions visited
          }
        }
        last <- rep_qs_repeat[[j]][length(rep_qs_repeat[[j]])] # Last question in rep_qs_repeat
        next_q <- all_next_questions[[which(names(all_next_questions)==last)]] # Use the next question of the last question
        
      }
      ############################# End of types ###########################
      
      list_of_qs <- append(list_of_qs, next_q) # append the next question to the list of questions visited
      list_of_cols <- append(list_of_cols, current_col) # Append the current column to the vector
      
      all_next <- unlist(all_next_questions[names(all_next_questions)[j]]) # All possible next questions
      not_next <- all_next[!all_next == next_q] # All questions that will be skipped
      if (length(not_next) ==0){  # if there are no questions paths skipped use NA
        not_next = NA
      }
      #   not_next <- ifelse(length(not_next) ==0 , NA, not_next) # if there are no questions paths skipped use NA
      
      # df of current question and all questions that will be skipped because of it
      reason1 <- data.frame(current_q = as.character(all_Qnums[j]), not_next = not_next) 
      #print(paste(all_Qnums[j], not_next))
      df_skip_reason1 <- rbind(df_skip_reason1, reason1) # rbind all reason data frames
    }
    ############### End of a row iteration ####################
    
    skipped_col <- setdiff(1:ncol(test4), list_of_cols) # columns of skipped questions
    sm[i,skipped_col] = "N_ASK" # input all not asked 
    sm[i, list_of_cols] = "ANS"
    
    data_na <- which(is.na(test4[i,])) # the NAs from data
    
    # The NA that are visited are not answer since they cant be from skipped
    sm[i, intersect(list_of_cols,data_na)] = "N_ANS" # finds which NA from data is not part of skip
    
    
    
    list_of_skip_reason1[[i]] <- df_skip_reason1
    list_of_qs2[[i]] <- list_of_qs[-length(list_of_qs)]
    list_of_cols2[[i]] <- list_of_cols
    
    
  } 
  ########################################## End of Shadow matrix###########################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################### Filling computed values ################################
  
  
  for (m in 1:nrow(sm)){ # iterate through each row
    for (i in 1:length(computed_Qnums)){ # Going through all computed questions
      
      comp_tp <- computed_type[i] # type of computed question
      comp_from <- computed_from[i] # Origin or question to compute from
      org_col <- which(sm_cols2 == names(comp_from))
      from_col <- which(sm_cols2 == comp_from) # Column of comp_from
      val <- test4[m,from_col]
      comp <- computed_vals[[i]]
      
      if (!(is.na(test4[m,org_col]))){ # if the value is already answered / not NA ,  we skip iteration
        next 
      } 
      
      # If we are computing from a repeated question
      if (comp_tp == "Repeat"){
        
        if (all(is.na(val))){ # If all the values we need are NA
          sm[m,computed_col[i]] = "N_ASK" 
          next
        }
        
        rep_cond2 <- rep_cond[[i]] # the condition used for computing repeat (all or any)
        val <- val[!is.na(val)] # removes all NA
        if (rep_cond2 == "all"){ # if the condition is all
          rep_eval <- all(val == names(comp)[1]) # check if all values
          fin <- names(comp)[which(comp==rep_eval)] # final computed value
          sm[m,computed_col[i]] = "CMPTD"
          test4[m,computed_col[i]] = fin
          
        } else if (rep_cond2 == "any") { # if the condition is any
          rep_eval <- any(val == names(comp)[1])
          fin <- names(comp)[which(comp==rep_eval)] # final computed values
          sm[m,computed_col[i]] = "CMPTD"
          test4[m,computed_col[i]] = fin
        }
        
        
        
        
        
      } else if (is.na(val)) { # if the value we need is NA, check if its missing or not answered
        
        all_opts <- all_next_questions[[which(names(all_next_questions) == comp_from)]] # options of the comp_from
        
        test_val <- test4[m, match(all_opts, sm_cols)] # The answered values of those options
        s_check <- all_next_questions[[which(names(all_next_questions)==names(test_val)[1])]] # next_q for sanity check
        s_check_val <- test4[m, match(s_check, sm_cols)]
        any_in <- any(s_check %in% names(test_val)) # checks if one of the test_val qs are in s_check
        
        if (!is.na(test_val[1])) { # if the first next_qs is answered then value goes to first
          sm[m,computed_col[i]] = "N_ANS"
        } else if (any_in == TRUE & all(!is.na(s_check_val)) == TRUE) { # any_in true and all s_checks are not missing
          sm[m,computed_col[i]] = "N_ANS"
        } else if (any_in == FALSE & any(!is.na(s_check_val)) == TRUE ) { # if just one s_check is answered when any_in false
          sm[m,computed_col[i]] = "N_ANS"
        } else if (!is.na(test_val[2]) == TRUE) {
          sm[m,computed_col[i]] = "N_ANS"
        } else {
          sm[m,computed_col[i]] = "N_ASK"
        }
        next
      }
      
      else if (comp_tp=="List") {
        final_comp_val <- comp[which(names(comp) == val)] # Find which computed value to use
        if (final_comp_val != "Skip") {
          sm[m,computed_col[i]] = "CMPTD"
          test4[m,computed_col[i]] = final_comp_val
        } 
        
        
        
      } else if (comp_tp == "Cut-point") {
        cut_eval <- paste(val,names(comp))
        final_eval <- unlist(lapply(cut_eval, function(x) eval(parse(text= x))))
        final_comp_val <- comp[which(final_eval)]
        if (final_comp_val != "Skip") {
          sm[m,computed_col[i]] = "CMPTD"
          test4[m,computed_col[i]] = final_comp_val
        }
        
      }
    }
    
  }
  
  
  
  survey_obj <- list(data = test4, sm = sm, questions = list_of_qs2, columns = list_of_cols2, not_next = list_of_skip_reason1, col_names = sm_cols)
  class(survey_obj) <- "survey_sp"
  survey_obj
  
  
}
