reason_for_skip <-
function(row, question, data){
  sm_cols <- data$col_names
  column <- which(sm_cols == question)
  if (!is.numeric(row) || !is.numeric(column) || is.na(row) || is.na(column) || class(data)!="survey_sp")
    stop("Invalid input")
  if (data_type(row, question, data) != "N_ASK") {
    skp_reason <- NA
    class(skp_reason) <- "Skip_reason"
    skp_reason
  } else {
    
    
    
    
    ################################## Checking why question was not asked ##########################
    list_of_skip_reason1 <- data$not_next
    list_of_skip_reason2 <- list()
    
    #1:nrow(test4)
    for (i in 1:nrow(test4)){
      list_of_qs <- all_Qnums[1] # add first question into list
      list_of_skip <- numeric()
      list_of_cols <- numeric() # add first column into the list
      
      df_skip_reason2 <- data.frame()
      
      next_q <- all_Qnums[1]
      
      for (j in 1:length(all_Qnums)){ # iterate through all question numbers
        if (next_q != all_Qnums[j]) {
          list_of_skip <- append(list_of_skip, all_Qnums[j])
          next # skip iteration if next_q is not corresponding question number
        }
        mcol <- all_cols[which(names(all_cols) == all_Qnums[j])] # what column this question is
        tp <- all_Qtypes[which(names(all_Qtypes)==all_Qnums[j])] # the type of question
        
        if (tp == "Standard"){ 
          # next question
          next_q <- all_next_questions[[which(names(all_next_questions)==all_Qnums[j])]]
          
        } else if (is.na(test4[i,mcol]) & tp == "Repeat"){ # If value is NA and a repeat question
          replen <- length(rep_qs_repeat[[j]]) # Number of questions to repeat
          max_len <- replen * unname(rep_qs_max[[j]]) # Max length of rep q
          
          which(!is.na(test4[i,(mcol+1):(mcol+max_len)])) # Shows which of the qs are answered
          
          if (any(!is.na(test4[i,(mcol+1):(mcol+max_len)])) == FALSE) { # if there are no values
            next_q <- all_next_questions[[which(names(all_next_questions)==last)]]
          } else {
            
            last_rep <- max(which(!is.na(test4[i,(mcol+1):(mcol+max_len)]))) # last q
            est_reps <- ceiling(solve(replen,last_rep)) # estimated repeats
            replen * est_reps # number of cols this q takes
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
        } else if (is.na(test4[i,mcol])){ # If the entry is NA
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
            ans <- unlist(str_split(ans, ",")) # splits the multi answers into vector
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
          # j is 5 not col number
          all_Qnums[j] # question number eg Q2.3
          
          test4[i,mcol] # the number of times we want to repeat the given questions
          rep_qs_repeat[[j]] # the questions we want to repeat
          unname(rep_qs_max[[j]]) # the maximum times we can repeat
          
          num_rep <- ifelse(test4[i,mcol] > unname(rep_qs_max[[j]]), unname(rep_qs_max[[j]]), test4[i,mcol])
          
          rep_cols <-  as.numeric(num_rep) * length(rep_qs_repeat[[j]]) # number of column this repeat section takes
          
          for (k in 1:test4[i,mcol]){ # iterate through number of repeats wanted
            for (m in c(all_Qnums[j],rep_qs_repeat[[j]][-length(rep_qs_repeat[[j]])])){
              
              next_q <- all_next_questions[[which(names(all_next_questions)==m)]]
              #print(unname(next_q))
              list_of_qs <- append(list_of_qs, next_q)
            }
          }
          last <- rep_qs_repeat[[j]][length(rep_qs_repeat[[j]])] # last question of the repeating
          next_q <- all_next_questions[[which(names(all_next_questions)==last)]]
          #print(next_q)
        }
        
        ############################## End of types #################################
        
        list_of_qs <- append(list_of_qs, next_q)
        skip_test <- unname(next_q)==all_Qnums[j+1] # checks if the next question in order is next
        #print(skip_test)
        
        if (skip_test == FALSE & mcol != ncol(test4) & next_q != "finish"){ # if the next question is skipped go through here
          # all questions that will be skipped because of current question
          # check if the skipped have multiple values in the not next
          
          # All questions starting from the skipped question to one before the next question (all questions skipped)
          qs_skipped <- all_Qnums[(which(all_Qnums==all_Qnums[j+1])) : (which(all_Qnums==next_q)-1)] 
          
          tttest = 0
          for (q in qs_skipped) { # if the skipped question is in not_next
            if (q %in% list_of_skip_reason1[[i]]$not_next) tttest = tttest +1 
          }
          if (tttest == 1){
            head_skip <- qs_skipped[1] #first question in the chain
            # the real question that caused the skipping of head skip
            real_reason <- list_of_skip_reason1[[i]]$current_q[which(list_of_skip_reason1[[i]]$not_next==head_skip)]
            
            rr_col <- all_cols[names(all_cols) == real_reason]
            rr_question <- all_Qnames[which(names(all_Qnames) == real_reason)]
            answer <- test4[i,rr_col]
            
            reason2 <- data.frame(Skipped_question = qs_skipped, Reason = real_reason, Answer = paste(answer))
            
            df_skip_reason2 <- rbind(df_skip_reason2, reason2)
            
          } 
          else {
            which_split <- which(qs_skipped %in% list_of_skip_reason1[[i]]$not_next)
            split_lst <- split(qs_skipped, cumsum(seq_along(qs_skipped) %in% which_split))
            
            for (a in 1:length(split_lst)){
              
              qs_skipped <- split_lst[[a]]
              
              head_skip <- qs_skipped[1] #first question in the chain
              
              # the real question that caused the skipping of head skip
              real_reason <- list_of_skip_reason1[[i]]$current_q[which(list_of_skip_reason1[[i]]$not_next==head_skip)]
              
              rr_col <- all_cols[names(all_cols) == real_reason]
              rr_question <- all_Qnames[which(names(all_Qnames) == real_reason)]
              answer <- test4[i,rr_col]
              
              reason2 <- data.frame(Skipped_question = qs_skipped, Reason = real_reason, Answer = paste(answer))
              df_skip_reason2 <- rbind(df_skip_reason2, reason2)
            }
          }
        }
      }
      ####################### End of a row iteration ###########################
      
      list_of_skip_reason2[[i]] <- df_skip_reason2
      
    }
    
    ############################################### end #############################################
    
    
    if (length(grep("-", question)) == 1){ # if there is an - in the name
      #  all_pos <- unlist(gregexpr("&", nm)) # index of the &
      question <- unlist(strsplit(question, "-"))[[1]]
    }
    
    df <- list_of_skip_reason2[[row]] # Getting the right df
    
    reason_row <- which(df$Skipped_question == question) # Which row of the df we need
    all_reason <- df[reason_row,] # The row of data we want
    question <- sm_cols[column] # Question of the input
    skp_reason <- list(Question = question, Reason = all_reason$Reason, Answer = all_reason$Answer)
    
    class(skp_reason) <- "Skip_reason"
    skp_reason
  }
  
}
