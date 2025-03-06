
####################### Intro parameters ######################################

setwd()

#where to save the multi-subject behavioral data
out_path <- "" 

#name for cleaned multi-sub file
fileName_clean <- "clean_flanker_data"


##################### start creating CLEAN dataframe #################################

# Import already made raw_data file
multiSub_data_raw <- read.csv("raw_flanker_data.csv")


######## contextually-labeled organizational columns ##############

#make a list of subjects to be used later. 
subList <- unique(multiSub_data_raw$subject)

## firstOfBlock
# Label the first trial in every block
# Every time there is a unique combo of sub and blockNum, label it as bad
multiSub_data_raw$firstOfBlock<- ifelse(!duplicated(multiSub_data_raw[c("subject", "blockNum")])==TRUE,1,0)


### PriorAccuracy ##
# Make a new column that will describe post-error (100) and post-correct (111)
# putting NA in order to bump everything down one to make it n+1. 
priorAccuracy <- c(NA, ifelse(multiSub_data_raw$accuracy <1, 100, 111))
multiSub_data_raw$priorAccuracy <- priorAccuracy[1:(length(priorAccuracy)-1)]


## priorCongruency ##
# Make a new column that will describe whether the prior trial was congruent (1) or incongruent (2)
priorCongruency <- c(NA, ifelse(multiSub_data_raw$congruency >1, 2, 1))
multiSub_data_raw$priorCongruency <- priorCongruency[1:(length(priorCongruency)-1)]



## priorBad ##
# Make a new column that will describe whether the prior trial was bad- fast RT in this case
# Note: there won't be a 1:1 with priorBad and currentBad because priorBad doesn't consider the 
# firstTrial or firstOfBlock.
priorBad <- c(NA, ifelse(multiSub_data_raw$rt <150, 1, 0))
multiSub_data_raw$priorBad <- priorBad[1:(length(priorBad)-1)]


## currentBad ##
# Make a new column that will describe whether the prior trial was bad-- fast RT, 
# firstTrial=1, or firstOfBlock=1
currentBad <- ifelse(multiSub_data_raw$rt <150 | multiSub_data_raw$firstOfBlock==1, 1, 0)
multiSub_data_raw$currentBad <- currentBad[1:(length(currentBad))]


######### Removing participants with low Acc or too many fast RTs or wrong age #################

collectiveDat <- read.csv("survey_data.csv")

# calculating how many fast RTs everyone has
fastRT_df <- data.frame(matrix(ncol=4, nrow=0))
colnames(fastRT_df)<- c ("subject", "number_fastRTs", "accuracy", "age")


for (j in 1:length(subList)){
  #isolate one subject so we can count fast RTs
  singlesubDat2 <- subset(multiSub_data_raw, multiSub_data_raw$subject==subList[j])
  
  subject <- subList[j]
  #calculate number of fast RTs for this participant and make a variable that holds the number
  numFastRTs <- nrow(singlesubDat2[singlesubDat2$rt < 150, ])
  #acc
  accuracy <- mean(singlesubDat2$accuracy)
  #pull age in from the big collective sheet
  age <- collectiveDat$age[collectiveDat$subject==subList[j]]
  
  #make new row with values created above
  newRow <- data.frame(subject, numFastRTs, accuracy, age)
  colnames(newRow)<- c("subject", "number_fastRTs", "accuracy", "age")
  
  #add the new row to the existing df
  fastRT_df <- rbind(fastRT_df, newRow)
  
}

#marking the people younger than 7 and older than 17 with NA
for (i in 1:length(subList)){
  if(fastRT_df$age[fastRT_df$subject==subList[i]]<7 | fastRT_df$age[fastRT_df$subject==subList[i]]>17){
    fastRT_df$age[fastRT_df$subject==subList[i]] <- NA
  }
}


# calculating outliers
# mean() + 3*sd()
outlierFastRTs<-  mean(fastRT_df$number_fastRTs[!is.na(fastRT_df$age)])+ 3*sd(fastRT_df$number_fastRTs[!is.na(fastRT_df$age)])


#initialize the temp table
cleaning_temp <- data.frame(matrix(ncol=16, nrow=0))
colnames(cleaning_temp)<- c ("subject", "session", "congruency", "trialType", "ITI", "accuracy", 
                                   "rt", "trialNum", "respType", "feedType", "blockNum", "firstOfBlock",
                             "priorAccuracy", "priorCongruency", "priorBad", "currentBad")


# Removing people with less than 60% acc and an outlier value of fast RTs and the correct age
for (i in 1:length(subList)){
  
  #make a df where we only look at the i-th sub
  singlesubDat2 <- subset(multiSub_data_raw, multiSub_data_raw$subject==subList[i])
  
  #this adds participants to the clean file if they have above 60% accuracy and less than the outlierfastRTs and not NA for fastRT_df (which we already marked bad ages with NA)
  if ((mean(singlesubDat2$accuracy) > .60)& fastRT_df$number_fastRTs[fastRT_df$subject==subList[i]]< outlierFastRTs & !is.na(fastRT_df$age[fastRT_df$subject==subList[i]])){
    cleaning_temp <- rbind(cleaning_temp, singlesubDat2)
  } # end of accuracy-check if loop
  
} # end of "accuracy" for loop

# just checking to make sure it removed people correctly
unique(cleaning_temp$subject)


################# remove trials with fast RTs #####################

# We remove people with below 60% accuracy, people with outlier number of fast RTs, and trials that have fast RTs
cleaning_temp_2 <- cleaning_temp[cleaning_temp$rt>150,]


################# RT and Acc analyses #####################

# calculate (for each person) average accuracy for congruent and incongruent trials, average rt for 
# correct-con correct-incon error-con error-Incon. This is after we remove people based on RT and Acc

#sublist
subList_RT_Acc <- unique(cleaning_temp_2$subject)


#initialize Acc RT df
RT_Acc_df <- data.frame(matrix(ncol=11, nrow=0))
colnames(RT_Acc_df)<- c("subject", "cong_Acc_avg", "incong_Acc_avg", 
                        "corr_cong_RT_avg", "corr_incong_RT_avg","err_cong_RT_avg","err_incong_RT_avg",
                        "num_corr_cong_trials","num_corr_incong_trials", "num_err_cong_trials", "num_err_incong_trials")

for (i in 1:length(subList_RT_Acc)){
  
  subject <- subList_RT_Acc[i]
  
  # 1 is congruent
  cong_Acc_avg <- mean(cleaning_temp_2$accuracy[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==1])
  
  # 2 is incongruent
  incong_Acc_avg <- mean(cleaning_temp_2$accuracy[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==2])
  
  # mean when subject is i, congruency is congruent, and acc is correct
  corr_cong_RT_avg <- mean(cleaning_temp_2$rt[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==1 & cleaning_temp_2$accuracy==1])
  
  # mean when subject is i, congruency is incongruent, and acc is correct
  corr_incong_RT_avg <- mean(cleaning_temp_2$rt[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==2 & cleaning_temp_2$accuracy==1])
  
  # mean when subject is i, congruency is congruent, and acc is error
  err_cong_RT_avg <- mean(cleaning_temp_2$rt[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==1 & cleaning_temp_2$accuracy==0])
  
  # mean when subject is i, congruency is incongruent, and acc is error
  err_incong_RT_avg <- mean(cleaning_temp_2$rt[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==2 & cleaning_temp_2$accuracy==0])
  
  
  # going to count these trials after all trial removals are complete (currentBad, priorBad, congruency)
  num_corr_cong_trials <- NA
  num_corr_incong_trials <- NA
  num_err_cong_trials <- NA
  num_err_incong_trials <- NA
  
  
  #make new row with values created above
  newRow <- data.frame(subject, cong_Acc_avg, incong_Acc_avg, 
                       corr_cong_RT_avg, corr_incong_RT_avg, err_cong_RT_avg, err_incong_RT_avg,
                       num_corr_cong_trials,num_corr_incong_trials, num_err_cong_trials, num_err_incong_trials)
  colnames(newRow)<- c("subject", "cong_Acc_avg", "incong_Acc_avg", 
                       "corr_cong_RT_avg", "corr_incong_RT_avg","err_cong_RT_avg","err_incong_RT_avg",
                       "num_corr_cong_trials","num_corr_incong_trials", "num_err_cong_trials", "num_err_incong_trials")
  
  #add the new row to the existing df
  RT_Acc_df <- rbind(RT_Acc_df, newRow)
}

# there was one person in err_cong_RT that didn't have any trials, so R put their value as NaN. Converting it to NA
RT_Acc_df[RT_Acc_df=="NaN"]<-NA

# Finding + and - outliers for each person. Then removing any values that are above + and below -
meanminus3sd_congAcc <- mean(RT_Acc_df$cong_Acc_avg)- 3* sd(RT_Acc_df$cong_Acc_avg)
meanplus3sd_congAcc <- mean(RT_Acc_df$cong_Acc_avg)+ 3* sd(RT_Acc_df$cong_Acc_avg)
RT_Acc_df$cong_Acc_avg[RT_Acc_df$cong_Acc_avg<meanminus3sd_congAcc | RT_Acc_df$cong_Acc_avg>meanplus3sd_congAcc] <-NA

meanminus3sd_incongAcc <- mean(RT_Acc_df$incong_Acc_avg)- 3* sd(RT_Acc_df$incong_Acc_avg)
meanplus3sd_incongAcc <- mean(RT_Acc_df$incong_Acc_avg)+ 3* sd(RT_Acc_df$incong_Acc_avg)
RT_Acc_df$incong_Acc_avg[RT_Acc_df$incong_Acc_avg<meanminus3sd_incongAcc | RT_Acc_df$incong_Acc_avg>meanplus3sd_incongAcc] <-NA

meanminus3sd_cong_corr_RT <- mean(RT_Acc_df$corr_cong_RT_avg)- 3* sd(RT_Acc_df$corr_cong_RT_avg)
meanplus3sd_cong_corr_RT<- mean(RT_Acc_df$corr_cong_RT_avg)+ 3* sd(RT_Acc_df$corr_cong_RT_avg)
RT_Acc_df$corr_cong_RT_avg[RT_Acc_df$corr_cong_RT_avg<meanminus3sd_cong_corr_RT | RT_Acc_df$corr_cong_RT_avg>meanplus3sd_cong_corr_RT] <-NA

meanminus3sd_corr_incong_RT <- mean(RT_Acc_df$corr_incong_RT_avg)- 3* sd(RT_Acc_df$corr_incong_RT_avg)
meanplus3sd_corr_incong_RT <- mean(RT_Acc_df$corr_incong_RT_avg)+ 3* sd(RT_Acc_df$corr_incong_RT_avg)
RT_Acc_df$corr_incong_RT_avg[RT_Acc_df$corr_incong_RT_avg<meanminus3sd_corr_incong_RT | RT_Acc_df$corr_incong_RT_avg>meanplus3sd_corr_incong_RT] <-NA

meanminus3sd_err_cong_RT<- mean(RT_Acc_df$err_cong_RT_avg,na.rm=TRUE)- 3* sd(RT_Acc_df$err_cong_RT_avg,na.rm=TRUE)
meanplus3sd_err_cong_RT <- mean(RT_Acc_df$err_cong_RT_avg,na.rm=TRUE)+ 3* sd(RT_Acc_df$err_cong_RT_avg,na.rm=TRUE)
RT_Acc_df$err_cong_RT_avg[RT_Acc_df$err_cong_RT_avg<meanminus3sd_err_cong_RT | RT_Acc_df$err_cong_RT_avg>meanplus3sd_err_cong_RT] <-NA

meanminus3sd_err_incong_RT <- mean(RT_Acc_df$err_incong_RT_avg)- 3* sd(RT_Acc_df$err_incong_RT_avg)
meanplus3sd_err_incong_RT <- mean(RT_Acc_df$err_incong_RT_avg)+ 3* sd(RT_Acc_df$err_incong_RT_avg)
RT_Acc_df$err_incong_RT_avg[RT_Acc_df$err_incong_RT_avg<meanminus3sd_err_incong_RT | RT_Acc_df$err_incong_RT_avg>meanplus3sd_err_incong_RT] <-NA



######## making the final df without priorBad and currentBad trials ##############


# Making a subset of data that excludes when
#   1. RT for trial before was too fast (priorBad)
#   2. current trial RT was too fast (currentBad)
#   3. current trial is the first of the block or of the expt (currentBad)
#   4. trial before was congruent (priorCongruency)- we only want to include n trials that are incongruent (2)
# (did priorCongruency ==2 instead of !=1 bc of the NA at the top, don't want that to be counted just in case)
cleaning_temp_3 <- subset(cleaning_temp_2, priorBad != 1 & currentBad != 1 & priorCongruency==2)

subList_clean <- unique(cleaning_temp_3$subject)




################# remove participants with fewer than 20 p-e or p-c trials #####################


#initialize post-error/correct df
postErrCorr_df <- data.frame(matrix(ncol=3, nrow=0))
colnames(postErrCorr_df)<- c("subject", "numPostError", "numPostCorrect")


for (i in 1:length(subList_clean)){

  #make a df where we only look at the i-th sub
  singlesubDat <- subset(cleaning_temp_3, cleaning_temp_2$subject==subList_clean[i])

  # define the subject number to add to our fastRts df
  subject <- subList_clean[i]

  #calculate number of post-errors (100)
  numPostError <- nrow(singlesubDat[singlesubDat$priorAccuracy == 100, ])
  #calculate number of post-corrects (111)
  numPostCorrect <- nrow(singlesubDat[singlesubDat$priorAccuracy == 111, ])

  #make new row with values created above
  newRow <- data.frame(subject, numPostError,numPostCorrect)
  colnames(newRow)<- c("subject", "numPostError", "numPostCorrect")

  #add the new row to the existing df
  postErrCorr_df <- rbind(postErrCorr_df, newRow)
}


#initialize the final table
multiSub_data_clean <- data.frame(matrix(ncol=16, nrow=0))
colnames(multiSub_data_clean)<- c ("subject", "session", "congruency", "trialType", "ITI", "accuracy",
                             "rt", "trialNum", "respType", "feedType", "blockNum", "firstOfBlock",
                             "priorAccuracy", "priorCongruency", "priorBad", "currentBad")

for (i in 1:length(subList_clean)){

  #make a df where we only look at the i-th sub
  singlesubDat3 <- subset(cleaning_temp_3, cleaning_temp_3$subject==subList_clean[i])

  # if the number of post-error trials OR the number of post-correct trials for a subject[i] is greater than 20...
  if (postErrCorr_df$numPostError[postErrCorr_df$subject==subList_clean[i]]>20 | postErrCorr_df$numPostCorrect[postErrCorr_df$subject==subList_clean[i]]>20){
   #... then add it to the final df.
     multiSub_data_clean <- rbind(multiSub_data_clean, singlesubDat3)
  }
}




################## clean .csv #####################################


#write data to disk
currentDate <- format(Sys.time(), "%Y-%m-%d_%I%p")
write.csv(multiSub_data_clean,paste(out_path,fileName_clean,currentDate,".csv",
                                    sep = "", collapse = NULL), row.names=FALSE)



###################### RT Acc .csv #############################

for (i in 1:length(subList_RT_Acc)){
  # count trials for the subject that are correct and congruent
  RT_Acc_df[RT_Acc_df$subject==subList_RT_Acc[i], 8] <- nrow(multiSub_data_clean[multiSub_data_clean$subject==subList_RT_Acc[i] & multiSub_data_clean$congruency==1 & multiSub_data_clean$accuracy==1,])
  
  # count trials for the subject that are correct and incongruent
  RT_Acc_df[RT_Acc_df$subject==subList_RT_Acc[i], 9] <- nrow(multiSub_data_clean[multiSub_data_clean$subject==subList_RT_Acc[i] & multiSub_data_clean$congruency==2 & multiSub_data_clean$accuracy==1,])
  
  # count trials for the subject that are error and congruent
  RT_Acc_df[RT_Acc_df$subject==subList_RT_Acc[i], 10] <- nrow(multiSub_data_clean[multiSub_data_clean$subject==subList_RT_Acc[i] & multiSub_data_clean$congruency==1 & multiSub_data_clean$accuracy==0,])
  
  # count trials for the subject that are error and incongruent
  RT_Acc_df[RT_Acc_df$subject==subList_RT_Acc[i], 11] <- nrow(multiSub_data_clean[multiSub_data_clean$subject==subList_RT_Acc[i] & multiSub_data_clean$congruency==2 & multiSub_data_clean$accuracy==0,])
  
}


# write to disk
write.csv(RT_Acc_df, paste(out_path,"RT_Acc_df_for_",fileName_clean,".csv",
                           sep = "", collapse = NULL), row.names=FALSE)



###### complete flanker file? #####
rawFlankerLength <- data.frame(matrix(ncol=2, nrow=0))
colnames(rawFlankerLength)<- c("subject", "flankerLength")
# a table showing me how many rows each subject has
for (i in 1:length(subList)){
  subject <- subList[i]
  flankerLength <- nrow(multiSub_data_raw[multiSub_data_raw$subject==subList[i],])
  
  newRow <- data.frame(subject, flankerLength)
  colnames(newRow)<- c("subject", "flankerLength")
  
  #add the new row to the existing df
  rawFlankerLength <- rbind(rawFlankerLength, newRow)
}
rawFlankerLength

write.csv(rawFlankerLength, paste0(out_path,"rawFlankerLength.csv"), row.names=FALSE)




