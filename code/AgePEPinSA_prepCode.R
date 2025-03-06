
####################### Intro parameters ######################################

setwd()

#where to save the multi-subject behavioral data
out_path <- "[location]" 

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


###### complete flanker file? #############################
# create data frame
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




######### Creating df with age, flanker length, and fast RTs #################

collectiveDat <- read.csv("survey_data.csv")

# calculating how many fast RTs everyone has
fastRT_df <- data.frame(matrix(ncol=5, nrow=0))
colnames(fastRT_df)<- c ("subject", "number_fastRTs", "accuracy", "age", "rawFlankerLength")


for (i in 1:length(subList)){
  
  subject <- subList[i]
  #calculate number of fast RTs for this participant and make a variable that holds the number
  numFastRTs <- nrow(multiSub_data_raw[multiSub_data_raw$subject==subList[i] & multiSub_data_raw$rt < 150, ])
  #acc
  accuracy <- mean(multiSub_data_raw$accuracy[multiSub_data_raw$subject==subList[i]])
  #pull age in from the big collective sheet
  age <- collectiveDat$age[collectiveDat$subject==subList[i]]
  
  rawflankerLength <- nrow(multiSub_data_raw[multiSub_data_raw$subject==subList[i],])
  
  #make new row with values created above
  newRow <- data.frame(subject, numFastRTs, accuracy, age, rawflankerLength)
  colnames(newRow)<- c("subject", "number_fastRTs", "accuracy", "age", "rawFlankerLength")
  
  #add the new row to the existing df
  fastRT_df <- rbind(fastRT_df, newRow)
  
}

#removing anyone with ages outside the range or incorrect flanker length
fastRT_df <- fastRT_df[fastRT_df$age>6 & fastRT_df$age<18 & fastRT_df$rawFlankerLength==352,]



######### Removing participants with low Acc or too many fast RTs #################

# calculating outliers
# mean() + 3*sd()
outlierFastRTs<-  mean(fastRT_df$number_fastRTs)+ 3*sd(fastRT_df$number_fastRTs)


#initialize the temp table
cleaning_temp <- data.frame(matrix(ncol=16, nrow=0))
colnames(cleaning_temp)<- c ("subject", "session", "congruency", "trialType", "ITI", "accuracy", 
                                   "rt", "trialNum", "respType", "feedType", "blockNum", "firstOfBlock",
                             "priorAccuracy", "priorCongruency", "priorBad", "currentBad")


# Removing people with less than 60% acc and an outlier value of fast RTs
for (i in 1:length(subList)){
  
  #make a df where we only look at the i-th sub
  singlesubDat2 <- subset(multiSub_data_raw, multiSub_data_raw$subject==subList[i])
  
  #this adds participants to the clean file if they have above 60% accuracy and less than the outlierfastRTs
  if ((mean(singlesubDat2$accuracy) > .60)& fastRT_df$number_fastRTs[fastRT_df$subject==subList[i]]< outlierFastRTs){
    cleaning_temp <- rbind(cleaning_temp, singlesubDat2)
  } # end of accuracy-check if loop
  
} # end of "accuracy" for loop

# just checking to make sure it removed people correctly
unique(cleaning_temp$subject)


################# remove trials with fast RTs #####################

# Remove trials that have fast RTs
cleaning_temp_2 <- cleaning_temp[cleaning_temp$rt>150,]

######## prepping the final df without priorBad and currentBad trials ##############

# For PE/PC RT, PE/PC Acc, and DDM analyses, we need to exclude the following 
#   1. RT for trial before was too fast (priorBad==0 excludes any NAs and 1s) 
#   2. current trial RT was too fast (currentBad==0 excludes any 1s or NAs)
#   3. current trial is the first of the block or of the expt (currentBad==0)
#   4. trial before was congruent (priorCongruency)- we only want to include n trials that are incongruent (2)
cleaning_temp_3 <- subset(cleaning_temp_2, priorBad==0 & currentBad==0 & priorCongruency==2)



################# RT and Acc analyses #####################

# calculate (for each person) average accuracy for congruent and incongruent trials, average rt for 
# correct-con correct-incon error-con error-Incon. This is after we remove people based on RT and Acc

#sublist
subList_RT_Acc <- unique(cleaning_temp_2$subject)


#initialize Acc RT df
RT_Acc_df <- data.frame(matrix(ncol=21, nrow=0))
colnames(RT_Acc_df)<- c("subject", "Acc_avg", "RT_avg", "cong_Acc_avg", "incong_Acc_avg", 
                        "corr_cong_RT_avg", "corr_incong_RT_avg","err_cong_RT_avg","err_incong_RT_avg",
                        "num_corr_cong_trials","num_corr_incong_trials", "num_err_cong_trials", "num_err_incong_trials", 
                        "PE_Acc_avg", "PC_Acc_avg", "PE_RT_avg", "PC_RT_avg", "PE_corr_RT_avg", "PC_corr_RT_avg",
                        "num_PE_trials", "num_PC_trials")

for (i in 1:length(subList_RT_Acc)){
  
  subject <- subList_RT_Acc[i]
  
  ### These use cleaning_temp_2 bc we want to include all valid trials-- regular RT and Acc analyses will have fast RTs removed and less than 60% acc participants removed
  # mean accuracy when subject = i
  Acc_avg <- mean(cleaning_temp_2$accuracy[cleaning_temp_2$subject==subList_RT_Acc[i]])
  
  #mean RT when sub = i
  RT_avg <- mean(cleaning_temp_2$rt[cleaning_temp_2$subject==subList_RT_Acc[i]])
  
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
  
  # Count 
  num_corr_cong_trials <- nrow(cleaning_temp_2[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==1 & cleaning_temp_2$accuracy==1,])
  num_corr_incong_trials <- nrow(cleaning_temp_2[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==2 & cleaning_temp_2$accuracy==1,])
  num_err_cong_trials <-nrow(cleaning_temp_2[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==1 & cleaning_temp_2$accuracy==0,])
  num_err_incong_trials <- nrow(cleaning_temp_2[cleaning_temp_2$subject==subList_RT_Acc[i] & cleaning_temp_2$congruency==2 & cleaning_temp_2$accuracy==0,])
  
  ### THESE USE cleaning_temp_3 bc PE/PC analyses are dependent on the priorBad, priorCongruency, etc. exclusions we did for cleaning_temp_3
  
  # mean of accuracy when sub = i, priorAccuracy is 100 (post-error)
  PE_Acc_avg <- mean(cleaning_temp_3$accuracy[cleaning_temp_3$subject==subList_RT_Acc[i] & cleaning_temp_3$priorAccuracy==100])
  
  # mean of accuracy when sub = i, priorAccuracy is 111 (post-correct)
  PC_Acc_avg <- mean(cleaning_temp_3$accuracy[cleaning_temp_3$subject==subList_RT_Acc[i] & cleaning_temp_3$priorAccuracy==111])
  
  # mean of RT when sub = i, priorAccuracy is 100 (post-error)
  PE_RT_avg <- mean(cleaning_temp_3$rt[cleaning_temp_3$subject==subList_RT_Acc[i] & cleaning_temp_3$priorAccuracy==100])
  
  # mean of RT when sub = i, priorAccuracy is 111 (post-correct)
  PC_RT_avg <- mean(cleaning_temp_3$rt[cleaning_temp_3$subject==subList_RT_Acc[i] & cleaning_temp_3$priorAccuracy==111])
  
  
  # mean of RT when sub = i, priorAccuracy is 100 (post-error), and accuracy is 1 (correct)
  PE_corr_RT_avg <- mean(cleaning_temp_3$rt[cleaning_temp_3$subject==subList_RT_Acc[i] & cleaning_temp_3$priorAccuracy==100 & cleaning_temp_3$accuracy==1])
  # mean of RT when sub = i, priorAccuracy is 111 (post-correct), and accuracy is 1 (correct)
  PC_corr_RT_avg <- mean(cleaning_temp_3$rt[cleaning_temp_3$subject==subList_RT_Acc[i] & cleaning_temp_3$priorAccuracy==111 & cleaning_temp_3$accuracy==1])
  
  
  #calculate number of post-errors (100)
  num_PE_trials <- nrow(cleaning_temp_3[cleaning_temp_3$subject==subList_RT_Acc[i] & cleaning_temp_3$priorAccuracy == 100, ])
  #calculate number of post-corrects (111)
  num_PC_trials <- nrow(cleaning_temp_3[cleaning_temp_3$subject==subList_RT_Acc[i] & cleaning_temp_3$priorAccuracy == 111, ])
  
  
  #make new row with values created above
  newRow <- data.frame(subject, Acc_avg, RT_avg, cong_Acc_avg, incong_Acc_avg, 
                       corr_cong_RT_avg, corr_incong_RT_avg, err_cong_RT_avg, err_incong_RT_avg,
                       num_corr_cong_trials,num_corr_incong_trials, num_err_cong_trials, num_err_incong_trials,
                       PE_Acc_avg, PC_Acc_avg, PE_RT_avg, PC_RT_avg, PE_corr_RT_avg, PC_corr_RT_avg,
                       num_PE_trials, num_PC_trials)
  colnames(newRow)<- c("subject", "Acc_avg", "RT_avg", "cong_Acc_avg", "incong_Acc_avg", 
                       "corr_cong_RT_avg", "corr_incong_RT_avg","err_cong_RT_avg","err_incong_RT_avg",
                       "num_corr_cong_trials","num_corr_incong_trials", "num_err_cong_trials", "num_err_incong_trials", 
                       "PE_Acc_avg", "PC_Acc_avg", "PE_RT_avg", "PC_RT_avg", "PE_corr_RT_avg", "PC_corr_RT_avg",
                       "num_PE_trials", "num_PC_trials")
  
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

# Put NA for PE/PC values when they have less than 20 trials bc that's what happens for main DDM analyses
RT_Acc_df$PE_Acc_avg[RT_Acc_df$num_PE_trials<20] <- NA
RT_Acc_df$PC_Acc_avg[RT_Acc_df$num_PC_trials<20] <- NA
RT_Acc_df$PE_RT_avg[RT_Acc_df$num_PE_trials<20] <- NA
RT_Acc_df$PC_RT_avg[RT_Acc_df$num_PC_trials<20] <- NA
RT_Acc_df$PE_corr_RT_avg[RT_Acc_df$num_PE_trials<20] <- NA
RT_Acc_df$PC_corr_RT_avg[RT_Acc_df$num_PC_trials<20] <- NA


meanminus3sd_PE_Acc <- mean(RT_Acc_df$PE_Acc_avg, na.rm=TRUE)- 3* sd(RT_Acc_df$PE_Acc_avg, na.rm=TRUE)
meanplus3sd_PE_Acc <- mean(RT_Acc_df$PE_Acc_avg, na.rm=TRUE)+ 3* sd(RT_Acc_df$PE_Acc_avg, na.rm=TRUE)
RT_Acc_df$PE_Acc_avg[RT_Acc_df$PE_Acc_avg<meanminus3sd_PE_Acc | RT_Acc_df$PE_Acc_avg>meanplus3sd_PE_Acc] <-NA

meanminus3sd_PC_Acc <- mean(RT_Acc_df$PC_Acc_avg, na.rm=TRUE)- 3* sd(RT_Acc_df$PC_Acc_avg, na.rm=TRUE)
meanplus3sd_PC_Acc <- mean(RT_Acc_df$PC_Acc_avg, na.rm=TRUE)+ 3* sd(RT_Acc_df$PC_Acc_avg, na.rm=TRUE)
RT_Acc_df$PC_Acc_avg[RT_Acc_df$PC_Acc_avg<meanminus3sd_PC_Acc | RT_Acc_df$PC_Acc_avg>meanplus3sd_PC_Acc] <-NA

meanminus3sd_PE_RT <- mean(RT_Acc_df$PE_RT_avg, na.rm=TRUE)- 3* sd(RT_Acc_df$PE_RT_avg, na.rm=TRUE)
meanplus3sd_PE_RT <- mean(RT_Acc_df$PE_RT_avg, na.rm=TRUE)+ 3* sd(RT_Acc_df$PE_RT_avg, na.rm=TRUE)
RT_Acc_df$PE_RT_avg[RT_Acc_df$PE_RT_avg<meanminus3sd_PE_RT | RT_Acc_df$PE_RT_avg>meanplus3sd_PE_RT] <-NA

meanminus3sd_PC_RT <- mean(RT_Acc_df$PC_RT_avg, na.rm=TRUE)- 3* sd(RT_Acc_df$PC_RT_avg, na.rm=TRUE)
meanplus3sd_PC_RT <- mean(RT_Acc_df$PC_RT_avg, na.rm=TRUE)+ 3* sd(RT_Acc_df$PC_RT_avg, na.rm=TRUE)
RT_Acc_df$PC_RT_avg[RT_Acc_df$PC_RT_avg<meanminus3sd_PC_RT | RT_Acc_df$PC_RT_avg>meanplus3sd_PC_RT] <-NA

meanminus3sd_PC_corr_RT <- mean(RT_Acc_df$PE_corr_RT_avg, na.rm=TRUE)- 3* sd(RT_Acc_df$PE_corr_RT_avg, na.rm=TRUE)
meanplus3sd_PE_corr_RT <- mean(RT_Acc_df$PE_corr_RT_avg, na.rm=TRUE)+ 3* sd(RT_Acc_df$PE_corr_RT_avg, na.rm=TRUE)
RT_Acc_df$PE_RT_avg[RT_Acc_df$PE_corr_RT_avg<meanminus3sd_PE_RT | RT_Acc_df$PE_corr_RT_avg>meanplus3sd_PE_RT] <-NA

meanminus3sd_PC_corr_RT <- mean(RT_Acc_df$PC_corr_RT_avg, na.rm=TRUE)- 3* sd(RT_Acc_df$PC_corr_RT_avg, na.rm=TRUE)
meanplus3sd_PC_corr_RT <- mean(RT_Acc_df$PC_corr_RT_avg, na.rm=TRUE)+ 3* sd(RT_Acc_df$PC_corr_RT_avg, na.rm=TRUE)
RT_Acc_df$PC_RT_avg[RT_Acc_df$PC_corr_RT_avg<meanminus3sd_PC_corr_RT | RT_Acc_df$PC_corr_RT_avg>meanplus3sd_PC_corr_RT] <-NA

# write to disk
write.csv(RT_Acc_df, paste(out_path,"RT_Acc_df_for_",fileName_clean,".csv",
                           sep = "", collapse = NULL), row.names=FALSE)

################## clean .csv #####################################


multiSub_data_clean <- cleaning_temp_3

#write data to disk
write.csv(multiSub_data_clean,paste(out_path,fileName_clean,".csv",
                                    sep = "", collapse = NULL), row.names=FALSE)







