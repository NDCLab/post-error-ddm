# DDM data cleanup and analysis with scared and age data


# Import DDM output
ddmOutput <- read.csv("~/OneDrive - Florida International University/Research/DDM project/masters-project/ddm-materials/2022-08-30_run/10000trialFits_for_v6-0_clean_PRE_flanker_2022-08-29_10-PM_rundate-2022-08-30.csv")

#Create sublist
subList <- unique(ddmOutput$subject)



############ Reject anyone with less than 10 trials in post-error or post-correct ###############

#read in the cleaned pre-ddm sheet
clean_pre_ddm_df <- read.csv("~/OneDrive - Florida International University/Research/DDM project/masters-project/cleaning-materials/v6-0_clean_PRE_flanker_2022-08-29_10-PM.csv")


# Add a column for number of trials to ddmOutput
ddmOutput$numTrials <- NA

for (i in 1:length(subList)){
  
  #make a df where we only look at the i-th sub
  singlesubDat <- subset(clean_pre_ddm_df, clean_pre_ddm_df$subject==subList[i])
  
  #calculate number of post-errors (100)
  numPostError <- nrow(singlesubDat[singlesubDat$priorAccuracy == 100, ])
  #calculate number of post-corrects (111)
  numPostCorrect <- nrow(singlesubDat[singlesubDat$priorAccuracy == 111, ])
  
  # when subject in ddmoutput is the one specified in the loop and whichcondition in ddmOutput is 100, put numPostError in it
  ddmOutput$numTrials[subList[i]==ddmOutput$subject & ddmOutput$whichCondition==100] <- numPostError
  # when subject in ddmoutput is the one specified in the loop and whichcondition in ddmOutput is 111, put numPostCorrect in it
  ddmOutput$numTrials[subList[i]==ddmOutput$subject & ddmOutput$whichCondition==111] <- numPostCorrect

}

# # Put NAs for people with too few trials in each condition- people with too few trials should already
# #be filtered out based on the cleaning script
ddmOutput$numTrials[ddmOutput$numTrials<10] <-NA

# #initialize a table for  means and SD 
# # "raw" = mean and SD  prior to numTrials exclusion and outlier exclusion. "clean" = with all exclusions
# summary_df <- data.frame(matrix(ncol=4, nrow=0))
# colnames(summary_df)<- c( "raw_mean", "raw_sd", "clean_mean", "clean_sd")

############# reject people with bad fit stats on either sheet ###################


#put an NA for error fitstats that are above 200 (bc an SD rejection wasn't catching all the wild fits)
#George determined this would be the best based on the following:
# Note that if you set at 200 or 1000 the same ones are removed. The reason I am saying 200 
# though is that based on the data, if you had anyone with a value at 200 or above that would 
# also be a clear outlier. So it is good to set the threshold in a way that we can use if we 
# run more people and happen to get someone with a value I'd say 500 or so
ddmOutput$fitStat[ddmOutput$fitStat>200] <-NA

# # calculate the SD of "fitStat" for error
# # mean(when condition is post-error and when numTrials is not NA) + 3*sd(when post-err and not NA)
# fitStat_meanplus3sd_err <- mean(ddmOutputEdit$fitStat[ddmOutputEdit$whichCondition==100 & !is.na(ddmOutputEdit$numTrials)])+ 3*sd(ddmOutputEdit$fitStat[ddmOutputEdit$whichCondition==100 & !is.na(ddmOutputEdit$numTrials)])
# 
# # calculate the SD of "fitStat" for correct
# fitStat_meanplus3sd_corr <- mean(ddmOutputEdit$fitStat[ddmOutputEdit$whichCondition==111 & !is.na(ddmOutputEdit$numTrials)])+ 3*sd(ddmOutputEdit$fitStat[ddmOutputEdit$whichCondition==111 & !is.na(ddmOutputEdit$numTrials)])
# 
# #Put an NA for fitstats that are outliers in the post-error/correct conditions
# #### Commenting out because this outlier rejection isn't catching all of the 
# #subs with wild fitStats. Made a new one below that rejects if fitStat is over 200
# ddmOutputEdit$fitStat[ddmOutputEdit$whichCondition==100 & ddmOutputEdit$fitStat>fitStat_meanplus3sd_err] <-NA
# ddmOutputEdit$fitStat[ddmOutputEdit$whichCondition==111 & ddmOutputEdit$fitStat>fitStat_meanplus3sd_corr] <-NA

# rejecting anyone with a ceiling number of iterations
ddmOutput$iterNum[ddmOutput$iterNum==200] <-NA

############ Create a new df that's more conducive to subtraction ###################


#initialize the df
analysisDat<-  data.frame(matrix(ncol=26, nrow=0))
colnames(analysisDat) <- c("subject","a_err", "a_corr", "a_diff", "ter_err", "ter_corr", "ter_diff", 
                           "p_err", "p_corr", "p_diff", "rd_err", "rd_corr", "rd_diff", 
                           "sda_err", "sda_corr", "sda_diff", "sda_rd_ratio_err", "sda_rd_ratio_corr", "sda_rd_ratio_diff",
                           "fitStat_err", "fitStat_corr", "fitStat_diff", "iterNum_err", "iterNum_corr",  
                           "numTrials_err", "numTrials_corr")

# adding the data from ddmOutput to analysisDat
for (i in 1:length(subList)){
  
  # subject variable- attaching 100 to it so it pulls the subject number only once instead of storing a two variable list
  subject <- ddmOutput$subject[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  
  # pull a when it's post-error and post-correct
  a_err <- ddmOutput$a[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  a_corr <- ddmOutput$a[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  # It is easy to do the subtraction here, but I want to do it after we check for outliers, bc it's easier to mark the diff as NA that way
  a_diff <- NA
  
  # pull ter when it's post-error and post-correct
  ter_err <- ddmOutput$ter[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  ter_corr <- ddmOutput$ter[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  ter_diff <- NA #will check for outliers before doing subtraction
  
  # pull p when it's post-error and post-correct
  p_err <- ddmOutput$p[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  p_corr <- ddmOutput$p[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  p_diff <- NA #will check for outliers before doing subtraction
  
  # pull rd when it's post-error and post-correct
  rd_err <- ddmOutput$rd[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  rd_corr <- ddmOutput$rd[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  rd_diff <- NA #will check for outliers before doing subtraction
  
  # pull sda when it's post-error and post-correct
  sda_err <- ddmOutput$sda[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  sda_corr <- ddmOutput$sda[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  sda_diff <- NA #will check for outliers before doing subtraction
  
  # make sda/rd ratios
  sda_rd_ratio_err <- NA #will check for outliers before doing calculation
  sda_rd_ratio_corr <- NA #will check for outliers before doing calculation
  sda_rd_ratio_diff <- NA #will check for outliers before doing calculation
  
  # pull fitStat when it's post-error and post-correct
  fitStat_err <- ddmOutput$fitStat[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  fitStat_corr <- ddmOutput$fitStat[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  fitStat_diff <- fitStat_err-fitStat_corr # can do subtraction here bc fitStat threshold is dealt with above
  
  # pull iterNum when it's post-error and post-correct
  iterNum_err <- ddmOutput$iterNum[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  iterNum_corr <- ddmOutput$iterNum[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  
  # pull numTrials when it's post-error and post-correct
  numTrials_err <- ddmOutput$numTrials[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  numTrials_corr <- ddmOutput$numTrials[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  
  # make new row with values created above
  newRow <- data.frame(subject, a_err, a_corr, a_diff, ter_err, ter_corr, ter_diff, p_err, p_corr, p_diff, rd_err, rd_corr, rd_diff, 
                       sda_err, sda_corr, sda_diff, sda_rd_ratio_err, sda_rd_ratio_corr, sda_rd_ratio_diff,
                       fitStat_err, fitStat_corr, fitStat_diff, iterNum_err, iterNum_corr,  
                       numTrials_err, numTrials_corr)
  colnames(newRow)<- c("subject","a_err", "a_corr", "a_diff", "ter_err", "ter_corr", "ter_diff", 
                       "p_err", "p_corr", "p_diff", "rd_err", "rd_corr", "rd_diff", 
                       "sda_err", "sda_corr", "sda_diff", "sda_rd_ratio_err", "sda_rd_ratio_corr", "sda_rd_ratio_diff",
                       "fitStat_err", "fitStat_corr", "fitStat_diff", "iterNum_err", "iterNum_corr",
                       "numTrials_err", "numTrials_corr")
  
  # add row to the df
  analysisDat <- rbind(analysisDat, newRow)
  
}


#placeholder
analysisDat1 <- analysisDat

# If a person has NAs for numTrials (lower than 10), NAs for fitStat (greater than 200), or NAs for iterNum (ceiling value of 200),
# then their post-error parms or post-correct parms are marked with NA.
# Ex. numTrials_err is lower than 10, meaning that all of the post-error parms are invalid since there's not enough trials to get a good fit.
# Since you fit post error and post correct separately, they could still have valid parms for one condition but not the other
# Included column 22 (fitStat_diff) because the diff was calculated above, meaning that the diff would 
# remain even tho the post-err/corr fitStat was labeled with NA for low numTrials or ceiling iterNum.
for (i in 1:length(subList)){
  
  # if number of trials for post-err is NA, fitStat_err is NA, or iterNum_err is ceiling (200), then all parms for post-error are NA
  if(is.na(analysisDat1$numTrials_err[analysisDat1$subject==subList[i]]) | is.na(analysisDat1$fitStat_err[analysisDat1$subject==subList[i]]) | is.na(analysisDat1$iterNum_err[analysisDat1$subject==subList[i]])){
    analysisDat1[analysisDat1$subject==subList[i],c(2,5,8,11,14,17,20,22)] <- NA
  }
  
  # if number of trials for post-corr is NA, fitStat_corr is NA, or iterNum_corr is ceiling (200), then all parms for post-error are NA
  if(is.na(analysisDat1$numTrials_corr[analysisDat1$subject==subList[i]]) | is.na(analysisDat1$fitStat_corr[analysisDat1$subject==subList[i]]) | is.na(analysisDat1$iterNum_corr[analysisDat1$subject==subList[i]])){
    analysisDat1[analysisDat1$subject==subList[i],c(3,6,9,12,15,18,21,22)] <- NA
  }
}



######### add in SCARED and age  ########



#read in COLLECTIVE spreadsheet
collectiveDat <- read.csv("~/OneDrive - Florida International University/Research/DDM project/masters-project/masters-data/temp_COLLECTIVE_age_SCARED_gender_v1.csv")

#new column for child age
analysisDat1[ , "age"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting age scores on comboDat
  analysisDat1$age[analysisDat1$subject== subList[i]] <- collectiveDat$age[collectiveDat$subject== subList[i]]
}

#new column for child gender
analysisDat1[ , "gender"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting age scores on comboDat
  analysisDat1$gender[analysisDat1$subject== subList[i]] <- collectiveDat$gender[collectiveDat$subject== subList[i]]
}


#new column for child SCARED
analysisDat1[ , "SCARED_total_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_total_Ch_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_total_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED panic
analysisDat1[ , "SCARED_panic_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_panic_Ch_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_panic_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED GAD
analysisDat1[ , "SCARED_GAD_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_GAD_Ch_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_GAD_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED separation
analysisDat1[ , "SCARED_separation_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_separation_Ch_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_separation_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED separation
analysisDat1[ , "SCARED_SAD_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_SAD_Ch_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_SAD_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED school 
analysisDat1[ , "SCARED_school_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_school_Ch_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_school_Ch_PRE[collectiveDat$subject==subList[i]]
}




#new column for parent SCARED
analysisDat1[ , "SCARED_total_Par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_total_Par_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_total_Par_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED panic
analysisDat1[ , "SCARED_panic_par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_panic_par_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_panic_par_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED GAD
analysisDat1[ , "SCARED_GAD_par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_GAD_par_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_GAD_par_PRE[collectiveDat$subject==subList[i]]
}

#new column for parent SCARED separation
analysisDat1[ , "SCARED_separation_par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_separation_par_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_separation_par_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED separation
analysisDat1[ , "SCARED_SAD_par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_SAD_par_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_SAD_par_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED school 
analysisDat1[ , "SCARED_school_par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat1$SCARED_school_par_PRE[analysisDat1$subject==subList[i]] <- collectiveDat$SCARED_school_par_PRE[collectiveDat$subject==subList[i]]
}



analysisDat2<- analysisDat1

################# Ter- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "ter" for post-error. 
ter_meanminus3sd_err <- mean(analysisDat2$ter_err, na.rm=TRUE)- 3*sd(analysisDat2$ter_err, na.rm=TRUE)
ter_meanplus3sd_err <- mean(analysisDat2$ter_err, na.rm=TRUE)+ 3*sd(analysisDat2$ter_err, na.rm=TRUE)
# mark NA for any outliers in ter for post-error
analysisDat2$ter_err[analysisDat2$ter_err<ter_meanminus3sd_err | analysisDat2$ter_err>ter_meanplus3sd_err] <-NA


## calculate the SD of "ter" for post-correct
ter_meanminus3sd_corr <- mean(analysisDat2$ter_corr, na.rm=TRUE)- 3*sd(analysisDat2$ter_corr, na.rm=TRUE)
ter_meanplus3sd_corr <- mean(analysisDat2$ter_corr, na.rm=TRUE)+ 3*sd(analysisDat2$ter_corr, na.rm=TRUE)
# mark NA for any outliers in ter for post-correct
analysisDat2$ter_corr[analysisDat2$ter_corr<ter_meanminus3sd_corr | analysisDat2$ter_corr>ter_meanplus3sd_corr] <-NA


#doing the subtraction
analysisDat2$ter_diff <- analysisDat2$ter_err-analysisDat2$ter_corr

#ter subtraction outlier rejection
ter_diff_meanminus3sd <- mean(analysisDat2$ter_diff, na.rm=TRUE)- 3*sd(analysisDat2$ter_diff, na.rm=TRUE)
ter_diff_meanplus3sd <- mean(analysisDat2$ter_diff, na.rm=TRUE)+ 3*sd(analysisDat2$ter_diff, na.rm=TRUE)
analysisDat2$ter_diff[analysisDat2$ter_diff<ter_diff_meanminus3sd | analysisDat2$ter_diff>ter_diff_meanplus3sd] <-NA


################# a- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "a" for post-error. 
a_meanminus3sd_err <- mean(analysisDat2$a_err, na.rm=TRUE)- 3*sd(analysisDat2$a_err, na.rm=TRUE)
a_meanplus3sd_err <- mean(analysisDat2$a_err, na.rm=TRUE)+ 3*sd(analysisDat2$a_err, na.rm=TRUE)
# mark NA for any outliers in a for post-error
analysisDat2$a_err[analysisDat2$a_err<a_meanminus3sd_err | analysisDat2$a_err>a_meanplus3sd_err] <-NA



## calculate the SD of "a" for post-correct
a_meanminus3sd_corr <- mean(analysisDat2$a_corr, na.rm=TRUE)- 3*sd(analysisDat2$a_corr, na.rm=TRUE)
a_meanplus3sd_corr <- mean(analysisDat2$a_corr, na.rm=TRUE)+ 3*sd(analysisDat2$a_corr, na.rm=TRUE)
# mark NA for any outliers in a for post-correct
analysisDat2$a_corr[analysisDat2$a_corr<a_meanminus3sd_corr | analysisDat2$a_corr>a_meanplus3sd_corr] <-NA


#doing the subtraction
analysisDat2$a_diff <- analysisDat2$a_err-analysisDat2$a_corr

#a subtraction outlier rejection
a_diff_meanminus3sd <- mean(analysisDat2$a_diff, na.rm=TRUE)- 3*sd(analysisDat2$a_diff, na.rm=TRUE)
a_diff_meanplus3sd <- mean(analysisDat2$a_diff, na.rm=TRUE)+ 3*sd(analysisDat2$a_diff, na.rm=TRUE)
analysisDat2$a_diff[analysisDat2$a_diff<a_diff_meanminus3sd | analysisDat2$a_diff>a_diff_meanplus3sd] <-NA


################# p- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "p" for post-error. 
p_meanminus3sd_err <- mean(analysisDat2$p_err, na.rm=TRUE)- 3*sd(analysisDat2$p_err, na.rm=TRUE)
p_meanplus3sd_err <- mean(analysisDat2$p_err, na.rm=TRUE)+ 3*sd(analysisDat2$p_err, na.rm=TRUE)
# mark NA for any outliers in p for post-error
analysisDat2$p_err[analysisDat2$p_err<p_meanminus3sd_err | analysisDat2$p_err>p_meanplus3sd_err] <-NA


## calculate the SD of "p" for post-correct
p_meanminus3sd_corr <- mean(analysisDat2$p_corr, na.rm=TRUE)- 3*sd(analysisDat2$p_corr, na.rm=TRUE)
p_meanplus3sd_corr <- mean(analysisDat2$p_corr, na.rm=TRUE)+ 3*sd(analysisDat2$p_corr, na.rm=TRUE)
# mark NA for any outliers in p for post-correct
analysisDat2$p_corr[analysisDat2$p_corr<p_meanminus3sd_corr | analysisDat2$p_corr>p_meanplus3sd_corr] <-NA


#doing the subtraction
analysisDat2$p_diff <- analysisDat2$p_err-analysisDat2$p_corr

#p subtraction outlier rejection
p_diff_meanminus3sd <- mean(analysisDat2$p_diff, na.rm=TRUE)- 3*sd(analysisDat2$p_diff, na.rm=TRUE)
p_diff_meanplus3sd <- mean(analysisDat2$p_diff, na.rm=TRUE)+ 3*sd(analysisDat2$p_diff, na.rm=TRUE)
analysisDat2$p_diff[analysisDat2$p_diff<p_diff_meanminus3sd | analysisDat2$p_diff>p_diff_meanplus3sd] <-NA



################# rd- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "rd" for post-error. 
rd_meanminus3sd_err <- mean(analysisDat2$rd_err, na.rm=TRUE)- 3*sd(analysisDat2$rd_err, na.rm=TRUE)
rd_meanplus3sd_err <- mean(analysisDat2$rd_err, na.rm=TRUE)+ 3*sd(analysisDat2$rd_err, na.rm=TRUE)
# mark NA for any outliers in rd for post-error
analysisDat2$rd_err[analysisDat2$rd_err<rd_meanminus3sd_err | analysisDat2$rd_err>rd_meanplus3sd_err] <-NA


## calculate the SD of "rd" for post-correct
rd_meanminus3sd_corr <- mean(analysisDat2$rd_corr, na.rm=TRUE)- 3*sd(analysisDat2$rd_corr, na.rm=TRUE)
rd_meanplus3sd_corr <- mean(analysisDat2$rd_corr, na.rm=TRUE)+ 3*sd(analysisDat2$rd_corr, na.rm=TRUE)
# mark NA for any outliers in rd for post-correct
analysisDat2$rd_corr[analysisDat2$rd_corr<rd_meanminus3sd_corr | analysisDat2$rd_corr>rd_meanplus3sd_corr] <-NA


#doing the subtraction
analysisDat2$rd_diff <- analysisDat2$rd_err-analysisDat2$rd_corr

#rd subtraction outlier rejection
rd_diff_meanminus3sd <- mean(analysisDat2$rd_diff, na.rm=TRUE)- 3*sd(analysisDat2$rd_diff, na.rm=TRUE)
rd_diff_meanplus3sd <- mean(analysisDat2$rd_diff, na.rm=TRUE)+ 3*sd(analysisDat2$rd_diff, na.rm=TRUE)
analysisDat2$rd_diff[analysisDat2$rd_diff<rd_diff_meanminus3sd | analysisDat2$rd_diff>rd_diff_meanplus3sd] <-NA


################# sda- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "sda" for post-error. 
sda_meanminus3sd_err <- mean(analysisDat2$sda_err, na.rm=TRUE)- 3*sd(analysisDat2$sda_err, na.rm=TRUE)
sda_meanplus3sd_err <- mean(analysisDat2$sda_err, na.rm=TRUE)+ 3*sd(analysisDat2$sda_err, na.rm=TRUE)
# mark NA for any outliers in sda for post-error
analysisDat2$sda_err[analysisDat2$sda_err<sda_meanminus3sd_err | analysisDat2$sda_err>sda_meanplus3sd_err] <-NA


## calculate the SD of "sda" for post-correct
sda_meanminus3sd_corr <- mean(analysisDat2$sda_corr, na.rm=TRUE)- 3*sd(analysisDat2$sda_corr, na.rm=TRUE)
sda_meanplus3sd_corr <- mean(analysisDat2$sda_corr, na.rm=TRUE)+ 3*sd(analysisDat2$sda_corr, na.rm=TRUE)
# mark NA for any outliers in sda for post-correct
analysisDat2$sda_corr[analysisDat2$sda_corr<sda_meanminus3sd_corr | analysisDat2$sda_corr>sda_meanplus3sd_corr] <-NA


#doing the subtraction
analysisDat2$sda_diff <- analysisDat2$sda_err-analysisDat2$sda_corr

#sda subtraction outlier rejection
sda_diff_meanminus3sd <- mean(analysisDat2$sda_diff, na.rm=TRUE)- 3*sd(analysisDat2$sda_diff, na.rm=TRUE)
sda_diff_meanplus3sd <- mean(analysisDat2$sda_diff, na.rm=TRUE)+ 3*sd(analysisDat2$sda_diff, na.rm=TRUE)
analysisDat2$sda_diff[analysisDat2$sda_diff<sda_diff_meanminus3sd | analysisDat2$sda_diff>sda_diff_meanplus3sd] <-NA


################# sda/rd- reject outliers, subtract, reject diff outliers ############

## calculate the ratio for error
analysisDat2$sda_rd_ratio_err <- (analysisDat2$sda_err)/(analysisDat2$rd_err)

## calculate the ratio for correct
analysisDat2$sda_rd_ratio_corr <- (analysisDat2$sda_corr)/(analysisDat2$rd_corr)

## calculate the outliers for "ter" for post-error. 
sdard_meanminus3sd_err <- mean(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)
sdard_meanplus3sd_err <- mean(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)
# mark NA for any outliers in ter for post-error
analysisDat2$sda_rd_ratio_err[analysisDat2$sda_rd_ratio_err<sdard_meanminus3sd_err | analysisDat2$sda_rd_ratio_err>sdard_meanplus3sd_err] <-NA


## calculate the SD of "ter" for post-correct
sdard_meanminus3sd_corr <- mean(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)
sdard_meanplus3sd_corr <- mean(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)
# mark NA for any outliers in ter for post-correct
analysisDat2$sda_rd_ratio_corr[analysisDat2$sda_rd_ratio_corr<sdard_meanminus3sd_corr | analysisDat2$sda_rd_ratio_corr>sdard_meanplus3sd_corr] <-NA


#doing the subtraction
analysisDat2$sda_rd_ratio_diff <- analysisDat2$sda_rd_ratio_err-analysisDat2$sda_rd_ratio_corr

#ter subtraction outlier rejection
sdard_diff_meanminus3sd <- mean(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)
sdard_diff_meanplus3sd <- mean(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)
analysisDat2$sda_rd_ratio_diff[analysisDat2$sda_rd_ratio_diff<sdard_diff_meanminus3sd | analysisDat2$sda_rd_ratio_diff>sdard_diff_meanplus3sd] <-NA



################ Analyses for age #############################

#regression model a
a_age_fit<- lm(a_diff ~ age, analysisDat2)
summary(a_age_fit)
# Call:
#   lm(formula = a_diff ~ age, data = analysisDat2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.065385 -0.016463  0.000243  0.015280  0.079543 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -0.0146016  0.0097795  -1.493   0.1380  
# age          0.0019480  0.0008695   2.240   0.0269 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02728 on 121 degrees of freedom
# (9 observations deleted due to missingness)
# Multiple R-squared:  0.03983,	Adjusted R-squared:  0.0319 
# F-statistic:  5.02 on 1 and 121 DF,  p-value: 0.02689


#regression model ter
ter_age_fit<- lm(ter_diff ~ age, analysisDat2)
summary(ter_age_fit)

#regression model p
p_age_fit<- lm(p_diff ~ age, analysisDat2)
summary(p_age_fit)

# Call:
#   lm(formula = p_diff ~ age, data = analysisDat2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.118846 -0.031763  0.002321  0.031909  0.122782 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.013270   0.018915   0.702  0.48431   
# age         -0.005133   0.001694  -3.029  0.00301 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.05247 on 118 degrees of freedom
# (12 observations deleted due to missingness)
# Multiple R-squared:  0.07216,	Adjusted R-squared:  0.0643 
# F-statistic: 9.178 on 1 and 118 DF,  p-value: 0.003011

# #regression model rd
# rd_age_fit<- lm(rd_diff  ~ age, analysisDat2)
# summary(rd_age_fit)
# 
# #regression model sda
# sda_age_fit<- lm(sda_diff  ~ age, analysisDat2)
# summary(sda_age_fit)

#regression model sda/rd
sdard_age_fit<- lm(sda_rd_ratio_diff ~ age, analysisDat2)
summary(sdard_age_fit)



################ Analyses for SCARED_total_Ch_PRE #############################

#regression model a
a_ch_total_age_fit<- lm(a_diff ~ SCARED_total_Ch_PRE + age + SCARED_total_Ch_PRE*age, analysisDat2)
summary(a_ch_total_age_fit)

#regression model ter
ter_ch_total_age_fit<- lm(ter_diff ~ SCARED_total_Ch_PRE + age + SCARED_total_Ch_PRE*age, analysisDat2)
summary(ter_ch_total_age_fit)

#regression model p
p_ch_total_age_fit<- lm(p_diff ~ SCARED_total_Ch_PRE + age + SCARED_total_Ch_PRE*age, analysisDat2)
summary(p_ch_total_age_fit)

# #regression model rd
# rd_ch_total_age_fit<- lm(rd_diff ~ SCARED_total_Ch_PRE + age + SCARED_total_Ch_PRE*age, analysisDat2)
# summary(rd_ch_total_age_fit)
# 
# #regression model sda
# sda_ch_total_age_fit<- lm(sda_diff ~ SCARED_total_Ch_PRE + age + SCARED_total_Ch_PRE*age, analysisDat2)
# summary(sda_ch_total_age_fit)

#regression model sda/rd
sdard_ch_total_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_total_Ch_PRE + age + SCARED_total_Ch_PRE*age, analysisDat2)
summary(sdard_ch_total_age_fit)




################ Analyses for SCARED_panic_Ch_PRE #############################

#regression model a
a_ch_panic_age_fit<- lm(a_diff ~ SCARED_panic_Ch_PRE + age + SCARED_panic_Ch_PRE*age, analysisDat2)
summary(a_ch_panic_age_fit)
# # Call:
# lm(formula = a_diff ~ SCARED_panic_Ch_PRE + age + SCARED_panic_Ch_PRE * 
#      age, data = analysisDat2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.062908 -0.019421 -0.000536  0.016288  0.086477 
# 
# Coefficients:
#                             Estimate  Std. Error t value Pr(>|t|)   
#   (Intercept)             -0.0362979   0.0160887  -2.256  0.02595 * 
#   SCARED_panic_Ch_PRE      0.0018473   0.0015619   1.183  0.23935   
#   age                      0.0042730   0.0014724   2.902  0.00444 **
#   SCARED_panic_Ch_PRE:age -0.0002116   0.0001307  -1.619  0.10824   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02717 on 115 degrees of freedom
# (13 observations deleted due to missingness)
# Multiple R-squared:  0.08127,	Adjusted R-squared:  0.0573 
# F-statistic: 3.391 on 3 and 115 DF,  p-value: 0.02044

#regression model ter
ter_ch_panic_age_fit<- lm(ter_diff ~ SCARED_panic_Ch_PRE + age + SCARED_panic_Ch_PRE*age, analysisDat2)
summary(ter_ch_panic_age_fit)

#regression model p
p_ch_panic_age_fit<- lm(p_diff ~ SCARED_panic_Ch_PRE + age + SCARED_panic_Ch_PRE*age, analysisDat2)
summary(p_ch_panic_age_fit)

# Call:
#   lm(formula = p_diff ~ SCARED_panic_Ch_PRE + age + SCARED_panic_Ch_PRE * 
#        age, data = analysisDat2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.120315 -0.033867 -0.000201  0.033029  0.123596 
# 
# Coefficients:
#                             Estimate Std. Error t value Pr(>|t|)  
#   (Intercept)              0.0280250  0.0317867   0.882   0.3799  
#   SCARED_panic_Ch_PRE     -0.0021680  0.0030529  -0.710   0.4791  
#   age                     -0.0064612  0.0029638  -2.180   0.0313 *
#   SCARED_panic_Ch_PRE:age  0.0001754  0.0002583   0.679   0.4985  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.05327 on 112 degrees of freedom
# (16 observations deleted due to missingness)
# Multiple R-squared:  0.07242,	Adjusted R-squared:  0.04758 
# F-statistic: 2.915 on 3 and 112 DF,  p-value: 0.03745


# #regression model rd
# rd_ch_panic_age_fit<- lm(rd_diff ~ SCARED_panic_Ch_PRE + age + SCARED_panic_Ch_PRE*age, analysisDat2)
# summary(rd_panic_age_fit)
# 
# #regression model sda
# sda_ch_panic_age_fit<- lm(sda_diff ~ SCARED_panic_Ch_PRE + age + SCARED_panic_Ch_PRE*age, analysisDat2)
# summary(sda_panic_age_fit)

#regression model sda/rd
sdard_ch_panic_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_panic_Ch_PRE + age + SCARED_panic_Ch_PRE*age, analysisDat2)
summary(sdard_ch_panic_age_fit)




################ Analyses for SCARED_GAD_Ch_PRE #############################

#regression model a
a_ch_GAD_age_fit<- lm(a_diff ~ SCARED_GAD_Ch_PRE + age + SCARED_GAD_Ch_PRE*age, analysisDat2)
summary(a_ch_GAD_age_fit)

# Call:
#   lm(formula = a_diff ~ SCARED_GAD_Ch_PRE + age + SCARED_GAD_Ch_PRE * 
#        age, data = analysisDat2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.062182 -0.016107 -0.001572  0.016468  0.082114 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)  
#   (Intercept)           -0.0353074  0.0192310  -1.836   0.0689 .
#   SCARED_GAD_Ch_PRE      0.0021548  0.0020313   1.061   0.2910  
#   age                    0.0039479  0.0017896   2.206   0.0294 *
#   SCARED_GAD_Ch_PRE:age -0.0002067  0.0001777  -1.163   0.2471  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02752 on 115 degrees of freedom
# (13 observations deleted due to missingness)
# Multiple R-squared:  0.05786,	Adjusted R-squared:  0.03328 
# F-statistic: 2.354 on 3 and 115 DF,  p-value: 0.07572

#regression model ter
ter_ch_GAD_age_fit<- lm(ter_diff ~ SCARED_GAD_Ch_PRE + age + SCARED_GAD_Ch_PRE*age, analysisDat2)
summary(ter_ch_GAD_age_fit)

#regression model p
p_ch_GAD_age_fit<- lm(p_diff ~ SCARED_GAD_Ch_PRE + age + SCARED_GAD_Ch_PRE*age, analysisDat2)
summary(p_ch_GAD_age_fit)

# #regression model rd
# rd_ch_GAD_age_fit<- lm(rd_diff ~ SCARED_GAD_Ch_PRE + age + SCARED_GAD_Ch_PRE*age, analysisDat2)
# summary(rd_ch_GAD_age_fit)
# 
# #regression model sda
# sda_ch_GAD_age_fit<- lm(sda_diff ~ SCARED_GAD_Ch_PRE + age + SCARED_GAD_Ch_PRE*age, analysisDat2)
# summary(sda_ch_GAD_age_fit)

#regression model sda/rd
sdard_ch_GAD_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_GAD_Ch_PRE + age + SCARED_GAD_Ch_PRE*age, analysisDat2)
summary(sdard_ch_GAD_age_fit)



################ Analyses for SCARED_separation_Ch_PRE #############################

#regression model a
a_ch_separation_age_fit<- lm(a_diff ~ SCARED_separation_Ch_PRE + age + SCARED_separation_Ch_PRE*age, analysisDat2)
summary(a_ch_separation_age_fit)

#regression model ter
ter_ch_separation_age_fit<- lm(ter_diff ~ SCARED_separation_Ch_PRE + age + SCARED_separation_Ch_PRE*age, analysisDat2)
summary(ter_ch_separation_age_fit)

# Call:
#   lm(formula = ter_diff ~ SCARED_separation_Ch_PRE + age + SCARED_separation_Ch_PRE * 
#        age, data = analysisDat2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.217745 -0.041342 -0.004123  0.029607  0.198632 
# 
# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)  
#   (Intercept)                  -0.1055332  0.0487829  -2.163   0.0326 *
#   SCARED_separation_Ch_PRE      0.0107209  0.0058375   1.837   0.0688 .
#   age                           0.0090253  0.0040410   2.233   0.0274 *
#   SCARED_separation_Ch_PRE:age -0.0008305  0.0005183  -1.602   0.1118  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0665 on 116 degrees of freedom
# (12 observations deleted due to missingness)
# Multiple R-squared:  0.04595,	Adjusted R-squared:  0.02127 
# F-statistic: 1.862 on 3 and 116 DF,  p-value: 0.1399

#regression model p
p_ch_separation_age_fit<- lm(p_diff ~ SCARED_separation_Ch_PRE + age + SCARED_separation_Ch_PRE*age, analysisDat2)
summary(p_ch_separation_age_fit)

# #regression model rd
# rd_ch_separation_age_fit<- lm(rd_diff ~ SCARED_separation_Ch_PRE + age + SCARED_separation_Ch_PRE*age, analysisDat2)
# summary(rd_ch_separation_age_fit)
# 
# #regression model sda
# sda_ch_separation_age_fit<- lm(sda_diff ~ SCARED_separation_Ch_PRE + age + SCARED_separation_Ch_PRE*age, analysisDat2)
# summary(sda_ch_separation_age_fit)

#regression model sda/rd
sdard_ch_separation_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_separation_Ch_PRE + age + SCARED_separation_Ch_PRE*age, analysisDat2)
summary(sdard_ch_separation_age_fit)



################ Analyses for SCARED_SAD_Ch_PRE #############################

#regression model a
a_ch_SAD_age_fit<- lm(a_diff ~ SCARED_SAD_Ch_PRE + age + SCARED_SAD_Ch_PRE*age, analysisDat2)
summary(a_ch_SAD_age_fit)

#regression model ter
ter_ch_SAD_age_fit<- lm(ter_diff ~ SCARED_SAD_Ch_PRE + age + SCARED_SAD_Ch_PRE*age, analysisDat2)
summary(ter_ch_SAD_age_fit)

#regression model p
p_ch_SAD_age_fit<- lm(p_diff ~ SCARED_SAD_Ch_PRE + age + SCARED_SAD_Ch_PRE*age, analysisDat2)
summary(p_ch_SAD_age_fit)

# #regression model rd
# rd_ch_SAD_age_fit<- lm(rd_diff ~ SCARED_SAD_Ch_PRE + age + SCARED_SAD_Ch_PRE*age, analysisDat2)
# summary(rd_ch_SAD_age_fit)
# 
# #regression model sda
# sda_ch_SAD_age_fit<- lm(sda_diff ~ SCARED_SAD_Ch_PRE + age + SCARED_SAD_Ch_PRE*age, analysisDat2)
# summary(sda_ch_SAD_age_fit)

#regression model sda/rd
sdard_ch_SAD_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_SAD_Ch_PRE + age + SCARED_SAD_Ch_PRE*age, analysisDat2)
summary(sdard_ch_SAD_age_fit)

# Call:
#   lm(formula = sda_rd_ratio_diff ~ SCARED_SAD_Ch_PRE + age + SCARED_SAD_Ch_PRE * 
#        age, data = analysisDat2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -736.40 -225.58 -121.70   25.62 1918.18 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)  
#   (Intercept)           -421.102    387.617  -1.086   0.2797  
#   SCARED_SAD_Ch_PRE       87.717     43.437   2.019   0.0459 *
#   age                     41.702     34.759   1.200   0.2329  
#   SCARED_SAD_Ch_PRE:age   -6.571      3.807  -1.726   0.0871 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 455.6 on 108 degrees of freedom
# (20 observations deleted due to missingness)
# Multiple R-squared:  0.05003,	Adjusted R-squared:  0.02364 
# F-statistic: 1.896 on 3 and 108 DF,  p-value: 0.1346


################ Analyses for SCARED_school_Ch_PRE #############################

#regression model a
a_ch_school_age_fit<- lm(a_diff ~ SCARED_school_Ch_PRE + age + SCARED_school_Ch_PRE*age, analysisDat2)
summary(a_ch_school_age_fit)

# Call:
#   lm(formula = a_diff ~ SCARED_school_Ch_PRE + age + SCARED_school_Ch_PRE * 
#        age, data = analysisDat2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.05937 -0.01880  0.00121  0.01590  0.08279 
# 
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)   
#   (Intercept)              -0.0400034  0.0159089  -2.515  0.01330 * 
#   SCARED_school_Ch_PRE      0.0073707  0.0042086   1.751  0.08255 . 
#   age                       0.0043710  0.0014186   3.081  0.00258 **
#   SCARED_school_Ch_PRE:age -0.0007104  0.0003573  -1.988  0.04917 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.02717 on 115 degrees of freedom
# (13 observations deleted due to missingness)
# Multiple R-squared:  0.08131,	Adjusted R-squared:  0.05735 
# F-statistic: 3.393 on 3 and 115 DF,  p-value: 0.02039

#regression model ter
ter_ch_school_age_fit<- lm(ter_diff ~ SCARED_school_Ch_PRE + age + SCARED_school_Ch_PRE*age, analysisDat2)
summary(ter_ch_school_age_fit)

#regression model p
p_ch_school_age_fit<- lm(p_diff ~ SCARED_school_Ch_PRE + age + SCARED_school_Ch_PRE*age, analysisDat2)
summary(p_ch_school_age_fit)

# #regression model rd
# rd_ch_school_age_fit<- lm(rd_diff ~ SCARED_school_Ch_PRE + age + SCARED_school_Ch_PRE*age, analysisDat2)
# summary(rd_ch_school_age_fit)
# 
# #regression model sda
# sda_ch_school_age_fit<- lm(sda_diff ~ SCARED_school_Ch_PRE + age + SCARED_school_Ch_PRE*age, analysisDat2)
# summary(sda_ch_school_age_fit)

#regression model sda/rd
sdard_ch_school_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_school_Ch_PRE + age + SCARED_school_Ch_PRE*age, analysisDat2)
summary(sdard_ch_school_age_fit)


################ Analyses for SCARED_total_Par_PRE #############################

#regression model a
a_par_total_age_fit<- lm(a_diff ~ SCARED_total_Par_PRE + age + SCARED_total_Par_PRE*age, analysisDat2)
summary(a_par_total_age_fit)

#regression model ter
ter_par_total_age_fit<- lm(ter_diff ~ SCARED_total_Par_PRE + age + SCARED_total_Par_PRE*age, analysisDat2)
summary(ter_par_total_age_fit)

#regression model p
p_par_total_age_fit<- lm(p_diff ~ SCARED_total_Par_PRE + age + SCARED_total_Par_PRE*age, analysisDat2)
summary(p_par_total_age_fit)

#regression model sda/rd
sdard_par_total_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_total_Par_PRE + age + SCARED_total_Par_PRE*age, analysisDat2)
summary(sdard_par_total_age_fit)


################ Analyses for SCARED_SAD_Par_PRE #############################

#regression model a
a_par_SAD_age_fit<- lm(a_diff ~ SCARED_SAD_par_PRE + age + SCARED_SAD_par_PRE*age, analysisDat2)
summary(a_par_SAD_age_fit)

#regression model ter
ter_par_SAD_age_fit<- lm(ter_diff ~ SCARED_SAD_par_PRE + age + SCARED_SAD_par_PRE*age, analysisDat2)
summary(ter_par_SAD_age_fit)

#regression model p
p_par_SAD_age_fit<- lm(p_diff ~ SCARED_SAD_par_PRE + age + SCARED_SAD_par_PRE*age, analysisDat2)
summary(p_par_SAD_age_fit)

#regression model sda/rd
sdard_par_SAD_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_SAD_par_PRE + age + SCARED_SAD_par_PRE*age, analysisDat2)
summary(sdard_par_SAD_age_fit)

################ Analyses for SCARED_GAD_Par_PRE #############################

#regression model a
a_par_GAD_age_fit<- lm(a_diff ~ SCARED_GAD_par_PRE + age + SCARED_GAD_par_PRE*age, analysisDat2)
summary(a_par_GAD_age_fit)

#regression model ter
ter_par_GAD_age_fit<- lm(ter_diff ~ SCARED_GAD_par_PRE + age + SCARED_GAD_par_PRE*age, analysisDat2)
summary(ter_par_GAD_age_fit)

#regression model p
p_par_GAD_age_fit<- lm(p_diff ~ SCARED_GAD_par_PRE + age + SCARED_GAD_par_PRE*age, analysisDat2)
summary(p_par_GAD_age_fit)

#regression model sda/rd
sdard_par_GAD_age_fit<- lm(sda_rd_ratio_diff ~ SCARED_GAD_par_PRE + age + SCARED_GAD_par_PRE*age, analysisDat2)
summary(sdard_par_GAD_age_fit)
