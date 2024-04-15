
####### START ######

setwd()

# Import DDM output
ddmOutput <- read.csv("ddm_output_data.csv")

#Create sublist
# verify that there is the expected number of subjects
subList <- unique(ddmOutput$subject)



############ Reject anyone with less than 20 trials in post-error or post-correct ###############

#read in the cleaned pre-ddm sheet
clean_pre_ddm_df <- read.csv("clean_flanker_data.csv")


# Add a column for number of trials to ddmOutput
ddmOutput$numTrials <- NA

for (i in 1:length(subList)){
  
  #make a df where we only look at the i-th sub
  singlesubDat <- subset(clean_pre_ddm_df, clean_pre_ddm_df$subject==subList[i])
  
  #calculate number of post-errors (100)
  numPostError <- nrow(singlesubDat[singlesubDat$priorAccuracy == 100, ])
  #calculate number of post-corrects (111)
  numPostCorrect <- nrow(singlesubDat[singlesubDat$priorAccuracy == 111, ])
  
  # when subject in ddmoutput is the one specified in the loop, and whichcondition in ddmOutput is 100, put numPostError (defined above) in it
  ddmOutput$numTrials[subList[i]==ddmOutput$subject & ddmOutput$whichCondition==100] <- numPostError
  # when subject in ddmoutput is the one specified in the loop, and whichcondition in ddmOutput is 111, put numPostCorrect (defined above) in it
  ddmOutput$numTrials[subList[i]==ddmOutput$subject & ddmOutput$whichCondition==111] <- numPostCorrect

}

# Put NAs for people with too few trials in each condition- people with too few trials might already
# be filtered out based on which cleaning script was used. 
ddmOutput$numTrials[ddmOutput$numTrials<20] <-NA


############# NA bad fitstats & iternum ###################


#put an NA for error fitstats that are above 200 
ddmOutput$fitStat[ddmOutput$fitStat>200] <-NA

# rejecting anyone with a ceiling number of iterations
ddmOutput$iterNum[ddmOutput$iterNum==200] <-NA


############ Create a new df that's more conducive to subtraction ###################


#initialize the df
analysisDat<-  data.frame(matrix(ncol=26, nrow=0))
colnames(analysisDat) <- c("subject","a_err", "a_corr", "a_diff", "ter_err", "ter_corr", "ter_diff", 
                           "p_err", "p_corr", "p_diff", "rd_err", "rd_corr", "rd_diff", 
                           "sda_err", "sda_corr", "sda_diff", "sda_rd_ratio_err", "sda_rd_ratio_corr", "sda_rd_ratio_diff",
                           "fitStat_err", "fitStat_corr", "fitStat_diff", "iterNum_err", "iterNum_corr",  
                           "numTrials_postErr", "numTrials_postCorr")

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
  numTrials_postErr <- ddmOutput$numTrials[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==100]
  numTrials_postCorr <- ddmOutput$numTrials[ddmOutput$subject == subList[i] & ddmOutput$whichCondition ==111]
  
  # make new row with values created above
  newRow <- data.frame(subject, a_err, a_corr, a_diff, ter_err, ter_corr, ter_diff, p_err, p_corr, p_diff, rd_err, rd_corr, rd_diff, 
                       sda_err, sda_corr, sda_diff, sda_rd_ratio_err, sda_rd_ratio_corr, sda_rd_ratio_diff,
                       fitStat_err, fitStat_corr, fitStat_diff, iterNum_err, iterNum_corr,  
                       numTrials_postErr, numTrials_postCorr)
  colnames(newRow)<- c("subject","a_err", "a_corr", "a_diff", "ter_err", "ter_corr", "ter_diff", 
                       "p_err", "p_corr", "p_diff", "rd_err", "rd_corr", "rd_diff", 
                       "sda_err", "sda_corr", "sda_diff", "sda_rd_ratio_err", "sda_rd_ratio_corr", "sda_rd_ratio_diff",
                       "fitStat_err", "fitStat_corr", "fitStat_diff", "iterNum_err", "iterNum_corr",
                       "numTrials_postErr", "numTrials_postCorr")
  
  # add row to the df
  analysisDat <- rbind(analysisDat, newRow)
  
}




############# add in SCARED, age, stim_med, gender, CDI ###################

collectiveDat <- read.csv("survey_data.csv")

subList <- unique(analysisDat$subject)

#new column for child age
analysisDat[ , "age"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting age scores on comboDat
  analysisDat$age[analysisDat$subject== subList[i]] <- collectiveDat$age[collectiveDat$subject== subList[i]]
}

#new column for child gender
analysisDat[ , "chgender"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting gender  on comboDat
  analysisDat$chgender[analysisDat$subject== subList[i]] <- collectiveDat$chgender[collectiveDat$subject== subList[i]]
}

# SCARED child data
analysisDat[ , c("SCARED_01_Ch_PRE","SCARED_02_Ch_PRE","SCARED_03_Ch_PRE","SCARED_04_Ch_PRE","SCARED_05_Ch_PRE","SCARED_06_Ch_PRE","SCARED_07_Ch_PRE","SCARED_08_Ch_PRE","SCARED_09_Ch_PRE","SCARED_10_Ch_PRE","SCARED_11_Ch_PRE","SCARED_12_Ch_PRE","SCARED_13_Ch_PRE","SCARED_14_Ch_PRE","SCARED_15_Ch_PRE","SCARED_16_Ch_PRE","SCARED_17_Ch_PRE","SCARED_18_Ch_PRE","SCARED_19_Ch_PRE","SCARED_20_Ch_PRE","SCARED_21_Ch_PRE","SCARED_22_Ch_PRE","SCARED_23_Ch_PRE","SCARED_24_Ch_PRE","SCARED_25_Ch_PRE","SCARED_26_Ch_PRE","SCARED_27_Ch_PRE","SCARED_28_Ch_PRE","SCARED_29_Ch_PRE","SCARED_30_Ch_PRE","SCARED_31_Ch_PRE","SCARED_32_Ch_PRE","SCARED_33_Ch_PRE","SCARED_34_Ch_PRE","SCARED_35_Ch_PRE","SCARED_36_Ch_PRE","SCARED_37_Ch_PRE","SCARED_38_Ch_PRE","SCARED_39_Ch_PRE","SCARED_40_Ch_PRE","SCARED_41_Ch_PRE")] <- NA
for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat[analysisDat$subject== subList[i], c(29:69)] <- collectiveDat[collectiveDat$subject== subList[i], c(3:43)]
}

#new column for child SCARED
analysisDat[ , "SCARED_total_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat$SCARED_total_Ch_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_total_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED GAD
analysisDat[ , "SCARED_GAD_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat$SCARED_GAD_Ch_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_GAD_Ch_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SAD separation
analysisDat[ , "SCARED_SAD_Ch_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat$SCARED_SAD_Ch_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_SAD_Ch_PRE[collectiveDat$subject==subList[i]]
}

# SCARED parent data
analysisDat[ , c("SCARED_01_Par_PRE","SCARED_02_Par_PRE","SCARED_03_Par_PRE","SCARED_04_Par_PRE","SCARED_05_Par_PRE","SCARED_06_Par_PRE","SCARED_07_Par_PRE","SCARED_08_Par_PRE","SCARED_09_Par_PRE","SCARED_10_Par_PRE","SCARED_11_Par_PRE","SCARED_12_Par_PRE","SCARED_13_Par_PRE","SCARED_14_Par_PRE","SCARED_15_Par_PRE","SCARED_16_Par_PRE","SCARED_17_Par_PRE","SCARED_18_Par_PRE","SCARED_19_Par_PRE","SCARED_20_Par_PRE","SCARED_21_Par_PRE","SCARED_22_Par_PRE","SCARED_23_Par_PRE","SCARED_24_Par_PRE","SCARED_25_Par_PRE","SCARED_26_Par_PRE","SCARED_27_Par_PRE","SCARED_28_Par_PRE","SCARED_29_Par_PRE","SCARED_30_Par_PRE","SCARED_31_Par_PRE","SCARED_32_Par_PRE","SCARED_33_Par_PRE","SCARED_34_Par_PRE","SCARED_35_Par_PRE","SCARED_36_Par_PRE","SCARED_37_Par_PRE","SCARED_38_Par_PRE","SCARED_39_Par_PRE","SCARED_40_Par_PRE","SCARED_41_Par_PRE")] <- NA
for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat[analysisDat$subject== subList[i], c(73:113)] <- collectiveDat[collectiveDat$subject== subList[i], c(50:90)]
}


#new column for parent SCARED
analysisDat[ , "SCARED_total_Par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat$SCARED_total_Par_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_total_Par_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SCARED GAD
analysisDat[ , "SCARED_GAD_Par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat$SCARED_GAD_Par_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_GAD_Par_PRE[collectiveDat$subject==subList[i]]
}

#new column for child SAD separation
analysisDat[ , "SCARED_SAD_Par_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat$SCARED_SAD_Par_PRE[analysisDat$subject==subList[i]] <- collectiveDat$SCARED_SAD_Par_PRE[collectiveDat$subject==subList[i]]
}


# ACS data
analysisDat[ , c("ACS_total_Ch_PRE_15item",	"ACS_attnFocus_Ch_PRE_15item",	"ACS_attnShift_Ch_PRE_15item")] <- NA
for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting ACS scores on comboDat
  analysisDat[analysisDat$subject== subList[i], c(117:119)] <- collectiveDat[collectiveDat$subject== subList[i], c(133:135)]
}

#new column for stimulant medications
analysisDat[ , "stim_meds_PRE"] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting med scores on comboDat
  analysisDat$stim_meds_PRE[analysisDat$subject==subList[i]] <- collectiveDat$stim_meds_PRE[collectiveDat$subject==subList[i]]
}

#new column for diagnoses
analysisDat[ , c("finaldx1_PRE",	"finaldx2_PRE",	"finaldx3_PRE",	"finaldx4_PRE",	"finaldx5_PRE",	"finaldx6_PRE")] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat$finaldx1_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx1_PRE[collectiveDat$subject==subList[i]]
 
  analysisDat$finaldx2_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx2_PRE[collectiveDat$subject==subList[i]]
 
  analysisDat$finaldx3_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx3_PRE[collectiveDat$subject==subList[i]]
  
  analysisDat$finaldx4_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx4_PRE[collectiveDat$subject==subList[i]]
  
  analysisDat$finaldx5_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx5_PRE[collectiveDat$subject==subList[i]]
  
  analysisDat$finaldx6_PRE[analysisDat$subject==subList[i]] <- collectiveDat$finaldx6_PRE[collectiveDat$subject==subList[i]]
}


# read in csv for Acc and RT
RT_Acc_df <- read.csv("RT_Acc_df_for_clean_flanker_data.csv")

#new columns for acc RT measures
analysisDat[ , c("cong_Acc_avg", "incong_Acc_avg", 
                  "corr_cong_RT_avg", "corr_incong_RT_avg","err_cong_RT_avg","err_incong_RT_avg",
                  "num_corr_cong_trials","num_corr_incong_trials", "num_err_cong_trials", "num_err_incong_trials")] <- NA

for (i in 1:length(subList)){
  #matching sub numbers on comboDat and collectiveDat and putting SCARED scores on comboDat
  analysisDat[analysisDat$subject==subList[i], c(127:136)] <- RT_Acc_df[RT_Acc_df$subject == subList[i],c(2:11)]
}

analysisDat1<- analysisDat

################# Recode gender and stim_meds #######################

library("plyr")
# original gender is male == 1 and female == 2. This will code it so male == -1 and female == 1
analysisDat1$chgender <- mapvalues(analysisDat1$chgender, from = c(1,2,NA), to = c(-1,1,NA))

# original stim_med_PRE is "not on stimulants" == 0 and "taking stimulants" == 1. This will code 
# it so "not on stimulants" == -1 and "taking stimulants" == 1
analysisDat1$stim_meds_PRE <- mapvalues(analysisDat1$stim_meds_PRE, from = c(0,1,NA), to = c(-1,1,NA))


################# Reject based on age #######################

subList<- unique(analysisDat1$subject)

# remove people under 7 and over 17

for(i in 1:length(subList)){
  if(analysisDat1$age[analysisDat1$subject==subList[i]]<7 | analysisDat1$age[analysisDat1$subject==subList[i]]>17){
    analysisDat1 <- subset(analysisDat1, analysisDat1$subject!=subList[i])
  }
}

######## Remove incomplete flanker file #########

rawFlankerLength <- read.csv("rawFlankerLength.csv")

for(i in 1:length(subList)){
  if(rawFlankerLength$flankerLength[rawFlankerLength$subject==subList[i]]!=352){
    analysisDat1 <- subset(analysisDat1, analysisDat1$subject!=subList[i])
  }
}



####### Reject all parms for condition #####

#placeholder
analysisDat2 <- analysisDat1

subList<- unique(analysisDat2$subject)


# If a person has NAs for numTrials (lower than 20), NAs for fitStat (greater than 200), or NAs for iterNum (ceiling value of 200),
# then their post-error parms or post-correct parms are marked with NA.
# Ex. numTrials_postErr is lower than 20, meaning that all of the post-error parms are invalid since there's not enough trials to get a good fit.
# Since you fit post error and post correct separately, they could still have valid parms for one condition but not the other
# Included column 22 (fitStat_diff) because the diff was calculated above, meaning that the diff would 
# remain even tho the post-err/corr fitStat was labeled with NA for low numTrials or ceiling iterNum.
for (i in 1:length(subList)){
  
  # if number of trials for post-err is NA, fitStat_err is NA, or iterNum_err is ceiling (200), then all parms for post-error are NA
  if(is.na(analysisDat2$numTrials_postErr[analysisDat2$subject==subList[i]]) | is.na(analysisDat2$fitStat_err[analysisDat2$subject==subList[i]]) | is.na(analysisDat2$iterNum_err[analysisDat2$subject==subList[i]])){
    analysisDat2[analysisDat2$subject==subList[i],c(2,5,8,11,14,17,20,22)] <- NA
  }
  
  # if number of trials for post-corr is NA, fitStat_corr is NA, or iterNum_corr is ceiling (200), then all parms for post-error are NA
  if(is.na(analysisDat2$numTrials_postCorr[analysisDat2$subject==subList[i]]) | is.na(analysisDat2$fitStat_corr[analysisDat2$subject==subList[i]]) | is.na(analysisDat2$iterNum_corr[analysisDat2$subject==subList[i]])){
    analysisDat2[analysisDat2$subject==subList[i],c(3,6,9,12,15,18,21,22)] <- NA
  }
}


################# Ter- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "ter" for post-error. 
ter_meanminus3sd_err <- mean(analysisDat2$ter_err, na.rm=TRUE)- 3*sd(analysisDat2$ter_err, na.rm=TRUE)
ter_meanplus3sd_err <- mean(analysisDat2$ter_err, na.rm=TRUE)+ 3*sd(analysisDat2$ter_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$ter_err))
# mark NA for any outliers in ter for post-error
analysisDat2$ter_err[analysisDat2$ter_err<ter_meanminus3sd_err | analysisDat2$ter_err>ter_meanplus3sd_err] <-NA


## calculate the SD of "ter" for post-correct
ter_meanminus3sd_corr <- mean(analysisDat2$ter_corr, na.rm=TRUE)- 3*sd(analysisDat2$ter_corr, na.rm=TRUE)
ter_meanplus3sd_corr <- mean(analysisDat2$ter_corr, na.rm=TRUE)+ 3*sd(analysisDat2$ter_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$ter_corr))
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
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$a_err))
# mark NA for any outliers in a for post-error
analysisDat2$a_err[analysisDat2$a_err<a_meanminus3sd_err | analysisDat2$a_err>a_meanplus3sd_err] <-NA
table(is.na(analysisDat2$a_err))


## calculate the SD of "a" for post-correct
a_meanminus3sd_corr <- mean(analysisDat2$a_corr, na.rm=TRUE)- 3*sd(analysisDat2$a_corr, na.rm=TRUE)
a_meanplus3sd_corr <- mean(analysisDat2$a_corr, na.rm=TRUE)+ 3*sd(analysisDat2$a_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$a_corr))
# mark NA for any outliers in a for post-correct
analysisDat2$a_corr[analysisDat2$a_corr<a_meanminus3sd_corr | analysisDat2$a_corr>a_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$a_corr))


#doing the subtraction
analysisDat2$a_diff <- analysisDat2$a_err-analysisDat2$a_corr

#a subtraction outlier rejection
a_diff_meanminus3sd <- mean(analysisDat2$a_diff, na.rm=TRUE)- 3*sd(analysisDat2$a_diff, na.rm=TRUE)
a_diff_meanplus3sd <- mean(analysisDat2$a_diff, na.rm=TRUE)+ 3*sd(analysisDat2$a_diff, na.rm=TRUE)
table(is.na(analysisDat2$a_diff))
analysisDat2$a_diff[analysisDat2$a_diff<a_diff_meanminus3sd | analysisDat2$a_diff>a_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$a_diff))



################# p- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "p" for post-error. 
p_meanminus3sd_err <- mean(analysisDat2$p_err, na.rm=TRUE)- 3*sd(analysisDat2$p_err, na.rm=TRUE)
p_meanplus3sd_err <- mean(analysisDat2$p_err, na.rm=TRUE)+ 3*sd(analysisDat2$p_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$p_err))
# mark NA for any outliers in p for post-error
analysisDat2$p_err[analysisDat2$p_err<p_meanminus3sd_err | analysisDat2$p_err>p_meanplus3sd_err] <-NA
table(is.na(analysisDat2$p_err))


## calculate the SD of "p" for post-correct
p_meanminus3sd_corr <- mean(analysisDat2$p_corr, na.rm=TRUE)- 3*sd(analysisDat2$p_corr, na.rm=TRUE)
p_meanplus3sd_corr <- mean(analysisDat2$p_corr, na.rm=TRUE)+ 3*sd(analysisDat2$p_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$p_corr))
# mark NA for any outliers in p for post-correct
analysisDat2$p_corr[analysisDat2$p_corr<p_meanminus3sd_corr | analysisDat2$p_corr>p_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$p_corr))


#doing the subtraction
analysisDat2$p_diff <- analysisDat2$p_err-analysisDat2$p_corr

#p subtraction outlier rejection
p_diff_meanminus3sd <- mean(analysisDat2$p_diff, na.rm=TRUE)- 3*sd(analysisDat2$p_diff, na.rm=TRUE)
p_diff_meanplus3sd <- mean(analysisDat2$p_diff, na.rm=TRUE)+ 3*sd(analysisDat2$p_diff, na.rm=TRUE)
table(is.na(analysisDat2$p_diff))
analysisDat2$p_diff[analysisDat2$p_diff<p_diff_meanminus3sd | analysisDat2$p_diff>p_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$p_diff))



################# rd- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "rd" for post-error. 
rd_meanminus3sd_err <- mean(analysisDat2$rd_err, na.rm=TRUE)- 3*sd(analysisDat2$rd_err, na.rm=TRUE)
rd_meanplus3sd_err <- mean(analysisDat2$rd_err, na.rm=TRUE)+ 3*sd(analysisDat2$rd_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$rd_err))
# mark NA for any outliers in rd for post-error
analysisDat2$rd_err[analysisDat2$rd_err<rd_meanminus3sd_err | analysisDat2$rd_err>rd_meanplus3sd_err] <-NA
table(is.na(analysisDat2$rd_err))


## calculate the SD of "rd" for post-correct
rd_meanminus3sd_corr <- mean(analysisDat2$rd_corr, na.rm=TRUE)- 3*sd(analysisDat2$rd_corr, na.rm=TRUE)
rd_meanplus3sd_corr <- mean(analysisDat2$rd_corr, na.rm=TRUE)+ 3*sd(analysisDat2$rd_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$rd_corr))
# mark NA for any outliers in rd for post-correct
analysisDat2$rd_corr[analysisDat2$rd_corr<rd_meanminus3sd_corr | analysisDat2$rd_corr>rd_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$rd_corr))


#doing the subtraction
analysisDat2$rd_diff <- analysisDat2$rd_err-analysisDat2$rd_corr

#rd subtraction outlier rejection
rd_diff_meanminus3sd <- mean(analysisDat2$rd_diff, na.rm=TRUE)- 3*sd(analysisDat2$rd_diff, na.rm=TRUE)
rd_diff_meanplus3sd <- mean(analysisDat2$rd_diff, na.rm=TRUE)+ 3*sd(analysisDat2$rd_diff, na.rm=TRUE)
table(is.na(analysisDat2$rd_diff))
analysisDat2$rd_diff[analysisDat2$rd_diff<rd_diff_meanminus3sd | analysisDat2$rd_diff>rd_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$rd_diff))


################# sda- reject outliers, subtract, reject diff outliers ############

## calculate the outliers for "sda" for post-error. 
sda_meanminus3sd_err <- mean(analysisDat2$sda_err, na.rm=TRUE)- 3*sd(analysisDat2$sda_err, na.rm=TRUE)
sda_meanplus3sd_err <- mean(analysisDat2$sda_err, na.rm=TRUE)+ 3*sd(analysisDat2$sda_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$sda_err))
# mark NA for any outliers in sda for post-error
analysisDat2$sda_err[analysisDat2$sda_err<sda_meanminus3sd_err | analysisDat2$sda_err>sda_meanplus3sd_err] <-NA
table(is.na(analysisDat2$sda_err))


## calculate the SD of "sda" for post-correct
sda_meanminus3sd_corr <- mean(analysisDat2$sda_corr, na.rm=TRUE)- 3*sd(analysisDat2$sda_corr, na.rm=TRUE)
sda_meanplus3sd_corr <- mean(analysisDat2$sda_corr, na.rm=TRUE)+ 3*sd(analysisDat2$sda_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$sda_corr))
# mark NA for any outliers in sda for post-correct
analysisDat2$sda_corr[analysisDat2$sda_corr<sda_meanminus3sd_corr | analysisDat2$sda_corr>sda_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$sda_corr))


#doing the subtraction
analysisDat2$sda_diff <- analysisDat2$sda_err-analysisDat2$sda_corr

#sda subtraction outlier rejection
sda_diff_meanminus3sd <- mean(analysisDat2$sda_diff, na.rm=TRUE)- 3*sd(analysisDat2$sda_diff, na.rm=TRUE)
sda_diff_meanplus3sd <- mean(analysisDat2$sda_diff, na.rm=TRUE)+ 3*sd(analysisDat2$sda_diff, na.rm=TRUE)
table(is.na(analysisDat2$sda_diff))
analysisDat2$sda_diff[analysisDat2$sda_diff<sda_diff_meanminus3sd | analysisDat2$sda_diff>sda_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$sda_diff))


################# sda/rd- reject outliers, subtract, reject diff outliers ############

## calculate the ratio for error
analysisDat2$sda_rd_ratio_err <- (analysisDat2$sda_err)/(analysisDat2$rd_err)
## calculate the ratio for correct
analysisDat2$sda_rd_ratio_corr <- (analysisDat2$sda_corr)/(analysisDat2$rd_corr)


## calculate the outliers for "sda/rd" for post-error. 
sdard_meanminus3sd_err <- mean(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)
sdard_meanplus3sd_err <- mean(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_err, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$sda_rd_ratio_err))
# FALSE  TRUE 
# 97      51 
# mark NA for any outliers in ratio for post-error
analysisDat2$sda_rd_ratio_err[analysisDat2$sda_rd_ratio_err<sdard_meanminus3sd_err | analysisDat2$sda_rd_ratio_err>sdard_meanplus3sd_err] <-NA
table(is.na(analysisDat2$sda_rd_ratio_err))
# FALSE  TRUE 
# 95      53 

## calculate the SD of "sda/rd" for post-correct
sdard_meanminus3sd_corr <- mean(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)
sdard_meanplus3sd_corr <- mean(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE)
# table to mark how many were rejected- see difference before and after marking NA
table(is.na(analysisDat2$sda_rd_ratio_corr))
# mark NA for any outliers in ter for post-correct
analysisDat2$sda_rd_ratio_corr[analysisDat2$sda_rd_ratio_corr<sdard_meanminus3sd_corr | analysisDat2$sda_rd_ratio_corr>sdard_meanplus3sd_corr] <-NA
table(is.na(analysisDat2$sda_rd_ratio_corr))


#doing the subtraction
analysisDat2$sda_rd_ratio_diff <- analysisDat2$sda_rd_ratio_err-analysisDat2$sda_rd_ratio_corr

#sda/rd subtraction outlier rejection
sdard_diff_meanminus3sd <- mean(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)- 3*sd(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)
sdard_diff_meanplus3sd <- mean(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)+ 3*sd(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)
table(is.na(analysisDat2$sda_rd_ratio_corr))
analysisDat2$sda_rd_ratio_diff[analysisDat2$sda_rd_ratio_diff<sdard_diff_meanminus3sd | analysisDat2$sda_rd_ratio_diff>sdard_diff_meanplus3sd] <-NA
table(is.na(analysisDat2$sda_rd_ratio_corr))


# triple checking that all NA are actually NA
analysisDat2[analysisDat2=="NA"] <- NA


######## make dfs with outliers removed values #######

a_outlier_df <- data.frame(a_meanminus3sd_err,a_meanplus3sd_err,a_meanminus3sd_corr, a_meanplus3sd_corr, a_diff_meanminus3sd, a_diff_meanplus3sd)
p_outlier_df <- data.frame(p_meanminus3sd_err,p_meanplus3sd_err,p_meanminus3sd_corr, p_meanplus3sd_corr, p_diff_meanminus3sd, p_diff_meanplus3sd)
ter_outlier_df <- data.frame(ter_meanminus3sd_err,ter_meanplus3sd_err,ter_meanminus3sd_corr, ter_meanplus3sd_corr, ter_diff_meanminus3sd, ter_diff_meanplus3sd)
rd_outlier_df <- data.frame(rd_meanminus3sd_err,rd_meanplus3sd_err,rd_meanminus3sd_corr, rd_meanplus3sd_corr, rd_diff_meanminus3sd, rd_diff_meanplus3sd)
sda_outlier_df <- data.frame(sda_meanminus3sd_err,sda_meanplus3sd_err,sda_meanminus3sd_corr, sda_meanplus3sd_corr, sda_diff_meanminus3sd, sda_diff_meanplus3sd)
sdard_outlier_df <- data.frame(sdard_meanminus3sd_err,sdard_meanplus3sd_err,sdard_meanminus3sd_corr, sdard_meanplus3sd_corr, sdard_diff_meanminus3sd, sdard_diff_meanplus3sd)

a_mean_sd_df <- data.frame (a_means  = c(mean(analysisDat2$a_err, na.rm=TRUE), mean(analysisDat2$a_corr, na.rm=TRUE), mean(analysisDat2$a_diff, na.rm=TRUE)),
                           a_SDs = c(sd(analysisDat2$a_err, na.rm=TRUE), sd(analysisDat2$a_corr, na.rm=TRUE), sd(analysisDat2$a_diff, na.rm=TRUE))
)

p_mean_sd_df <- data.frame (p_means  = c(mean(analysisDat2$p_err, na.rm=TRUE), mean(analysisDat2$p_corr, na.rm=TRUE), mean(analysisDat2$p_diff, na.rm=TRUE)),
                            p_SDs = c(sd(analysisDat2$p_err, na.rm=TRUE), sd(analysisDat2$p_corr, na.rm=TRUE), sd(analysisDat2$p_diff, na.rm=TRUE))
)

ter_mean_sd_df <- data.frame (ter_means  = c(mean(analysisDat2$ter_err, na.rm=TRUE), mean(analysisDat2$ter_corr, na.rm=TRUE), mean(analysisDat2$ter_diff, na.rm=TRUE)),
                              ter_sd = c(sd(analysisDat2$ter_err, na.rm=TRUE), sd(analysisDat2$ter_corr, na.rm=TRUE), sd(analysisDat2$ter_diff, na.rm=TRUE))
)

rd_mean_sd_df <- data.frame (rd_means  = c(mean(analysisDat2$rd_err, na.rm=TRUE), mean(analysisDat2$rd_corr, na.rm=TRUE), mean(analysisDat2$rd_diff, na.rm=TRUE)),
                              rd_sd = c(sd(analysisDat2$rd_err, na.rm=TRUE), sd(analysisDat2$rd_corr, na.rm=TRUE), sd(analysisDat2$rd_diff, na.rm=TRUE))
)

sda_mean_sd_df <- data.frame (sda_means  = c(mean(analysisDat2$sda_err, na.rm=TRUE), mean(analysisDat2$sda_corr, na.rm=TRUE), mean(analysisDat2$sda_diff, na.rm=TRUE)),
                              sda_sd = c(sd(analysisDat2$sda_err, na.rm=TRUE), sd(analysisDat2$sda_corr, na.rm=TRUE), sd(analysisDat2$sda_diff, na.rm=TRUE))
)

sda_rd_ratio_mean_sd_df <- data.frame (sda_rd_ratio_means  = c(mean(analysisDat2$sda_rd_ratio_err, na.rm=TRUE), mean(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE), mean(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE)),
                              sda_rd_ratio_sd = c(sd(analysisDat2$sda_rd_ratio_err, na.rm=TRUE), sd(analysisDat2$sda_rd_ratio_corr, na.rm=TRUE), sd(analysisDat2$sda_rd_ratio_diff, na.rm=TRUE))
)

################ Converting wide data to long #############################

# in order to mixed effects models, the data needs to be long instead of wide. 

analysisDat3 <- analysisDat2

analysisDat3_long <- reshape(analysisDat3, idvar="subject", direction="long", 
        varying=list(a=c(2,3), ter=c(5,6), p=c(8,9), rd=c(11,12), sda=c(14,15), sda_rd_ratio=c(17,18)),
        # -1 is post-correct and 1 is post-error
        times=c('1', '-1'),
        v.names = c("a_measurement", "ter_measurement", "p_measurement", "rd_measurement", "sda_measurement", "sda_rd_ratio_measurement"))

#renaming the column called "time" to "PE_PC"
colnames(analysisDat3_long)[which(names(analysisDat3_long) == "time")] <- "PE_PC"

analysisDat3_long_unscaled <- analysisDat3_long
#converting the behavioral analyses into long format. This will be done in separate tables bc we do not run analyses with them together
# there are no PE_PC considerations (prior congruency, prior trial fast RT, or first trial of block) for these analyses
library("tidyverse")

# this one is for accuracy analyses based on congruency
analysisDat3_long_acc <- pivot_longer(analysisDat3, c(cong_Acc_avg,	incong_Acc_avg), names_to = "avgAcc_congruency",
                                      names_pattern = "(.*)_Acc_avg", values_to = "avgAcc_value")
# converting the names to binary (congruent == 1 and incongruent == -1)
analysisDat3_long_acc$avgAcc_congruency[analysisDat3_long_acc$avgAcc_congruency=="cong"] <- 1
analysisDat3_long_acc$avgAcc_congruency[analysisDat3_long_acc$avgAcc_congruency=="incong"] <- -1


# This one is for reaction time analyses based on correct/error and congruent/incongruent
analysisDat3_long_RT <- pivot_longer(analysisDat3, c(corr_cong_RT_avg,	corr_incong_RT_avg,	err_cong_RT_avg,	err_incong_RT_avg), names_to = c("avgRT_accuracy", "avgRT_congruency"),
                                      names_pattern = "(.*)_(.*)_RT_avg", values_to = "avgRT_value")
# converting the names to binary. Normally we would have congruent == 1 and incongruent == 2, and correct==1 and error==0,
#but for the analyses they need to be at congruent == 1 and incongruent == -1; correct==1 and error==-1
analysisDat3_long_RT$avgRT_congruency[analysisDat3_long_RT$avgRT_congruency=="cong"] <- 1
analysisDat3_long_RT$avgRT_congruency[analysisDat3_long_RT$avgRT_congruency=="incong"] <- -1
analysisDat3_long_RT$avgRT_accuracy[analysisDat3_long_RT$avgRT_accuracy=="corr"] <- 1
analysisDat3_long_RT$avgRT_accuracy[analysisDat3_long_RT$avgRT_accuracy=="err"] <- -1




#################### Prep for mixed effects models #############################


library(nlme)
library(emmeans)
library(ggplot2)
library(sjPlot)

# Factorize the categorical variables
analysisDat3_long$PE_PC <- as.factor(analysisDat3_long$PE_PC)
analysisDat3_long$chgender <- as.factor(analysisDat3_long$chgender)
analysisDat3_long$stim_meds_PRE <- as.factor(analysisDat3_long$stim_meds_PRE)

analysisDat3_long_acc$avgAcc_congruency <- as.factor(analysisDat3_long_acc$avgAcc_congruency)
analysisDat3_long_acc$chgender <- as.factor(analysisDat3_long_acc$chgender)
analysisDat3_long_acc$stim_meds_PRE <- as.factor(analysisDat3_long_acc$stim_meds_PRE)

analysisDat3_long_RT$avgRT_accuracy <- as.factor(analysisDat3_long_RT$avgRT_accuracy)
analysisDat3_long_RT$avgRT_congruency <- as.factor(analysisDat3_long_RT$avgRT_congruency)
analysisDat3_long_RT$chgender <- as.factor(analysisDat3_long_RT$chgender)
analysisDat3_long_RT$stim_meds_PRE <- as.factor(analysisDat3_long_RT$stim_meds_PRE)



# Assign the sum contrast codes
contrasts(analysisDat3_long$chgender) <- rev(contr.sum(2))
contrasts(analysisDat3_long$stim_meds_PRE) <- rev(contr.sum(2))

contrasts(analysisDat3_long_acc$avgAcc_congruency) <- rev(contr.sum(2))
contrasts(analysisDat3_long_acc$chgender) <- rev(contr.sum(2))
contrasts(analysisDat3_long_acc$stim_meds_PRE) <- rev(contr.sum(2))

contrasts(analysisDat3_long_RT$avgRT_accuracy) <- rev(contr.sum(2))
contrasts(analysisDat3_long_RT$avgRT_congruency) <- rev(contr.sum(2))
contrasts(analysisDat3_long_RT$chgender) <- rev(contr.sum(2))
contrasts(analysisDat3_long_RT$stim_meds_PRE) <- rev(contr.sum(2))

# Setting up to "reverse score" the sda/rd ratio
# This purely makes interpreting the graphs easier. the ratio represents attentional focus. previously it was 
# misleading/confusing because "more" attentional focus ends up being low on the y-axis (big ratio occurs due
# to big spotlight[sda] and small rate-of-spotlight-change[rd], which equals less attentional focus). Reversing
# it makes it so "more" attentional focus is high on the y-axis
analysisDat3_long$reverse_sda_rd_ratio_measurement <- analysisDat3_long$sda_rd_ratio_measurement

#scale and center (z score) all continuous vars)
#putting a c() around it keeps it numeric instead of changing to matrix
analysisDat3_long$SCARED_SAD_Ch_PRE <- c(scale(analysisDat3_long$SCARED_SAD_Ch_PRE , center = TRUE, scale = TRUE))
analysisDat3_long$SCARED_SAD_Par_PRE <- c(scale(analysisDat3_long$SCARED_SAD_Par_PRE , center = TRUE, scale = TRUE))
analysisDat3_long$age <- c(scale(analysisDat3_long$age , center = TRUE, scale = TRUE))
analysisDat3_long$a_measurement <- c(scale(analysisDat3_long$a_measurement , center = TRUE, scale = TRUE))
analysisDat3_long$p_measurement <- c(scale(analysisDat3_long$p_measurement , center = TRUE, scale = TRUE))
analysisDat3_long$ter_measurement <- c(scale(analysisDat3_long$ter_measurement , center = TRUE, scale = TRUE))
analysisDat3_long$sda_rd_ratio_measurement <- c(scale(analysisDat3_long$sda_rd_ratio_measurement , center = TRUE, scale = TRUE))
#ACS score/subscores child only
analysisDat3_long[,c(105,106,107)] <- lapply(analysisDat3_long[,c(105,106,107)], scale, center = TRUE, scale = TRUE)

# multiplying this by -1 because we want to reverse score the ratio so the graphs make more sense (see above 
# when the column is added).
analysisDat3_long$reverse_sda_rd_ratio_measurement <- -1*scale(analysisDat3_long$sda_rd_ratio_measurement , center = TRUE, scale = TRUE)


# doing this again for the analysisDat3_long_RT dataframe. 
analysisDat3_long_RT$age <- c(scale(analysisDat3_long_RT$age , center = TRUE, scale = TRUE))

# doing this again for the analysisDat3_long_acc dataframe. 
analysisDat3_long_acc$age <- c(scale(analysisDat3_long_acc$age , center = TRUE, scale = TRUE))



################# ch SAD:age:PE/PC- Mixed effects models ############


ratio_ch_SAD_age_fit<- lme(reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Ch_PRE*PE_PC*age +stim_meds_PRE 
                           + chgender, random = ~1 | subject, data = analysisDat3_long, na.action=na.omit)
summary(ratio_ch_SAD_age_fit)
# Fixed effects:  reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Ch_PRE * PE_PC *      age + stim_meds_PRE + chgender 
# Value  Std.Error  DF   t-value p-value
# (Intercept)                   0.2475373 0.10190522 118  2.429094  0.0166
# SCARED_SAD_Ch_PRE             0.0145939 0.08017592 118  0.182023  0.8559
# PE_PC1                       -0.4903979 0.12524021  75 -3.915659  0.0002 ***
# age                           0.0052435 0.08164372 118  0.064224  0.9489
# stim_meds_PRE1                0.0364249 0.08920824 118  0.408314  0.6838
# chgender1                     0.0010038 0.06338274 118  0.015837  0.9874
# SCARED_SAD_Ch_PRE:PE_PC1     -0.2750281 0.12343417  75 -2.228136  0.0289 *
# SCARED_SAD_Ch_PRE:age         0.0055223 0.08529643 118  0.064742  0.9485
# PE_PC1:age                    0.1439097 0.12649356  75  1.137684  0.2589
# SCARED_SAD_Ch_PRE:PE_PC1:age  0.2853462 0.13159515  75  2.168364  0.0333 *

# plotting
plot_model(ratio_ch_SAD_age_fit, type = "pred", terms = c("SCARED_SAD_Ch_PRE", "PE_PC", "age [-1,0,1]"), colors = c("dodgerblue3", "firebrick2"))
plot_model(ratio_ch_SAD_age_fit, type = "pred", terms = c("SCARED_SAD_Ch_PRE", "PE_PC", "age [-1,0,1]"), colors = "bw")


#simple slopes
library("reghelper")
simple_slopes(ratio_ch_SAD_age_fit, levels= list(PE_PC = c(1), SCARED_SAD_Ch_PRE= 'sstest', age= c(-1,0,1)))
# SCARED_SAD_Ch_PRE PE_PC age Test Estimate Std. Error t value  df  Pr(>|t|) Sig.
#   1            sstest     1  -1       -0.5513     0.1402 -3.9311 118 0.0001431  ***
#   2            sstest     1   0       -0.2604     0.0957 -2.7208 118 0.0074970   **
#   3            sstest     1   1        0.0304     0.1388  0.2193 118 0.8268192     



a_ch_SAD_age_fit<- lme(a_measurement ~ SCARED_SAD_Ch_PRE*age*PE_PC +stim_meds_PRE 
                       + chgender, random = ~1 | subject, data = analysisDat3_long, na.action=na.omit)

summary(a_ch_SAD_age_fit)
# Fixed effects:  a_measurement ~ SCARED_SAD_Ch_PRE * PE_PC + age * PE_PC + SCARED_SAD_Ch_PRE *      age * PE_PC + stim_meds_PRE + chgender 
# Value  Std.Error  DF   t-value p-value
# (Intercept)                  -0.0451253 0.11440705 119 -0.394428  0.6940
# SCARED_SAD_Ch_PRE             0.1764349 0.08477350 119  2.081250  0.0396 *
# PE_PC1                        0.0579199 0.10185979  77  0.568624  0.5713
# age                          -0.4936022 0.08632837 119 -5.717729  0.0000 **
# stim_meds_PRE1               -0.2229477 0.10970373 119 -2.032271  0.0444 *
# chgender1                     0.1650269 0.07708605 119  2.140815  0.0343 *
# SCARED_SAD_Ch_PRE:PE_PC1     -0.0499417 0.10027024  77 -0.498071  0.6199
# PE_PC1:age                    0.1927078 0.10152078  77  1.898210  0.0614
# SCARED_SAD_Ch_PRE:age         0.0352887 0.09108604 119  0.387422  0.6991
# SCARED_SAD_Ch_PRE:PE_PC1:age -0.0466012 0.10648122  77 -0.437648  0.6629

#plotting
plot_model(a_ch_SAD_age_fit, type = "pred", terms = c("PE_PC", "age [-1,0,1]"))



################ par SAD:age:PE/PC- Mixed effects models #############################

ratio_par_SAD_age_fit<- lme(reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Par_PRE*PE_PC + age*PE_PC + SCARED_SAD_Par_PRE*age*PE_PC +stim_meds_PRE + chgender, random = ~1 | subject, data = analysisDat3_long, na.action=na.omit)
summary(ratio_par_SAD_age_fit)
# Fixed effects:  reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Par_PRE * PE_PC +      age * PE_PC + SCARED_SAD_Par_PRE * age * PE_PC + stim_meds_PRE +      chgender 
# Value  Std.Error  DF   t-value p-value
# (Intercept)                    0.2788789 0.12156976 117  2.293982  0.0236
# SCARED_SAD_Par_PRE            -0.0240437 0.09977171 117 -0.240987  0.8100
# PE_PC1                        -0.5595155 0.14486006  76 -3.862455  0.0002 **
# age                            0.0159567 0.09498898 117  0.167985  0.8669
# stim_meds_PRE1                 0.0693246 0.10386834 117  0.667428  0.5058
# chgender1                      0.0604152 0.07729592 117  0.781609  0.4360
# SCARED_SAD_Par_PRE:PE_PC1     -0.1891118 0.15132817  76 -1.249680  0.2153
# PE_PC1:age                     0.3042314 0.14648034  76  2.076943  0.0412 *
# SCARED_SAD_Par_PRE:age        -0.0089337 0.08580687 117 -0.104114  0.9173
# SCARED_SAD_Par_PRE:PE_PC1:age  0.0140812 0.14403002  76  0.097766  0.9224

plot_model(ratio_par_SAD_age_fit, type = "pred", terms = c("PE_PC", "age [-1,0,1]"))

library(reghelper)
simple_slopes(ratio_par_SAD_age_fit, levels= list(PE_PC = c(1,-1),  age= "sstest", SCARED_SAD_Par_PRE = 0))




a_par_SAD_age_fit<- lme(a_measurement ~ SCARED_SAD_Par_PRE*PE_PC + age*PE_PC + SCARED_SAD_Par_PRE*age*PE_PC +stim_meds_PRE + chgender, random = ~1 | subject, data = analysisDat3_long, na.action=na.omit)
summary(a_par_SAD_age_fit)
# Fixed effects:  a_measurement ~ SCARED_SAD_Par_PRE * PE_PC + age * PE_PC + SCARED_SAD_Par_PRE *      age * PE_PC + stim_meds_PRE + chgender 
# Value  Std.Error  DF   t-value p-value
# (Intercept)                   -0.0815272 0.11942504 118 -0.682664  0.4962
# SCARED_SAD_Par_PRE             0.1226935 0.09215597 118  1.331368  0.1856
# PE_PC1                         0.1184816 0.10285543  78  1.151923  0.2529
# age                           -0.5377089 0.08812315 118 -6.101789  0.0000 ***
# stim_meds_PRE1                -0.2365492 0.11086749 118 -2.133621  0.0349 *
# chgender1                      0.1294762 0.08118187 118  1.594891  0.1134
# SCARED_SAD_Par_PRE:PE_PC1      0.1296381 0.10895422  78  1.189841  0.2377
# PE_PC1:age                     0.1549791 0.10244800  78  1.512759  0.1344
# SCARED_SAD_Par_PRE:age         0.0444675 0.07885135 118  0.563942  0.5739
# SCARED_SAD_Par_PRE:PE_PC1:age -0.0849106 0.10303880  78 -0.824064  0.4124




`################ ACS_total:SAD:age:PE/PC- Mixed effects models #############################

#subset the data to look at PE trials only
analysisDat3_long_PEonly <- subset(analysisDat3_long, PE_PC == 1)

#FIFTEEN ITEM ACS w 20 trial cutoff 
ratio_ch_ACS_SAD_age_PEonly_fit<- lme(reverse_sda_rd_ratio_measurement ~  SCARED_SAD_Ch_PRE*age + ACS_total_Ch_PRE_15item*SCARED_SAD_Ch_PRE*age +stim_meds_PRE + chgender, random = ~1 | subject, data = analysisDat3_long_PEonly, na.action=na.omit)
summary(ratio_ch_ACS_SAD_age_PEonly_fit)
# Fixed effects:  reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Ch_PRE * age +      ACS_total_Ch_PRE_15item * SCARED_SAD_Ch_PRE * age + stim_meds_PRE +      chgender 
# Value Std.Error DF    t-value p-value
# (Intercept)                                   -0.0231412 0.2232651 70 -0.1036491  0.9177
# SCARED_SAD_Ch_PRE                             -0.1085440 0.1723218 70 -0.6298915  0.5308
# age                                           -0.0480173 0.1747830 70 -0.2747253  0.7843
# ACS_total_Ch_PRE_15item                       -0.4530648 0.1933951 70 -2.3426898  0.0220 *
# stim_meds_PRE1                                 0.1206488 0.2116122 70  0.5701412  0.5704
# chgender1                                      0.0153016 0.1665831 70  0.0918555  0.9271
# SCARED_SAD_Ch_PRE:age                          0.1782593 0.1750354 70  1.0184187  0.3120
# SCARED_SAD_Ch_PRE:ACS_total_Ch_PRE_15item     -0.3694760 0.1790446 70 -2.0635975  0.0428 *
# age:ACS_total_Ch_PRE_15item                    0.1690507 0.1792934 70  0.9428718  0.3490
# SCARED_SAD_Ch_PRE:age:ACS_total_Ch_PRE_15item  0.3922887 0.1901540 70  2.0630055  0.0428 *



plot_model(ratio_ch_ACS_SAD_age_PEonly_fit, type = "pred", terms = c("SCARED_SAD_Ch_PRE", "age [-1,1]", "ACS_total_Ch_PRE_15item [-1,1]"))


simple_slopes(ratio_ch_ACS_SAD_age_PEonly_fit, levels= list(ACS_total_Ch_PRE_15item = c(-1,0,1), SCARED_SAD_Ch_PRE= 'sstest', age= c(-1,0,1)))


#subset the data to look at PC trials only
analysisDat3_long_PConly <- subset(analysisDat3_long, PE_PC == -1)

ratio_ch_ACS_SAD_age_PConly_fit<- lme(reverse_sda_rd_ratio_measurement ~  SCARED_SAD_Ch_PRE*age + ACS_total_Ch_PRE_15item*SCARED_SAD_Ch_PRE*age +stim_meds_PRE + chgender, random = ~1 | subject, data = analysisDat3_long_PConly, na.action=na.omit)
summary(ratio_ch_ACS_SAD_age_PConly_fit)
# Fixed effects:  reverse_sda_rd_ratio_measurement ~ SCARED_SAD_Ch_PRE * age +      ACS_total_Ch_PRE_15item * SCARED_SAD_Ch_PRE * age + stim_meds_PRE +      chgender 
# Value  Std.Error  DF   t-value p-value
# (Intercept)                                    0.20970589 0.02111986 110  9.929320  0.0000
# SCARED_SAD_Ch_PRE                              0.01932352 0.01546686 110  1.249350  0.2142
# age                                            0.01758008 0.01512034 110  1.162677  0.2475
# ACS_total_Ch_PRE_15item                       -0.00118913 0.01672568 110 -0.071096  0.9435
# stim_meds_PRE1                                -0.03028999 0.02113986 110 -1.432838  0.1547
# chgender1                                      0.01879579 0.01450930 110  1.295431  0.1979
# SCARED_SAD_Ch_PRE:age                          0.01166697 0.01522358 110  0.766375  0.4451
# SCARED_SAD_Ch_PRE:ACS_total_Ch_PRE_15item     -0.01466091 0.01535443 110 -0.954833  0.3418
# age:ACS_total_Ch_PRE_15item                    0.00065751 0.01665504 110  0.039478  0.9686
# SCARED_SAD_Ch_PRE:age:ACS_total_Ch_PRE_15item -0.00503140 0.01769516 110 -0.284338  0.7767



############ behav t-test Acc RT ########

# ttest of accuracy when congruency = 1 vs acc when cong. = -1
t.test(analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency==-1], analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency==1])
# data:  analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency == -1] and analysisDat3_long_acc$avgAcc_value[analysisDat3_long_acc$avgAcc_congruency == 1]
# t = -10.843, df = 239.3, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.12582589 -0.08713582
# sample estimates:
#   mean of x mean of y 
# 0.8036060 0.9100869 

#correct congruent vs. correct incongruent (NOT post-error/correct)
t.test(analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency==1 & analysisDat3_long_RT$avgRT_accuracy==1], analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency==-1 & analysisDat3_long_RT$avgRT_accuracy==1])
# data:  analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency == 1 & analysisDat3_long_RT$avgRT_accuracy == 1] and analysisDat3_long_RT$avgRT_value[analysisDat3_long_RT$avgRT_congruency == -1 & analysisDat3_long_RT$avgRT_accuracy == 1]
# t = -3.869, df = 289.33, p-value = 0.000135
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -91.32370 -29.73872
# sample estimates:
#   mean of x   mean of y 
#    551.2683   611.7995 







