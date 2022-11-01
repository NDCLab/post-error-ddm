# This script is child_multiSubImport_cleanAccRT_v6-0

# It's different from v5-0 in that it takes congruency into account. We want to only look 
# at n+1 trials (AKA post-error/post-correct trials) where n (the trial preceding p-e/p-c) is incongruent.
# Additionally, we brought back the rejection based on low accuracy and outlier number of fast RTs. 
# Lastly, it removes participants with fewer than 10 p-e or 10 p-c trials.

# What this script does:
#   1. Import all the .txt files for each subject into one big "raw" file (including the block number)
#   2. Organize data with the following columns:
#         a. make a "firstOfBlock" column
#         b. make a "priorAccuracy" column
#         b. make a "priorCongruency" column
#         c. make a "priorBad" column (the trial before was a fastRT)
#         c. make a "currentBad" column (current trial is a fastRT or the first trial in a block/subject)
#   2. Remove participants with accuracy lower than 60% and an outlier number of fast RTs.
#   3. Remove fast RT trials, first trial of each block, and trials where n is congruent (based on organizational columns)
#   4. Remove participants with fewer than 10 post-error or 10 post-correct trials
#   4. Create a "clean" csv with all the data for the remaining subs and their remaining trials
#
# The "clean" csv will be read into the DDM script.
#
# Notes about participant exclusion are located in PRE_dataset_organization.xlsx
#
####################### Intro parameters ######################################

library("rprime")
library("knitr")
library("plyr")
library("readr")
library("dplyr")
# beepr sends a chime when it's done running
library(beepr)

opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE,
  collapse = TRUE)

#where to save the mult-subject behavioral data
out_path <- "~/OneDrive - Florida International University/Research/DDM project/masters-project/cleaning-materials/" 

#name for raw multi-sub file
fileName_raw <- "v6-0_raw_PRE_flanker_"

#name for cleaned multi-sub file
fileName_clean <- "v6-0_clean_PRE_flanker_"


####################### making the function ######################################

## set up function to pull out data of interest for each participant
ImpPrimeData <- function(data_path) {
  
  # Read in an Eprime text file
  experiment_lines <- read_eprime(data_path)
  
  # Extract and parse the log-frames from the file
  experiment_data <- FrameList(experiment_lines)
  
  # select only the experimental trial data
  expTrial_data <- filter_in(experiment_data, "Procedure", "trialproc")
  
  #convert to data frame
  expTrial_data_df <- to_data_frame(expTrial_data)
  
  #throw away unneeded columns
  columns_to_keep <- c("Flanker","TrialType","ITI",
                       "FlankerImage.ACC","FlankerImage.RT","TrialNumber",
                       "RespType","Feed", "BlockNumber")
  expTrial_data_limited_df <- expTrial_data_df[columns_to_keep]
  
  #Convert data formats as appropriate
  expTrial_data_limited_df$Flanker = as.numeric(factor(expTrial_data_limited_df$Flanker, levels = c("Congruent","Incongruent")))
  expTrial_data_limited_df$TrialType = as.numeric(factor(expTrial_data_limited_df$TrialType, levels = c("CL","CR","IL","IR")))
  expTrial_data_limited_df$ITI = as.numeric(as.character(expTrial_data_limited_df$ITI))
  expTrial_data_limited_df$FlankerImage.ACC = as.numeric(as.character(expTrial_data_limited_df$FlankerImage.ACC))
  expTrial_data_limited_df$FlankerImage.RT = as.numeric(as.character(expTrial_data_limited_df$FlankerImage.RT))
  expTrial_data_limited_df$TrialNumber = as.numeric(as.character(expTrial_data_limited_df$TrialNumber))
  expTrial_data_limited_df$RespType = as.numeric(factor(expTrial_data_limited_df$RespType, levels = c("Correct","Commission","Omission")))
  expTrial_data_limited_df$Feed = as.numeric(factor(expTrial_data_limited_df$Feed, levels = c("none","GoodJob","Speed","Accuracy")))
  expTrial_data_limited_df$BlockNumber = as.numeric(as.character(expTrial_data_limited_df$BlockNumber))
  
  #Change the variable names
  colnames(expTrial_data_limited_df) <- c("congruency", "trialType", "ITI", "accuracy", "rt", "trialNum", "respType", "feedType", "blockNum")
  
  ## Now get the subject ID and add it to the data frame before exporting
  
  # select only the header data
  Header_data <- filter_in(experiment_data, "Procedure", "Header")
  
  #convert to data frame
  Header_data_df <- to_data_frame(Header_data)
  
  #throw away unneeded columns #OAS- keeping session so we can easily identify multiples
  columns_to_keep2 <- c("Subject","Session")
  Subject_df <- Header_data_df[columns_to_keep2]
  
  #extract subNum and convert to integer
  subNum <- as.integer(Subject_df[1,1])
  #extract sessNum and convert to integer (first row, second column)
  sessNum <- as.integer(Subject_df[1,2])
  
  #determine how many rows in this person's data
  N<-nrow(expTrial_data_limited_df)
  
  #create new data frame that only has subject ID and is length of data for this person
  subNum_df <- data.frame(subject = integer(N), session= integer(N))
  subNum_df$subject <- subNum
  subNum_df$session <- sessNum
  
  #bind the subject ID and the rest of the data for this person
  expTrial_data_limited_withID_df <- cbind(subNum_df, expTrial_data_limited_df)
  
  # Optionally, one might save the processed file via: 
  # csv <- paste0(file_path_sans_ext(data_path), ".csv")
  # write.csv(sails, csv, row.names = FALSE)
  
  ImportedData <- expTrial_data_limited_withID_df
}


##################### creating RAW dataframe ###################################

## Apply function that pulls out data for each participant to multiple participants and concatenates data
#get the file paths for all participants in this folder. Note: r scripts in folder interpreted as .txt as well!!
data_paths <- list.files("~/OneDrive - Florida International University/Research/DDM project/masters-project/masters-data/flanker-behavior-only-data/run_2022-08/included-data",
                         pattern = ".txt", full.names = TRUE)

#print the filepaths to the terminal
data_paths

#run this on everyone and concatenate all the data into one huge file
multiSub_data_raw_import <- ldply(data_paths, ImpPrimeData)
# chime so I know when the concatenating part is done (takes about 15 min)
beep()

#write data to disk
currentDate <- format(Sys.time(), "%Y-%m-%d_%I-%p")
write.csv(multiSub_data_raw_import, paste(out_path,fileName_raw,currentDate,".csv",
                                          sep="", collapse=NULL), row.names = FALSE)



##################### creating CLEAN dataframe #################################

#renaming so if needed I can return to the raw file without waiting 15 min for the ldply function
# multiSub_data_raw <- read.csv("~/OneDrive - Florida International University/Research/DDM project/masters-project/cleaning-materials/v6-0_raw_PRE_flanker_2022-08-29_04-PM.csv")
multiSub_data_raw <- multiSub_data_raw_import

######## contextually-labeled organizational columns ##############

#make a list of subjects to be used later. 
subList <- unique(multiSub_data_raw$subject)

## firstOfBlock
# Label the first trial in every block
# Every time there is a unique combo of sub and blockNum, label it as bad
# Note: this also (by definition) marks the first trial for each sub, so no need for a separate "firstTrial" column
multiSub_data_raw$firstOfBlock<- ifelse(!duplicated(multiSub_data_raw[c("subject", "blockNum")])==TRUE,1,0)


# Note: Previous script versions put HOLD at the first trial of each sub in
# the priorAccuracy, priorCongruency, priorBad, and currentBad columns. That's no longer 
# necessary due to labeling of firstOfBlock in currentBad column 


### PriorAccuracy ##
# Make a new column that will describe post-error (100) and post-correct (111)
# putting HOLD in order to bump everything down one to make it n+1. Used HOLD instead of NA so I knew it was intentional
priorAccuracy <- c("HOLD", ifelse(multiSub_data_raw$accuracy <1, 100, 111))
multiSub_data_raw$priorAccuracy <- priorAccuracy[1:(length(priorAccuracy)-1)]


## priorCongruency ##
# Make a new column that will describe whether the prior trial was congruent (1) or incongruent (2)
priorCongruency <- c("HOLD", ifelse(multiSub_data_raw$congruency >1, 2, 1))
multiSub_data_raw$priorCongruency <- priorCongruency[1:(length(priorCongruency)-1)]



## priorBad ##
# Make a new column that will describe whether the prior trial was bad- fast RT in this case
# Note: there won't be a 1:1 with priorBad and currentBad because priorBad doesn't consider the 
# firstTrial or firstOfBlock.
priorBad <- c("HOLD", ifelse(multiSub_data_raw$rt <150, 1, 0))
multiSub_data_raw$priorBad <- priorBad[1:(length(priorBad)-1)]


## currentBad ##
# Make a new column that will describe whether the prior trial was bad-- fast RT, 
# firstTrial=1, or firstOfBlock=1
currentBad <- ifelse(multiSub_data_raw$rt <150 | multiSub_data_raw$firstOfBlock==1, 1, 0)
multiSub_data_raw$currentBad <- currentBad[1:(length(currentBad))]


######### Removing participants with low Acc or too many fast RTs #################

# calculating how many fast RTs everyone has
fastRT_df <- data.frame(matrix(ncol=3, nrow=0))
colnames(fastRT_df)<- c ("subject", "number_fastRTs", "accuracy")


for (j in 1:length(subList)){
  #isolate one subject so we can count fast RTs
  singlesubDat2 <- subset(multiSub_data_raw, multiSub_data_raw$subject==subList[j])
  
  subject <- subList[j]
  #calculate number of fast RTs for this participant and make a variable that holds the number
  numFastRTs <- nrow(singlesubDat2[singlesubDat2$rt < 150, ])
  #acc
  accuracy <- mean(singlesubDat2$accuracy)
  
  #make new row with values created above
  newRow <- data.frame(subject, numFastRTs, accuracy)
  colnames(newRow)<- c("subject", "number_fastRTs", "accuracy")
  
  #add the new row to the existing df
  fastRT_df <- rbind(fastRT_df, newRow)
  
}

# calculating outliers
# mean() + 3*sd()
outlierFastRTs<-  mean(fastRT_df$number_fastRTs)+ 3*sd(fastRT_df$number_fastRTs)


#initialize the temp table
cleaning_temp <- data.frame(matrix(ncol=16, nrow=0))
colnames(cleaning_temp)<- c ("subject", "session", "congruency", "trialType", "ITI", "accuracy", 
                                   "rt", "trialNum", "respType", "feedType", "blockNum", "firstOfBlock",
                             "priorAccuracy", "priorCongruency", "priorBad", "currentBad")


# Removing people with less than 60% acc and an outlier value of fast RTs
for (k in 1:length(subList)){
  
  #make a df where we only look at the k-th sub
  singlesubDat2 <- subset(multiSub_data_raw, multiSub_data_raw$subject==subList[k])
  
  #this adds participants to the clean file if they have above 60% accuracy
  if ((mean(singlesubDat2$accuracy) > .60)& fastRT_df$number_fastRTs[fastRT_df$subject==subList[k]]< outlierFastRTs){
    cleaning_temp <- rbind(cleaning_temp, singlesubDat2)
  } # end of accuracy-check if loop
  
} # end of "accuracy" for loop

# just checking to make sure it removed people correctly
unique(cleaning_temp$subject)


######## making the final df without priorBad and currentBad trials ##############



# Making a subset of data that excludes when
#   1. RT for trial before was too fast (priorBad)
#   2. current trial RT was too fast (currentBad)
#   3. current trial is the first of the block or of the expt (currentBad)
#   4. trial before was congruent (priorCongruency)- we only want to include n trials that are incongruent (2)
# (did priorCongruency ==2 instead of !=1 bc of the HOLD, don't want that to be counted just in case)
cleaning_temp_2 <- subset(cleaning_temp, priorBad != 1 & currentBad != 1 & priorCongruency==2)

subList_clean <- unique(cleaning_temp_2$subject)


################# remove participants with fewer than 10 p-e or p-c trials #####################

# We reject any participants with less than 10 post-error trials or less
# than 10 post-correct trials. 

# Doing this after we remove priorBad, currentBad, and n trials that are congruent.

# This is also a good time to spot check the data and make sure nobody that 
# should have been rejected is in there and that p-e/p-c counts make sense.

#initialize post-error/correct df 
postErrCorr_df <- data.frame(matrix(ncol=3, nrow=0))
colnames(postErrCorr_df)<- c("subject", "numPostError", "numPostCorrect")


for (i in 1:length(subList_clean)){
  
  #make a df where we only look at the i-th sub
  singlesubDat <- subset(cleaning_temp_2, cleaning_temp_2$subject==subList_clean[i])
  
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

for (m in 1:length(subList_clean)){
  
  #make a df where we only look at the m-th sub
  singlesubDat3 <- subset(cleaning_temp_2, cleaning_temp_2$subject==subList_clean[m])
  
  # if the number of post-error trials & the number of post-correct trials for a subject[m] is greater than 10...
  if (postErrCorr_df$numPostError[postErrCorr_df$subject==subList_clean[m]]>10 & postErrCorr_df$numPostCorrect[postErrCorr_df$subject==subList_clean[m]]>10){
   #... then add it to the final df.
     multiSub_data_clean <- rbind(multiSub_data_clean, singlesubDat3)
  }
}

# just checking that the loop rejected/included who it was supposed to
unique(multiSub_data_clean$subject)


################## Make a .csv #####################################


#write data to disk
currentDate <- format(Sys.time(), "%Y-%m-%d_%I-%p")
write.csv(multiSub_data_clean,paste(out_path,fileName_clean,currentDate,".csv",
                                    sep = "", collapse = NULL), row.names=FALSE)





