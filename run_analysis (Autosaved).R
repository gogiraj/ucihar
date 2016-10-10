#  name :       run_analysis.R 
#  author :     Murray Pung
#  date :       2016-10-10
#  description: 1. Merges the training and the test sets to create one data set.
#               2. Extracts only the measurements on the mean and standard deviation for each measurement.
#               3. Uses descriptive activity names to name the activities in the data set
#               4. Appropriately labels the data set with descriptive variable names.
#               5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

setwd("/Users/Muz/Documents/coursera/course3/ass3/UCI HAR Dataset") # set your working directory here - it must include the Samsung data
library(dplyr)
library(reshape2)
# The subjects are extracted 
# Each row identifies the subject who performed the activity for each window sample. 
subject          <- rbind(mutate(read.table("test/subject_test.txt",col.names="subject"),source="test"),
                          mutate(read.table("train/subject_train.txt",col.names="subject"),source="train"))

# extracts the indices of mean & SD vars
indices_meanstd  <- grep(".*mean().*|.*std().*", read.table("features.txt")[,2]) 

#read dataset and extract only mean & SD vars - step 1 & 2.
set              <- rbind(read.table("test/X_test.txt")[indices_meanstd], 
                          read.table("train/X_train.txt")[indices_meanstd])
names(set)       <- readLines(con="features.txt")[indices_meanstd] #add variable names

#The activity labels are extracted
label            <- rbind(read.table("test/Y_test.txt",col.names="activityID"), 
                          read.table("train/Y_train.txt",col.names="activityID"))

#read activity labels
act              <- read.table("activity_labels.txt",col.names=c("activityID","activity"))
#combine all data and merge activity labels
# step 4.
combined         <- left_join(cbind(subject,label,set),act,by = "activityID") %>% # step 3. add descriptive activity names
                    select(-activityID) # the activity ID is removed as it's no longer needed

combined_trans   <- melt(combined,id=c("subject","source","activity")) #transposition required for mean calculation below

# step 5. 
combined_casted  <- (dcast(combined_trans, subject + source + activity ~ variable, mean))
tidy             <- melt(combined_casted,id=c("subject","source","activity"))

#the tidy_data is written to the working directory
setwd("../ucihar")
write.table(tidy,"tidy_data.txt",row.name=FALSE)
setwd("../UCI HAR Dataset")