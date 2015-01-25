## Step 1 
## Reading the Train and Test datasets
trainx <- read.table("./projectdata/train/X_train.txt")
testx <- read.table("./projectdata/test/X_test.txt")
## Combining both the datasets with 561 variables
rowcomb1 <- rbind(trainx, testx)
## Reading the corresponding Subject values for Train and Test datasets
subtrain <- read.table("./projectdata/train/subject_train.txt")
subtest <- read.table("./projectdata/test/subject_test.txt")
## Combining all the subject values into 1 variable
rowcomb2 <- rbind(subtrain, subtest)
## Reading the corresponding activity labels for Train and Test datasets
trainy <- read.table("./projectdata/train/y_train.txt")
testy <- read.table("./projectdata/test/y_test.txt")
## Combining all the activity values into 1 variable
rowcomb3 <- rbind(trainy, testy)
## Combining the datasets with Activity and Subject variables
colcomb <- cbind(rowcomb1, rowcomb2, rowcomb3)
## Reading the variable names for 561 variables
var1 <- read.table("./projectdata/features.txt")
## Converting the values into character to store them as variable names for the combined dataset
var1c <- as.character(var1[,2])
var <- c(var1c, "subject", "activity")
dat <- colcomb
## Creating unique variable names to avoid duplicate name error with Select later on 
colnames(dat) <- make.names(var, unique=TRUE)
## 'dat' is the merged dataset
## Step 2
library(dplyr)
## Selecting column names which have "mean" and "std" as we need to extract only the measurements on mean and standard deviation
fdat <- dat %>% select(subject, activity, contains("mean"), contains("std"))
## 'fdat' is the smaller extracted dataset
## Step 3
## Class of activity variable is an integer, which needs to be a factor for setting descriptive activity names
fdat$activity = factor(fdat$activity)
library(plyr)
## Setting descriptive values for the levels in activity
fdat$activity <- revalue(fdat$activity, c("1"="Walking", "2"="WalkingUpstairs", "3"="WalkingDownstairs", "4"="Sitting", "5"="Standing", "6"="Laying"))
## 'fdat' modified to include descriptive activity values
## Step 4
## Substituting vague letters with more descriptive names
names(fdat) <- gsub("fBody", "FreqBody", names(fdat))
names(fdat) <- gsub("tBody", "TimeBody", names(fdat))
names(fdat) <- gsub("tGravity", "TimeGravity", names(fdat))
names(fdat) <- gsub("BodyBody", "Body", names(fdat))
names(fdat) <- gsub("X", "XAxis", names(fdat))
names(fdat) <- gsub("Y", "YAxis", names(fdat))
names(fdat) <- gsub("Z", "ZAxis", names(fdat))
## Using Camel case to make lengthy strings more readable
camel <- function(x){ #function for camel case
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}
names(fdat) <- names(fdat) %>% camel() 
## Making the subject variable a factor before the grouping operation
fdat$Subject = factor(fdat$Subject)
## 'fdat' modified to include descriptive variable names
## Step 5
## Grouping the 'fdat' dataset based on subject and activity
gdat <- group_by(fdat, Subject, Activity)
## Creating second tidy dataset with the average of each variable for each activity and each subject
tdat <- gdat %>% summarise_each(funs(mean))
## tdat is the final tidy dataset