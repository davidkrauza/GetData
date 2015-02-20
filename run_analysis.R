#Setup Variables
temp.file <- tempfile()
unzip.directory <- "data"
data.base.dir <- paste(unzip.directory,"/","UCI HAR Dataset", sep="")
file.zip <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#Download File from UCI and unzip to data directory
download.file(file.zip,temp.file, method="curl")
unzip(temp.file, exdir=unzip.directory)

#Cleanup memory
rm(temp.file)

#merge training and test datasets
file.train.x <- paste(data.base.dir,"/","train/X_train.txt", sep="")
file.train.y <- paste(data.base.dir,"/", "train/y_train.txt", sep="")
file.train.subject <- paste(data.base.dir,"/","train/subject_train.txt", sep="")
file.test.x <- paste(data.base.dir,"/", "test/X_test.txt", sep="")
file.test.y <- paste(data.base.dir, "/", "test/y_test.txt", sep="")
file.test.subject <- paste(data.base.dir,"/", "test/subject_test.txt", sep="")

data.x <- rbind(read.table(file.test.x), read.table(file.train.x))
data.y <- rbind(read.table(file.test.y), read.table(file.train.y))
data.subject <- rbind(read.table(file.test.subject)
                      ,read.table(file.train.subject))

#read features list
file.features <- paste(data.base.dir,"/","features.txt", sep="")
data.features <- read.table(file.features, stringsAsFactors=FALSE)

#only use names from features list
features <- data.features$V2

## Logical Vector to keep only std and mean columns
keepColumns <- grepl("(std|mean[^F])", features, perl=TRUE)

## Keep only some data 
data.x <- data.x[, keepColumns]
names(data.x) <- features[keepColumns]
names(data.x) <- gsub("\\(|\\)", "", names(data.x))
names(data.x) <- tolower(names(data.x))

## Read ActivityList (to add descriptive names to data set)
file.activities <- paste(data.base.dir,"/","activity_labels.txt", sep="")
activities <- read.table(file.activities)
activities[,2] = gsub("_", "", tolower(as.character(activities[,2])))
data.y[,1] = activities[data.y[,1], 2]
names(data.y) <- "activity"

## Add human readable labels to activity names
file.tidyData <- paste(data.base.dir,"/","tidyData.txt", sep="")
names(data.subject) <- "subject"
tidyData <- cbind(data.subject, data.y, data.x)
write.table(tidyData, file.tidyData)

## Create second tiny data set with avg of each var for each act and each sub
uS = unique(data.subject)[,1]
nS = length(uS)
nA = length(activities[,1])
nC = length(names(tidyData))
td = tidyData[ 1:(nS*nA), ]

row = 1
for (s in 1:nS) {
    for (a in 1:nA) {
        td[row,1] = uS[s]
        td[row,2] = activities[a, 2]
        tmp <- tidyData[tidyData$subject==s & tidyData$activity==activities[a,2],]
        td[row, 3:nC] <- colMeans(tmp[, 3:nC])
        row = row + 1
    }
}
file.tidyData2 <- paste(data.base.dir,"/","tidyData2.txt", sep="")
write.table(td, file.tidyData2, row.name=FALSE)