library(dplyr)
library(reshape2)

setwd("D:\\RCoursera\\Getting and cleaning data\\UCI HAR Dataset")

features <- readLines("features.txt")

activities <- readLines("activity_labels.txt")


### TEST FILES


test.subject <- read.table("test\\subject_test.txt")
x.test <- read.table("test\\X_test.txt")
y.test <- readLines("test\\y_test.txt")



test <- cbind(test.subject, y.test, x.test)



colnames(test)[1] <- "id"

colnames(test)[2] <- "activities"

colnames(test)[3:563] <- features

                        

## TRAINING




train.subject <- read.table("train\\subject_train.txt")
x.train <- read.table("train\\X_train.txt")
y.train <- readLines("train\\y_train.txt")


train <- cbind(train.subject, y.train, x.train)

colnames(train)[1] <- "id"

colnames(train)[2] <- "activities"

colnames(train)[3:563] <- features



### Mergging the 2 data frames:

proyect <- rbind(train, test)

proyect$activities <- as.factor(proyect$activities)

### Get all proyect names of means and std, besides "id" and "activities"

proyect.names <- colnames(proyect)


mean.and.std <- grep("mean|std", proyect.names, value = F)
id.activity <- grep("^id$|^activities$", proyect.names, value = F)

### Keeping only relevant columns

proyect <- proyect[,c(id.activity,mean.and.std)]

### Class of "activities" is factor, nees to be "numeric"

proyect$activities <- as.numeric(as.character(proyect$activities))


### Reeplacing code numbers with their meaning: Activities.


for (i in 1:nrow(proyect)) {
        
        
        if(proyect$activities[i] == 1) {
                proyect$activities[i] <- "WALKING"  
        }
        
        else if (proyect$activities[i] == 2) {
                proyect$activities[i] <- "WALKING_UPSTAIRS"
        }
        
        else if (proyect$activities[i] == 3) {
                proyect$activities[i] <- "WALKING_DOWNSTAIRS"
        }
        
        else if (proyect$activities[i] == 4) {
                proyect$activities[i] <- "SITTING"
        }
        
        else if (proyect$activities[i] == 5) {
                proyect$activities[i] <- "STANDING"
        }
        
        else if (proyect$activities[i] == 6) {
                proyect$activities[i] <- "LAYING"
        }
        
        else {
                proyect$activities[i] <- proyect$activities[i]
        }
}


##### STEP 4: renaming variables to make them easier to work with

new.features <- colnames(proyect)


new.features <- sub("bodybody", "Body", new.features, ignore.case = T)
new.features <- sub("[0-9]{1,3} ", "", new.features, ignore.case = T)
new.features <- gsub("[-]*", "", new.features, ignore.case = T)
new.features <- gsub("[()]*", "", new.features, ignore.case = T)
new.features <- tolower(new.features)


colnames(proyect) <- new.features


### Step 5: Making an independant tidy data set

#We need to take the out what our "measure.vars" are going to be

measurevars <- new.features[3:length(new.features)]

#Transform from wide to long (tidy data)

proyect2 <- melt(proyect,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("id", "activities"),
                  # The source columns
                  measure.vars= measurevars,
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="features",
                  value.name="value")



proyect2$id <- as.factor(proyect2$id)
proyect2$activities <- as.factor(proyect2$activities)


proyecto2
#write.table(proyect2, "proyect-tidy.txt", row.names = F)

