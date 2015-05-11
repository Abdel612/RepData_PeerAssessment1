#
# Unzip data archive file (ZIP) - Default to archive name (fileName) = activity.zip
#
uncompressDataArchive = function(fileName = "activity.zip"){ 
    unzip(fileName, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = ".", unzip = "internal",
          setTimes = FALSE)
}

#
# Load dataset from a CSV file - Default to file name (fileName) = activity.csv
#
loadDataset = function(fromFileName  ="activity.csv"){
  dataSet = read.csv(fromFileName)
  return(dataSet)
}



#
# Compute mean total (sum) steps by date
#
# Return a new data.frame with 2 columns:
#     - date : POSIX Date in YYYY-MM-DD format
#     - steps: numeric : Total number of steps in the day "date"
getNumberOfStepsByDate = function(dataSet){
  totalStepPerDay = tapply(dataSet$steps, dataSet$date, sum, na.rm = TRUE)
  theDate = names(totalStepPerDay)
  theTotal = as.numeric(totalStepPerDay)
  result = data.frame(date = strptime(theDate, format="%Y-%m-%d"), steps = theTotal)
  return( result )
}


##################################################
#
#       Here we Go for Assignement #1
#
##################################################

#
# PREREQUISITE: Loading and preprocessing the data
#

#   Step 1: explode the zip archive
uncompressDataArchive()

#   Step 2: Read the original dataset
originalDF = loadDataset()




#
# QUESTION #1: What is mean total number of steps taken per day?
#       For this part of the assignment, you can ignore the missing values in the dataset.
#

#   Step 1: Calculate the total number of steps taken per day
question1DF = getNumberOfStepsByDate( subset(originalDF, !is.na(originalDF$date)) )

#   Step 2: Make a histogram of the total number of steps taken each day
hist(question1DF$steps, xlab = "5' interval", ylab = "# steps", main = "Steps distribution")

#   Step 3: Calculate and report the mean and median of the total number of steps taken per day
meanQuestion1 = mean(question1DF$steps)
medianQuestion1 = median(question1DF$steps)

rm(question1DF) # gain some memory space by removing this dataset (no more use)


#
# QUESTION #2: What is the average daily activity pattern?
#

#   Step 1: Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
#         the average number of steps taken, averaged across all days (y-axis)
question2List=tapply(originalDF$steps, originalDF$interval, mean, na.rm = TRUE)
plot(names(question2List), as.numeric(question2List), 
     type = "l",
     xlab = "Interval", ylab = "# daily steps", main = "Average steps")

#   Step 2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
idxMaxSteps = which.max(question2List)
intervalMaxStep = as.numeric(names(question2List)[idxMaxSteps])
intervalMaxStep.hour = floor(intervalMaxStep/60)
intervalMaxStep.minute = intervalMaxStep%%60
sprintf("5' Interval #%d (starting at %02d:%02d:00) contains the maximum average number of steps over all days in the dataset: %0.2f",
        intervalMaxStep, intervalMaxStep.hour, intervalMaxStep.minute, question2List[idxMaxSteps])



#
# QUESTION 3: Imputing missing values
#

# Note that there are a number of days/intervals where there are missing values (coded as NA). 
# The presence of missing days may introduce bias into some calculations or summaries of the data.

#   Step 1: Calculate and report the total number of missing values in the 
#           dataset (i.e. the total number of rows with NAs)
missingStepIdx = which(is.na(originalDF$steps))
sprintf("There are %d missing values in the original dataset", length(missingStepIdx) )

#   Step 2 & 3: Devise a strategy for filling in all of the missing values in the dataset. 
#               The strategy does not need to be sophisticated. 
#               For example, you could use the mean/median for that day, or the mean 
#               for that 5-minute interval, etc.
#               Create a new dataset that is equal to the original dataset but with the missing data filled in.
#
#   The choosen strategy is to assign missing values the previously calculated mean (rounded) that
#   corresponds to the same time interval
question3DF = originalDF
question3DF$steps[missingStepIdx] = floor( as.numeric(question2List[as.character(originalDF$interval[missingStepIdx])]) + 0.5 )
question3DF = getNumberOfStepsByDate( question3DF )

#   Step 4: Made of 4 steps: a, b, c and d
#         4.a: Make a histogram of the total number of steps taken each day
hist(question3DF$steps, xlab = "5' interval", ylab = "# steps", main = "Steps distribution")


#         4.b: Calculate and report the mean and median total number of steps taken per day
meanQuestion3 = mean(question3DF$steps)
medianQuestion3 = median(question3DF$steps)

#         4.c: Do these values differ from the estimates from the first part of the assignment?
# Response: Only the mean is changing since according to the choosen strategy, the distribution does not change, thus the median won't change

#         4.d: What is the impact of imputing missing data on the estimates of the total daily number of steps?
# Response: The impact of imputing missing data with ur strategy did get the mean value raising


rm(question3DF) # gain some memory space by removing this dataset (no more use)

#
# QUESTION 4: Are there differences in activity patterns between weekdays and weekends?
#             Use the dataset with the filled-in missing values for this part.
WEEK_DAY = "Week Day" # Constant for weekday.type factor column (see below) => It's on week day
WEEK_END = "Week-End" # Constant for weekday.type factor column (see below) => It's on week-end
question4DF = originalDF
question4DF$steps[missingStepIdx] = floor( as.numeric(question2List[as.character(originalDF$interval[missingStepIdx])]) + 0.5 )

#   Step 1: Create a new factor variable in the dataset with 
#           two levels – “weekday” and “weekend” indicating whether a given date 
#           is a weekday or weekend day.
question4DF$weekday = strptime(question4DF$date, format = "%Y-%m-%d")
question4DF$weekday = lapply( question4DF$weekday$wday, # Avoid localisation issue (I'm using a french locale)
                              function(x){ 
                                if (x>5)
                                  return(WEEK_END)
                                else 
                                  return(WEEK_DAY)
                                }
                              )
question4DF$weekday = as.factor(as.character(question4DF$weekday))


#   Step 2: Make a panel plot containing a time series plot (i.e. type = "l") of 
#           the 5-minute interval (x-axis) and the average number of steps taken, 
#           averaged across all weekday days or weekend days (y-axis). 
#           See the README file in the GitHub repository to see an example of what 
#           this plot should look like using simulated data.
library(lattice) # Just to get sure the package is loaded

# Build weekday dataset with total steps an interval over all considered week days 
weekdayDF = subset(question4DF, question4DF$weekday == WEEK_DAY)
weekdayAvg = tapply(weekdayDF$steps, weekdayDF$interval, mean)
weekdayAvgDF = data.frame( interval = as.numeric( names(weekdayAvg) ), steps = floor(as.numeric(weekdayAvg)+0.5), weekday.type = rep(WEEK_DAY,length(weekdayAvg) ) )

# Build weekend dataset with total steps an interval over all considered week-end day
weekendDF = subset(question4DF, question4DF$weekday == WEEK_END)
weekendAvg = tapply(weekendDF$steps, weekendDF$interval, mean)
weekendAvgDF = data.frame( interval = as.numeric( names(weekendAvg) ), steps = floor(as.numeric(weekendAvg)+0.5), weekday.type = rep(WEEK_END,length(weekendAvg) ) ) 

# The final dataset is made of the two above dataset (weekday and weekden)
resultPanel = rbind(weekdayAvgDF,weekendAvgDF)

# Plot the panel as expected by the assignment
xyplot( steps ~ interval | weekday.type, data = resultPanel, 
        layout=c(1,2), type="l", col="red", xlab="Interval", ylab="Number of steps" )

