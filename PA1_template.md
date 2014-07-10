# Reproducible Research: Peer Assessment 1
by Melinda Higgins
dated 07/09/2014

## Loading and preprocessing the data



Note: The data cover 61 days. The number of steps taken are recorded every 5 minutes for a total of 288 entries per day, for a total of 17568 data entries (i.e. the number of rows in the dataset).

There are 2304 NA's (missing values), 11014 zeros (0's), and 4250 entries of non-zero steps ranging from 1 to 99 steps in the 5 minute window.




## What is mean total number of steps taken per day?

Calculate the overall mean and the mean for each day. Also run for the medians per day and overall - with an without the "imputation" method tried below...




## What is the average daily activity pattern?

Look at the raw data - scatterplot by time of day (the increments) - look at overall and by day of the week - also look at a table summarizing these data given the n, mean, median, maybe Q1, Q3, min and max - over all 61 days by time of day and again by day of week. make some plots to go along with...



## Imputing missing values

Need to consider why the data are "missing" (NA's). These are most likely due to inactivity, although zeros were also recorded during other entries. Perhaps the NAs are due to the monitor being turned off or other reason no data was recorded. 

If we assume that the NA's are due to inactivity then substituting 0's for NA would be appropriate. If this cannot be confirmed, a smoothing approach could be used taking a simple average of the reading before and after the NA is noted. Although if the reading before and after are both NAs then perhaps a zero is best.

However, the number of steps are highly right-skewed with obvious zero-inflation. This distribution indicates that using the mean to substitute for the NAs is not appropriate. Instead using the median would be better - although these are essentially zero for every day as well - again suggesting that substituting zero's for the NAs would be appropriate.

It is noted that without a proper explanation for the source of the NAs, any method used for substitution will be biased. To what extent the chosen method is biased is unverifiable without further information.





## Are there differences in activity patterns between weekdays and weekends?

Need to figure out what function is needed to extract day of the week from the provided dates. Also need to check the date formatting...

