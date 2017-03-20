require(ggplot2);library(ggplot2movies);library(plyr);library(robustHD);library(MASS);library(reshape2)
data("movies");data("baseball");data("ozone");data("diamonds");data("tips")

# NA Quiz
mean(movies$length) # average length
mean(movies$budget) # average budget
mean(movies$budget, na.rm = TRUE) # mean avg budget remove missing values
mean(is.na(movies$budget)) # frequency of missing budget
moviesNoNA = na.omit(movies) # returns a dataset with all missing data removed

# R Plot Quiz
moviesNoNA = na.omit(movies)
qplot(rating, budget, data = moviesNoNA, size = I(1.2)) +
  stat_smooth(color = "red", size = I(2), se = F)

# Movie Data Analysis Quiz
moviesNoNA = na.omit(movies)
qplot(rating, votes, data = moviesNoNA, size = I(1.2))

# Winsorize Data Quiz
originalData <- c(1000.0000000  ,  1.8410249 ,  -0.7923505 ,   0.1776514  ,  0.4002135)
Data = winsorize(originalData)

# Std Mean Quiz
original_data = rnorm(20)
original_data[1] = 1000
sorted_data = sort(original_data)
filtered_data = sorted_data[3:18]
lower_limit = mean(filtered_data) - 5*sd(filtered_data)
upper_limit = mean(filtered_data) + 5*sd(filtered_data)
not_outlier_ind = (lower_limit < original_data) & (original_data < upper_limit)
data_w_no_outliers = original_data[not_outlier_ind]

# Skewness: Diamond Example Quiz (original values vs log values)
diamondsSubset = diamonds[sample(dim(diamonds)[1],1000),]
qplot(price, data = diamondsSubset)
qplot(log(price), size = I(1), data = diamondsSubset)

# Power Transformation  Quiz (log-log scale)
qplot(brain, body, data = Animals)
qplot(log(brain), log(body), data = Animals)
  # or
qplot(brain, body, log = "xy", data = Animals)

# Data Manipulations: Partitioning
D = array(data = seq(1,20), dim = c(4,5))
rand_perm = sample(dim(D)[1],dim(D)[1])
first_set_of_indices = rand_perm[1:floor(4*0.75)]
second_set_of_indices = rand_perm[(floor(4*0.75)+1):4]
D1 = D[first_set_of_indices,]
D2 = D[second_set_of_indices,]

# Reshaping Data
smiths_tall = melt(smiths, id = 1)

# Smoker-Tip Example/Quiz
tipsm = melt(tips, id = c("sex","smoker","day","time","size"))
dcast(tipsm, # Mean of measurement variables broken by sex
      sex~variable,
      fun.aggregate = mean)

  # Number of occurrences for measurement variables broken by sex
dcast(tipsm,
      sex~variable,
      fun.aggregate = length)

  # Average total bill and tip for different times
dcast(tipsm,
      time~variable,
      fun.aggregate = mean)

  # Similar to above with breakdown for sex and time:
dcast(tipsm,
      sex+time~variable,
      fun.aggregate = length)

  # Similar to above, but with mean and added margins
dcast(tipsm,
      sex+time~variable,
      fun.aggregate = mean,
      margins = TRUE)

# Split-Apply-Combine (plry)
names(baseball)
  # count number of players recorded for each year
bbPerYear = ddply(baseball, "year", "nrow")
head(bbPerYear)
qplot(x = year, y = nrow,
      data = bbPerYear, geom = "line",
      ylab = "number of player seasons")

  # compute mean rbi for all years. summarize is the apply function, which takes as argument a function that computes the rbi mean
bbMod = ddply(baseball, "year", summarise,
              mean.rbi = mean(rbi, na.rm = TRUE))
qplot(x = year, y = mean.rbi, data = bbMod,
      geom = "line", ylab = "mean RBI")

  # add a column career.year which measures the number of years passed since each player started batting
bbMod2 = ddply(baseball,
               "id",
               transform,
               career.year = year - min(year) + 1)
  # sample a random subset 3000 rows to avoid over-ploting
bbSubset = bbMod2[sample(dim(bbMod2)[1], 3000),]
qplot(career.year,
      rbi, data = bbSubset,
      size = I(0.8),
      geom = "jitter",
      ylab = "RBI",
      xlab = "years of player") +
  geom_smooth(color = "red", se = F, size = 1.5)

# Ozone Example/Quiz
latitude.mean = aaply(ozone, 1, mean)
longitude.mean = aaply(ozone, 2, mean)
time.mean = aaply(ozone, 3, mean)
longitude = seq(along = longitude.mean)
qplot(x = longitude,
      y = longitude.mean,
      ylab = "mean ozone level",
      geom = "line")

latitude = seq(along = latitude.mean)
qplot(x = latitude,
      y = latitude.mean,
      ylab = "mean ozone level",
      geom = "line")

months = seq(along = time.mean)
qplot(x = months,
      y = time.mean,
      ylab = "mean ozone level",
      geom = "line",
      xlab = "months since January 1985")
