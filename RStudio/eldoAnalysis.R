require(MASS); require(ggplot2); library(scales)
library(plyr);library(robustHD);library(reshape2)
PROD_wNA = read.csv("ELDO_HIST.csv", header = TRUE)
PROD = na.omit(PROD_wNA) # Dataframe without NA/NULL values
PERC_NULL = (dim(PROD_wNA)[1]-dim(PROD)[1])/dim(PROD_wNA)[1] # Percentage of records with NULL values

PROD$DATA_RATE = PROD$DATA_SIZE/PROD$JOB_DURATION_MINUTES # Added column Datasize/JobDuration (DATA_RATE)
PROD$RUN_TIME_BIN = cut(PROD$JOB_DURATION_MINUTES, 
                        breaks = c(seq(0,10, by = 0.5),200), 
                        labels = paste(as.character(seq(0, 10, by = 0.5)),'-',
                                       as.character(c(seq(0.5, 10, by = 0.5),200)),sep = ''))

computeMetrics <- function(data){
  # Compute JOB_DURATION metrics
  avgDuration = mean(data$JOB_DURATION_MINUTES)
  stdDuration = sd(data$JOB_DURATION_MINUTES)
  medianDuration = median(data$JOB_DURATION_MINUTES)
  percLongDuration = mean(data$JOB_DURATION_MINUTES > avgDuration+stdDuration)
  
  print(deparse(substitute(data)))
  
  print(paste("Average Runtime =",avgDuration))
  print(paste("Standard Deviation Runtime =", stdDuration))
  print(paste("Median Runtime= ", medianDuration))
  print(paste("Percentage of jobs running above standard deviation runtime =", percLongDuration))
  
  # Compute DATA_SIZE metrics
  avgSize = mean(data$DATA_SIZE)
  stdSize = sd(data$DATA_SIZE)
  medianSize = median(data$DATA_SIZE)
  percBigData = mean(data$DATA_SIZE > avgSize+stdSize)
  
  print(paste("Average Data Size =",avgSize))
  print(paste("Standard Deviation Data Size =", stdSize))
  print(paste("Median Data Size =", medianSize))
  print(paste("Percentage of jobs with greater than standard deviation datasize =", percBigData))
  
  # Compute DATA_RATE metrics
  avgRate = mean(data$DATA_RATE)
  stdRate = sd(data$DATA_RATE)
  medianRate = median(data$DATA_RATE)
  percBadPerformance = mean(data$DATA_RATE < avgRate-stdRate)
  
  print(paste("Average data consumption rate =",avgRate))
  print(paste("Standard Deviation data consumption rate =", stdRate))
  print(paste("Median data consumption rate =", medianRate))
  
  # Compute NUM Mappers metrics
  avgMaps = mean(data$NUM_OF_MAPPERS)
  stdMaps = sd(data$NUM_OF_MAPPERS)
  medianMaps = median(data$NUM_OF_MAPPERS)
  
  print(paste("Average number of mappers =", avgMaps))
  print(paste("Standard deviation mappers = ", stdMaps))
  print(paste("Median mappers = ", medianMaps))
  
  # Compute NUM Reducers metrics
  avgRed = mean(data$NUM_OF_REDUCERS)
  stdRed = sd(data$NUM_OF_REDUCERS)
  medianRed = median(data$NUM_OF_REDUCERS)
  
  print(paste("Average number of reducers =", avgRed))
  print(paste("Standard deviation reducers = ", stdRed))
  print(paste("Median reducers = ", medianRed))
  
}


dataRate <- function(data){
  ggplot(data = data, aes(x = DATA_SIZE, y = JOB_DURATION_MINUTES, color = RUN_TIME_BIN)) + 
    geom_point(size = 0.7) + 
    geom_smooth() +
    labs(title = paste("Runtime vs Data Size", deparse(substitute(data))), 
         x = "Data Size", y = "Runtime (min)")
}

dataMappers <- function(data){
  ggplot(data = data, aes(x = NUM_OF_MAPPERS, y = DATA_SIZE, color = RUN_TIME_BIN)) +
    geom_point(size = 0.7) + 
    stat_summary(fun.y = mean, color = 'black', geom = 'line') +
    labs(title = paste("Data Size vs Number of Mappers", deparse(substitute(data))), 
         x = "Mappers", y = "Data Size")
}

base_breaks <- function(n = 10){
  function(x){
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

breaks <- axTicks(side = 2)

dataRateLog <- function(data){
  ggplot(data = data, aes(x = DATA_SIZE, y = JOB_DURATION_MINUTES, color = RUN_TIME_BIN)) + 
    geom_point() + 
    #geom_smooth() +
    scale_x_continuous(trans = log_trans(), breaks = base_breaks(),
                       labels = prettyNum) +
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),
                       labels = prettyNum) +
    labs(title = paste("Runtime vs Data Size (log)", deparse(substitute(data))), 
         x = "Data Size", y = "Runtime (min)")
}

dataMappersLog <- function(data){
  ggplot(data = data, aes(x = NUM_OF_MAPPERS, y = DATA_SIZE, color = RUN_TIME_BIN)) +
    geom_point(size = 0.7) +
    #geom_smooth() +
    scale_x_continuous(trans = log_trans(), breaks = base_breaks(),
                       labels = prettyNum) +
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),
                       labels = prettyNum) +
    labs(title = paste("Data Size vs Number of Mappers (log)", deparse(substitute(data))), 
         x = "Mappers", y = "Data Size")
}




# Windsorized Data
winPROD = PROD
winPROD$DATA_SIZE = winsorize(winPROD$DATA_SIZE)
winPROD$JOB_DURATION_MINUTES = winsorize(winPROD$JOB_DURATION_MINUTES)
winPROD$NUM_OF_MAPPERS = winsorize(winPROD$NUM_OF_MAPPERS)
winPROD$NUM_OF_REDUCERS = winsorize(winPROD$NUM_OF_REDUCERS)
winPROD$DATA_RATE = winPROD$DATA_SIZE/winPROD$JOB_DURATION_MINUTES

# No Long Outliers (Long running jobs)
noLongPROD_ind = (PROD$JOB_DURATION_MINUTES < 9)
longPROD_ind = (PROD$JOB_DURATION_MINUTES > 9)
longPROD = PROD[longPROD_ind,]
noLongPROD = PROD[noLongPROD_ind,]

# No Fast Outliers (Fast DATA RATES)
noFastPROD_ind = (noLongPROD$DATA_RATE < 1e9 & noLongPROD$DATA_SIZE > 0)
noFastPROD = noLongPROD[noFastPROD_ind,]


# Mappers Vs Reducers
mapsRed <- function(data){
  ggplot(data = data, aes(x = NUM_OF_REDUCERS, y = NUM_OF_MAPPERS, color = RUN_TIME_BIN)) + 
    geom_point(size = 0.7) + 
    stat_summary(fun.y = mean, color = 'black', geom = 'line') +
    labs(title = paste("Mappers vs Reducers", deparse(substitute(data))), 
         x = "Reducers", y = "Mappers")
}

# Mappers Vs Reducers
mapsRedLog <- function(data){
  ggplot(data = data, aes(x = NUM_OF_REDUCERS, y = NUM_OF_MAPPERS, color = RUN_TIME_BIN)) + 
    geom_point(size = 0.7, stat = 'summary', fun.y = median) + 
    stat_summary(fun.y = median, color = 'black', geom = 'line') +
    # scale_x_continuous(trans = log_trans(), breaks = base_breaks(),
    #                    labels = prettyNum) +
    scale_y_continuous(trans = log_trans(), breaks = base_breaks(),
                       labels = prettyNum) +
    labs(title = paste("Mappers vs Reducers", deparse(substitute(data))), 
         x = "Reducers", y = "Mappers")
}

dataRateHist <- function(data){
  ggplot(data, aes(x = DATA_RATE)) +
    geom_histogram(bins = 10, color = 'gold', fill = 'black') + 
    #xlim(0,2e11) + ylim(0,750) +
    labs(title = paste('Data Rate Histogram',deparse(substitute(data)))) +
    theme(panel.background = element_rect(fill = 'darkturquoise'))
}

threeWay <- function(data, dataSizeMin, dataSizeMax, dataRateMin, dataRateMax){
  ggplot(data, aes(x = DATA_SIZE, y = DATA_RATE, color = RUN_TIME_BIN)) + 
    geom_point(size = 0.7) + 
    #geom_smooth() +
    xlim(dataSizeMin,dataSizeMax) + 
    ylim(dataRateMin,dataRateMax) +
    labs(title = paste('Data Rate vs Data Size', deparse(substitute(data))), 
      x = 'Data Size (Bytes)', y = 'Data Rate (Byte/Min)')
}

# dataRateHist(noLongPROD)

# mapsRed(PROD)
# mapsRed(winPROD)
# mapsRedLog(PROD)
# mapsRedLog(winPROD)

# dataRate(PROD)
# dataMappers(PROD)
# dataRateLog(PROD)
# dataMappersLog(PROD)

# dataRate(longPROD)
# dataMappers(longPROD)
# dataRateLog(longPROD)
# dataMappersLog(longPROD)


computeMetrics(PROD)
computeMetrics(noLongPROD)



