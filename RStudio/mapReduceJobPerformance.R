require(MASS); require(ggplot2); library(scales); library(plyr);library(robustHD);library(reshape2)
hdfsPerf = read.csv("MapReduceJobPerformance.csv", header = TRUE)

percPoorGC = round(mean(hdfsPerf$PERC_GC_TIME >= 0.2),4) * 100
percPoorMem = round(mean(hdfsPerf$PERC_MEM_ALLOCATION >= 0.9),4) * 100
percPoorCPU = round(mean(hdfsPerf$PERC_CPU_USAGE >= 2),4) * 100

ggplot(hdfsPerf, aes(x = PERC_MEM_ALLOCATION, y = PERC_GC_TIME)) +
  geom_point(size = 0.6, color = 'blue') +
  geom_vline(xintercept = 0.9, color = 'red') +
  geom_hline(yintercept = 0.2, color = 'red') +
  xlim(0,1) + ylim(0,0.5) +
  labs(title = 'Fraction of time in GC vs. Fraction memory used', 
       x = paste('Fraction memory used (', percPoorMem, '% Poor Performing)', sep = ''), 
       y = paste('Fraction of time in GC (', percPoorGC, '% Poor Performing)', sep = ''))

ggplot(hdfsPerf, aes(x = PERC_CPU_USAGE)) +
  geom_histogram(binwidth = 0.2, fill = 'blue', color = 'white') +
  xlim(0,5) +
  labs(title = 'Used CPU / Allocated CPU', 
       x = paste('Used CPU / Allocated CPU (', percPoorCPU, '% Poor Usage)', sep = ''), 
       y = '')

ggplot(hdfsPerf, aes(x = PERC_CPU_USAGE)) +
  geom_histogram(binwidth = 0.2, fill = 'blue', color = 'white') +
  xlim(1.6,5) +
  labs(title = 'Used CPU / Allocated CPU ( >1.6 )', 
       x = paste('Used CPU / Allocated CPU (', percPoorCPU, '% Poor Usage)', sep = ''), 
       y = '')
  