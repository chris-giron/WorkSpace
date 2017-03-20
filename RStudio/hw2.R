### Code for CSE-6242 (Data and Visual Analytics) Homework 2 ###
# dgiron3
# =============================== #
# Problem 1 #
library(ggplot2)
library(plyr)
data("midwest")

prof_employ <- function(){
  # Create new data frame to include only necessary columns (state, popadults, percprof) ...
  # ... then create new column with population of people with professional employment
  mw.prof = midwest[,c("state","popadults","percprof")]
  mw.prof$proftotal = mw.prof$popadults*mw.prof$percprof/100
  
  # Create new data frame with grouping by state with populations sums (popadults, proftotal)...
  # ... then create new aggregated percentage of professional employment column
  mw.prof.group = ddply(mw.prof, ~state, summarize, popadults=sum(popadults), proftotal=sum(proftotal))
  mw.prof.group$percprof = round(mw.prof.group$proftotal/mw.prof.group$popadults*100, 1)
  
  
  # Graphical representation
  ggplot(data = mw.prof.group, aes(x = reorder(state,percprof), percprof)) +
    geom_bar(stat = "identity", fill = 'white', color = 'blue') +
    geom_text(aes(label = percprof), vjust = 1.6, color = 'blue', size = 3.5) +
    labs(x = 'State', y = 'Professional Employment (%)', title = 'Professional Employment by State')
}

prof_employ()

# =============================== #
# Problem 2 #
library(reshape2)
percedu <- function(){
  # Create new data frame to include only necessary columns (state, popadults, perchsd, percollege) ...
  # ... then create new column with population of people high school and college educations
  mw.edu = midwest[,c("state","popadults","perchsd","percollege")]
  mw.edu$hsdtotal = mw.edu$popadults*mw.edu$perchsd/100
  mw.edu$collegetotal = mw.edu$popadults*mw.edu$percollege/100
  
  # Create new data frame with grouping by state with populations sums (popadults, hsdtotal, college total)...
  # ... then create new aggregated percentage of high school and college education columns
  mw.edu.group = ddply(mw.edu, ~state, summarize, popadults=sum(popadults), hsdtotal=sum(hsdtotal), collegetotal=sum(collegetotal))
  mw.edu.group$HS = round(mw.edu.group$hsdtotal/mw.edu.group$popadults*100, 1)
  mw.edu.group$COLLEGE = round(mw.edu.group$collegetotal/mw.edu.group$popadults*100, 1)
  
  # Reformate data to be in long format
  mw.edu.group.long <- melt(mw.edu.group,id.vars=c("state","popadults","hsdtotal","collegetotal"))
  
  # Graphical representation
  ggplot(data = mw.edu.group.long, aes(x = reorder(state, value), y = reorder(value, value), fill = factor(variable))) +
    geom_bar(position = 'dodge', stat = 'identity') + 
    geom_text(aes(label = value), vjust = 1.6, color = 'white', position = position_dodge(0.9), size = 3.5) +
    scale_fill_discrete(name = 'Level Edu') +
    labs(x = 'State', y = 'Education (%)', title = 'Education by State')
  
}

percedu()

# =============================== #
# Problem 3 #
boxhisto <- function(){
  SD20 = rnorm(500, mean = 0, sd = 20)
  SD50 = rnorm(500, mean = 0, sd = 50)
  N100 = rnorm(100, mean = 0, sd = 50)
  N50 = rnorm(50, mean = 0, sd = 50)
  test_data = data.frame(SD20, SD50, N100, N50)
  boxplot(test_data, main = "Box Plot Example (SD = 20/50)")
  hist(SD20, main = "Histogram Example (N = 500)", breaks = 20)
  hist(SD50, main = "Histogram Example (SD = 50)", breaks = 20)
  hist(N50, main = "Histogram Example (N = 50)", breaks = 20)
}

#boxhisto()

# =============================== #
# Problem 4 #
file_size_test <- function(nstart,nend){
  # Initiallize arrays (one for each file type and N array for history tracking and the final plot)
  n = nstart
  N = rep(0, times = 0)
  ps = rep(0, times = 0)
  pdf = rep(0, times = 0)
  jpeg = rep(0, times = 0)
  png = rep(0, times = 0)
  while(n<nend){
    # Generate random points using runif() function given the size of n
    x = runif(n)
    y = runif(n)
    # Tag a file size for each file type given the size of n.  
    # File size will be placed into each file type array respectively
    jpeg('jpeg.jpeg')
    plot(x,y,main = paste("jpeg: N =",n))
    dev.off()
    jpeg = c(jpeg,file.size("jpeg.jpeg"))
    pdf('pdf.pdf')
    plot(x,y,main = paste("pdf: N =",n))
    dev.off()
    pdf = c(pdf,file.size("pdf.pdf"))
    postscript('ps.ps')
    plot(x,y,main = paste("ps: N =",n))
    dev.off()
    ps = c(ps,file.size("ps.ps"))
    png('png.png')
    plot(x,y,main = paste("png: N =",n))
    dev.off()
    png = c(png,file.size("png.png"))
    # Include n into N array and increment N by 1000
    N = c(N,n)
    if(n == nstart){
      plot(x,y, main = paste("n =",n))
    }
    if(n == nstart+5000){
      plot(x,y, main = paste("n =",n))
    }
    if(n == nstart+70000){
      plot(x,y, main = paste("n =",n))
    }
    n = n + 1000
  }
  # Create data frame including all of the arrays
  file_size = data.frame(N = N, jpeg = jpeg, png = png, pdf = pdf, ps = ps)
  # plot the data of file size vs n
  ggplot(data = file_size, aes(N,y = Size, color = variable)) +
    geom_line(aes(y = jpeg, col = "jpeg")) +
    geom_line(aes(y = pdf, col = "pdf")) +
    geom_line(aes(y = ps, col = "ps")) +
    geom_line(aes(y = png, col = "png"))
}

#file_size_test(100,80000)

# =============================== #
# Problem 5 #
data("diamonds")
diamonds$numcolor = as.numeric(diamonds$color)

dmnds.colorhist <- function(){
  ggplot(data = diamonds, aes(x = numcolor)) + 
    geom_histogram(binwidth = 1, colour = 'white', fill = 'blue') +
    labs(x = 'Ordinal Color Value', title = 'Color Histogram')
}

dmnds.carathist <- function(){
  ggplot(data = diamonds, aes(x = carat)) +
    geom_histogram(binwidth = 0.25, colour = 'gold', fill = 'purple') +
    labs(x = 'carat', title = 'Carat Histogram')
}

dmnds.pricehist <- function(){
  ggplot(data = diamonds, aes(x = price)) +
    geom_histogram(binwidth = 1000, colour = 'gold', fill = 'forestgreen') +
    labs(x = 'price', title = 'Price Histogram')
}

dmnds.3way <- function(){
  ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point(colour = 'blue', size = 0.3) + facet_wrap(~color)
}

# dmnds.colorhist()
# dmnds.carathist()
# dmnds.pricehist()
# dmnds.3way()