# Blood sugar monitoring data
# Script Claire Styles
# R version 4.0.3 (2020-10-10)

# Packages and settings required
library(dplyr)
library(ggplot2)

theme_set(theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5)))



# Core data ---------------------------------------------------------------

# Wide
sugarwide <- read.csv("sugar.csv", header = TRUE)
sugarwide$date <- as.Date(sugarwide$date, format = "%e/%m/%Y")
sugarwide$sug.dayavg <- apply(sugarwide[, c(2,3)], 1, mean)


# Long
rawlong <- reshape(sugarwide, varying=2:4, direction = "long")
sugarlong <- na.omit(rawlong[, -4])
row.names(sugarlong) <- c()
rm(rawlong)



# Rolling averages --------------------------------------------------------

# Average of last (complete) 7 days

avebyday <- na.omit(select(sugarwide, date, sug.dayavg))
avebyday$sug.7dayavg <- NA

for(i in 7:length(avebyday$sug.dayavg)){
  avrange <- (i-6):i
  rolldat <- avebyday[avrange,]
  avebyday$sug.7dayavg[i] <- mean(rolldat$sug.dayavg)
}


# Averages by readings: last 10, and cumulative
avebyread <- arrange(sugarlong, date, time) %>% filter(time == "am" | time == "pm")
avebyread$r10avg <- NA
avebyread$cmltv <- NA

for(i in 1:length(avebyread$sug)){
  cmltvrange <- 1:i
  cmltvdat <- avebyread[cmltvrange,]
  avebyread$cmltv[i] <- mean(cmltvdat$sug, na.rm = TRUE)
  
  if(i > 9){
    rollrange <- (i-9):i
    rolldat <- avebyread[rollrange,]
    avebyread$r10avg[i] <- mean(rolldat$sug, na.rm = TRUE)
  }
}

rm(cmltvrange, cmltvdat, rollrange, rolldat, i)


# Rolling average dataframe

rolling <- merge(avebyread[, c(1,4,5)], avebyday[, c(1,3)], all.x = TRUE)
colnames(rolling) <- c("date", "reads10", "cmltv", "days7")



# Plot functions and ref data ----------------------------------------------

line.data <- data.frame(yintercept = c(8,10), 
                        limits = c("fasting", "non-fasting"))

## Detailed line graph function with options for period of time

plotter <- function(df, type = "daily", numdays = "all"){
  # use numdays to specify time period in number of recent days
  
  # check that arguments are valid
  case <- tolower(type)
  stopifnot(case %in% c("daily", "rolling"))
  stopifnot(is.numeric(numdays) | numdays == "all")
  
  # data to use according to numdays specification
  if(numdays == "all"){
    daystart <- min(df$date)
    dat <- df
    interval <- "14 days"
  }
  
  else{
    daystart <- max(df$date)-numdays 
    stopifnot(daystart > min(df$date))
    dat <- subset(df, subset = date >= daystart)
    interval <- "7 days"
  }
  
  # variables for axis limits according to data subset to be plotted
  sugdat <- na.omit(switch(case, 
                           daily = dat[,3], 
                           rolling = Reduce(union, dat[, -1])))
  minax <- floor(min(sugdat)) - 0.5
  maxax <- ceiling(max(sugdat)) + 0.5
  
  # plot function
  
  switch(case,
         
         daily = ggplot(dat, aes(x = date, y = sug, colour = time)) + 
           geom_line(size = 1) + geom_point(size = 2) + 
           labs(title = "Daily blood sugar monitoring"),
         
         rolling = ggplot(dat, aes(x = date)) + 
           geom_line(data = dat[!is.na(dat$reads10),], 
                     aes(y = reads10, colour = "last 10 reads"), size = 1) + 
           geom_line(data = dat[!is.na(dat$days7),], 
                     aes(y = days7, colour = "last 7 days"), size = 1) + 
           geom_line(aes(y = cmltv, colour = "cumulative total"), size = 1) +
           scale_colour_manual("rolling average", 
                               values = c("black", "green2", "purple")) +
           labs(title = "Blood sugar rolling averages")) + 
    
    geom_hline(aes(yintercept = yintercept, linetype = limits), line.data) +  
    scale_x_date(breaks = seq(daystart, max(dat$date), 
                              by = interval), date_labels = "%e/%m/%Y") + 
    scale_y_continuous(limits = c(minax, maxax), 
                       breaks = seq(minax + 0.5, maxax - 0.5, 1),
                       labels = seq(minax + 0.5, maxax - 0.5, 1)) +
    labs(x = "Date", y = "Blood glucose (mmol/L)")
  
}


# Boxplot of all measurements
boxplot <- avebyread %>% select(time, sug) %>% 
  ggplot(aes(x = time, y = sug)) + 
  geom_boxplot(fill = "steelblue1", na.rm = TRUE) + 
  labs(title = "Summary of all available blood sugar measurements", 
       x = "Time", y = "Blood glucose (mmol/L)")



# Produce graphs ----------------------------------------------------------

# Daily measurements with daily average
print(plotter(sugarlong)) # all data
print(plotter(sugarlong, numdays = 30)) # last 30 days


# Rolling average graph
print(plotter(rolling, type = "rolling")) # all data
print(plotter(rolling, type = "rolling", numdays = 30)) # last 30 days


# Boxplot
print(boxplot)