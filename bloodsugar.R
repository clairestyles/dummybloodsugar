# Blood sugar monitoring data
# Script Claire Styles
# R version 4.0.3 (2020-10-10)

# Packages and settings required
library(tidyverse)

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

start <- as.Date("2020-11-21")

# 7-day average
sugarwide$sug.7dayavg <- NA

for(i in 1:length(sugarwide$date)){
  today <- sugarwide$date[i]
  weekstart <- today - 6
  if(weekstart >= start){
    rolldat <- sugarwide %>% filter(date %in% seq(weekstart, today, "days"))
    sugarwide$sug.7dayavg[i] <- mean(rolldat$sug.dayavg, na.rm = TRUE)
  }
}

for(i in 1:length(sugarwide$sug.7dayavg)){
  if(is.nan(sugarwide$sug.7dayavg[i])){
    sugarwide$sug.7dayavg[i] <- NA
  }
}

rm(today, weekstart)


# Average of last 10 readings
ave10 <- arrange(sugarlong, date, time) %>% filter(time == "am" | time == "pm")
ave10$r10avg <- NA

for(i in 10:length(ave10$sug)){
   avrange <- (i-9):i
   rolldat <- ave10[avrange,]
   ave10$r10avg[i] <- mean(rolldat$sug, na.rm = TRUE)
}
rm(rolldat, avrange, i)


# Rolling average dataframe

avebyday <- na.omit(select(sugarwide, date, sug.7dayavg))
avebyread <- na.omit(select(ave10, date, r10avg))
rolling <- merge(avebyread, avebyday, all.x = TRUE)
colnames(rolling) <- c("date", "reads10", "days7")




# Plot function and ref data ----------------------------------------------

maxam <- 8  # morning reading should be lower than
maxpm <- 10  # evening reading should be lower than
maxax <- ceiling(max(sugarlong$sug))
minax <- floor(min(sugarlong$sug))


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
  }
  
  else{
    daystart <- max(df$date)-numdays 
    stopifnot(daystart > min(df$date))
    dat <- subset(df, subset = date >= daystart)
  }
  
  # plot function
  
  switch(case,
         
         daily = ggplot(dat, aes(x = date, y = sug, colour = time)) + 
           geom_line(size = 1) + geom_point(size = 2) + 
           scale_y_continuous(limits = c(minax, maxax), 
                              breaks = seq(minax, maxax, 1),
                              labels = seq(minax, maxax, 1)),
         
         rolling = ggplot(dat, aes(x = date)) + 
           geom_line(data = dat[!is.na(dat$reads10),], 
                     aes(y = reads10, colour = "last 10 reads"), size = 1) + 
           geom_line(data = dat[!is.na(dat$days7),], 
                     aes(y = days7, colour = "last 7 days"), size = 1) + 
           scale_colour_manual("rolling average", 
                               values = c("green2", "purple"))) + 
    
    geom_line(aes(y = maxam), colour = "black", linetype = "dashed") + 
    geom_line(aes(y = maxpm), colour = "black", linetype = "dotted") + 
    scale_x_date(date_labels = "%e/%m/%Y", date_breaks = "7 days") + 
    geom_text(mapping = aes(x = daystart, y = maxam, label = "fasting max"), 
              vjust = 1.0, show.legend = FALSE) + 
    geom_text(mapping = aes(x = daystart, y = maxpm, label = "true max"), 
              vjust = 1.0, show.legend = FALSE) + 
    labs(title = "Blood sugar monitoring", x = "Date",
         y = "Blood glucose (mmol/L)")
  
}




# Produce graphs ----------------------------------------------------------

# Daily measurements with daily average
print(plotter(sugarlong)) # all data
print(plotter(sugarlong, numdays = 30)) # last 30 days


# Rolling average graph
print(plotter(rolling, type = "rolling")) # all data
print(plotter(rolling, type = "rolling", numdays = 30)) # last 30 days



