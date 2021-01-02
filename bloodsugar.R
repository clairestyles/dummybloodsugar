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



# Graphs ------------------------------------------------------------------

maxam <- 8  # morning reading should be lower than
maxpm <- 10  # evening reading should be lower than
maxax <- ceiling(max(sugarlong$sug))
minax <- floor(min(sugarlong$sug))


# Daily measurements with daily average

dayplot <- ggplot(sugarlong, aes(x = date, y = sug, colour = time)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  scale_x_date(date_labels = "%e/%m/%Y", date_breaks = "7 days") +
  scale_y_continuous(limits = c(minax, maxax), breaks = seq(minax, maxax, 1),
                     labels = seq(minax, maxax, 1)) +
  labs(title = "Daily blood sugar readings", x = "Date",
       y = "Blood glucose (mmol/L)") +
  geom_line(aes(y = maxam), colour = "black", linetype = "dashed") + 
  geom_line(aes(y = maxpm), colour = "black", linetype = "dotted") + 
  geom_text(mapping = aes(x = start, y = maxam, label = "max AM"), vjust = 1.0, 
            show.legend = FALSE) + 
  geom_text(mapping = aes(x = start, y = maxpm, label = "max PM"), vjust = 1.0, 
            show.legend = FALSE) 

print(dayplot)  


# Rolling average graph

rollplot <- ggplot(rolling, aes(x = date)) + 
  geom_line(data = rolling[!is.na(rolling$reads10),], 
            aes(y = reads10, colour = "Last 10 reads")) + 
  geom_line(data = rolling[!is.na(rolling$days7),], 
            aes(y = days7, colour = "Last 7 days")) +
  geom_line(aes(y = maxam), colour = "black", linetype = "dashed") + 
  geom_line(aes(y = maxpm), colour = "black", linetype = "dotted") + 
  scale_colour_manual("Based on", values = c("green2", "purple")) + 
  scale_x_date(date_labels = "%e/%m/%Y", date_breaks = "7 days") +  
  labs(title = "Blood sugar rolling averages", x = "Date",
       y = "Blood glucose (mmol/L)")
  
print(rollplot)



