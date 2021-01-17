library(ggplot2)
library(dplyr)
setwd("C:/Users/Hank/Desktop/STAT 403")

# reads in the file
crime <- read.csv("Crime_Data.csv", header = T, stringsAsFactors = F)
nrow(crime)

# converts the reported date frome chr to Date
crime$Reported.Date <- as.Date(crime$Reported.Date, "%m/%d/%Y")

# filters for relevant dates
crime <- crime[crime$Reported.Date <= "2019-03-20" & crime$Reported.Date >= "2008-01-01", ]
nrow(crime)

# fills in the blanks with "UNKNOWN" label
crime$Crime.Subcategory[crime$Crime.Subcategory == ""] <- "UNKNOWN"

# crimes specifically in University District
university <- filter(crime, Neighborhood == "UNIVERSITY")

# retrieves the type of crimes
types_crime <- unique(crime$Crime.Subcategory)

# how many different types of crime were committed
number_of_types <- length(types_crime)

# grabs only the list of crimes by their label
all_crimes <- crime$Crime.Subcategory

m <- length(crime$Crime.Subcategory)
n <- length(university$Crime.Subcategory)

# creates a frequency table of the crimes from all of them
count_university_crime <- as.data.frame(table(university$Crime.Subcategory))

# bootstrap test
B <- 10000

# creates data frame to store the bootstrapped crimes in university district
crime_BT <- matrix(NA, nrow = B, ncol = number_of_types)
colnames(crime_BT) <- count_university_crime$Var1
crime_BT <- as.data.frame(crime_BT)
for(i in 1:B) {
  w = sample(m,n,replace=T)
  dat_BT = crime$Crime.Subcategory[w]
  freq_BT <- table(dat_BT)
  list_crimes <- as.data.frame(freq_BT)$dat_BT
  for(j in list_crimes) {
    crime_BT[i,j] <- freq_BT[[j]]
  }
}

# converts any crime with, turning it into 0
crime_BT[is.na(crime_BT)] <- 0
colMeans(crime_BT)


# basic plots for each type of crime
for(i in types_crime) {
  homi <- quantile(crime_BT[,i] / n, prob=c(.05 / length(types_crime), 1 - .05 / length(types_crime)))
  hist(crime_BT[,i]/ n, main = i, xlab = i)
  abline(v = count_university_crime[count_university_crime$Var1 == i, ]$Freq/ n, lw = 3, col = "red")
  abline(v = homi, col = c("green", "green"), lty = c(2, 2), lwd = c(3,3))
}

View(crime)
for(i in 1:ncol(crime_BT)){
  crime_BT[10001, i] <- quantile(crime_BT[,i] / n, prob = .05 /length(types_crime))
  crime_BT[10002, i] <- quantile(crime_BT[,i] / n, prob = 1 - .05 /length(types_crime))
  crime_BT[10003, i] <- i
}



crime_BT_transpose <- as.data.frame(t(crime_BT))
crime_BT_transpose[, 'crime_name'] <- rownames(crime_BT_transpose)
colnames(crime_BT_transpose)[c(10001, 10002)] <- c("q1", "q2")

plot <- ggplot(data = crime_BT_transpose[c("UNKNOWN", "ROBBERY-RESIDENTIAL", "ROBBERY-COMMERCIAL",
                                           "RAPE", "PROSTITUTION", "PORNOGRAPHY", "LOITERING", 
                                           "LIQUOR LAW VIOLATION", "HOMICIDE", "DISORDERLY CONDUCT",
                                           'ARSON', 'BURGLARY-COMMERCIAL-SECURE PARKING', "GAMBLE", 'WEAPON'),]) +
  geom_segment(aes(x = crime_name, xend = crime_name, 
                                              y = q1, yend = q2), col = 'black') +
  geom_point(aes(x = crime_name, y = q1), shape = "|", col = "black", size = 3) +
  geom_point(aes(x = crime_name, y = q2), shape = "|", col = "black", size = 3) +
  geom_point(data = count_university_crime[count_university_crime$Var1 %in% c("UNKNOWN", "ROBBERY-RESIDENTIAL", "ROBBERY-COMMERCIAL",
                                             "RAPE", "PROSTITUTION", "PORNOGRAPHY", "LOITERING", 
                                             "LIQUOR LAW VIOLATION", "HOMICIDE", "DISORDERLY CONDUCT",
                                             'ARSON', 'BURGLARY-COMMERCIAL-SECURE PARKING', "GAMBLE", 'WEAPON'), ], aes(x = Var1, y = Freq / n), col = "red", size = 2) +
  coord_flip() +
  labs(x = "Proportion of Crimes", y = "Type of Crime") +
  ggtitle("Bootstrapped Intervals of Crime Proportions (1)") +
  theme_bw()
plot

plot1 <- ggplot(data = crime_BT_transpose[c("TRESPASS", "THEFT-BUILDING", "THEFT-BICYCLE", "SEX OFFENSE-OTHER", "ROBBERY-STREET",
                                           "NARCOTIC", "FAMILY OFFENSE-NONVIOLENT", "DUI", "BURGLARY-RESIDENTIAL-SECURE PARKING",
                                           "BURGLARY-COMMERCIAL", "AGGRAVATED ASSAULT-DV", "AGGRAVATED ASSAULT"), ]) +
  geom_segment(aes(x = crime_name, xend = crime_name, 
                   y = q1, yend = q2), col = 'black') +
  geom_point(aes(x = crime_name, y = q1), shape = "|", col = "black", size = 3) +
  geom_point(aes(x = crime_name, y = q2), shape = "|", col = "black", size = 3) +
  geom_point(data = count_university_crime[count_university_crime$Var1 %in% c("TRESPASS", "THEFT-BUILDING", "THEFT-BICYCLE", "SEX OFFENSE-OTHER", 
                                                                              "ROBBERY-STREET",
                                                        "NARCOTIC", "FAMILY OFFENSE-NONVIOLENT", "DUI", "BURGLARY-RESIDENTIAL-SECURE PARKING",
                                                        "BURGLARY-COMMERCIAL", "AGGRAVATED ASSAULT-DV", "AGGRAVATED ASSAULT"),], aes(x = Var1, y = Freq / n), col = "red", size = 2) +
  coord_flip() +
  labs(x = "Proportion of Crimes", y = "Type of Crime") +
  ggtitle("Bootstrapped Intervals of Crime Proportions (2)") +
  theme_bw()
plot1

plot2 <- ggplot(data = crime_BT_transpose[c("THEFT-SHOPLIFT", "THEFT-ALL OTHER", "MOTOR VEHICLE THEFT",
                                            'CAR PROWL', 'BURGLARY-RESIDENTIAL'),]) +
  geom_segment(aes(x = crime_name, xend = crime_name, 
                   y = q1, yend = q2), col = 'black') +
  geom_point(aes(x = crime_name, y = q1), shape = "|", col = "black", size = 3) +
  geom_point(aes(x = crime_name, y = q2), shape = "|", col = "black", size = 3) +
  geom_point(data = count_university_crime[count_university_crime$Var1 %in% c("THEFT-SHOPLIFT", "THEFT-ALL OTHER", "MOTOR VEHICLE THEFT",
                                                                              'CAR PROWL', 'BURGLARY-RESIDENTIAL'),], aes(x = Var1, y = Freq / n), col = "red", size = 2) +
  coord_flip() +
  labs(x = "Proportion of Crimes", y = "Type of Crime") +
  ggtitle("Bootstrapped Intervals of Crime Proportions (3)") +
  theme_bw()
plot2


plot1

