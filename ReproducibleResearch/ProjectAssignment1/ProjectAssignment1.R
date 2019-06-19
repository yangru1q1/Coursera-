########### load data ##################################

if(!file.exists("Data")) dir.create("Data")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, destfile = "./Data/activity.zip", method = "curl")
unzip("./Data/activity.zip", exdir = "./Data")

data <- read.csv("./Data/activity.csv")


################### exploratary analysis #############################

# step
head(data)
table(is.na(data$steps)) # 2304 missing value in steps column

# date
str(data$date)
data$date <- as.Date(data$date, "%Y-%m-%d")

library(tidyverse)

data1 <- data %>% group_by(date) %>%
        summarize(totalSteps = sum(steps, na.rm = TRUE))

# 1
data %>% group_by(date) %>%
        summarize(totalSteps = sum(steps, na.rm = TRUE)) %>%
        ggplot() +
        geom_histogram(aes(totalSteps), col = "red", fill = "blue") +
        labs(x = "Total Steps", 
             y = "Number of days", 
             title = "Total Steps Per Day",
             subtitle = "NA Value Ignored") +
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5))


data %>% group_by(date) %>%
        summarize(totalSteps = sum(steps, na.rm = TRUE)) %>%
        summarize(median = median(totalSteps), mean = mean(totalSteps))


# 2
data %>% group_by(interval) %>%
        summarize(meanSteps = mean(steps, na.rm=TRUE)) %>%
        ggplot() +
        geom_line(aes(x = interval, y = meanSteps), color = "blue") +
        labs(x = "Time Interval",
             y = "Mean Steps",
             title = "Mean Steps at Each Time Interval",
             subtitle = "NA Value Ignored") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))

df1<- data %>% group_by(interval) %>%
                summarize(meanSteps = mean(steps, na.rm=TRUE)) 
df1 %>% filter(meanSteps == max(meanSteps))


# 3
df2 <- data

df2 <- df2 %>% mutate(meanSteps = rep(df1$meanSteps, length(unique(df2$date))))
df2$steps <- ifelse(is.na(df2$steps), df2$meanSteps, df2$steps)
df2 <- df2 %>%
        select(steps, date, interval)
head(df2)

df2 %>% group_by(date) %>%
        summarize(totalSteps = sum(steps)) %>%
        ggplot() +
        geom_histogram(aes(x=totalSteps), col = "red", fill = "blue") +
        labs(x = "Total Steps",
             y = "Number of days",
             title = "Total Steps per Day",
             subtitle = "With Imputed NA Values") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))

df2 %>% group_by(date) %>%
        summarize(totalSteps = sum(steps)) %>%
        summarize(meanSteps = mean(totalSteps), 
                  medianSteps = median(totalSteps))

#
df2 %>% mutate(Day = ifelse(weekdays(date) %in% c("Saturday", "Sunday"),
                            "weekend", "weekday")) %>%
        group_by(Day, interval) %>%
        summarize(meanSteps = mean(steps)) %>%
        ggplot() +
        geom_line(aes(x = interval, y = meanSteps), color = "blue") +
        facet_grid(Day~.) +
        labs(x = "Interval",
             y = "Mean number of Steps",
             title = "Mean Number of Steps in each Time Interval",
             subtitle = "Weekday and Weekend") +
        theme_bw(base_family = "Times") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))


