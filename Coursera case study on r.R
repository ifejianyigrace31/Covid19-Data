

#install packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggpubr")

# load packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(skimr)
library(readr)
library (janitor)
library(dplyr)
library(RColorBrewer)
library(ggpubr)

#import dataset
daily_activity <- read_csv("C:/Users/HP/Desktop/Case_study in r/dailyActivity_merged.csv")
hourly_calories <- read_csv("C:/Users/HP/Desktop/Case_study in r/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("C:/Users/HP/Desktop/Case_study in r/hourlyIntensities_merged.csv")  
hourly_steps <- read_csv("C:/Users/HP/Desktop/Case_study in r/hourlySteps_merged.csv")
sleep_day <- read_csv("C:/Users/HP/Desktop/Case_study in r/sleepDay_merged.csv")


View(daily_activity)
View(hourly_calories)
View(hourly_intensities)
View(hourly_steps)
View(sleep_day)


# preview dataframes
head(daily_activity,5)
head(hourly_calories,5)
head(hourly_intensities,5)
head(hourly_steps,5)
head(sleep_day,5)

str(daily_activity)
str(hourly_calories)
str(hourly_intensities)
str(hourly_steps)
str(sleep_day)

colnames(daily_activity)
colnames(hourly_calories)
colnames(hourly_intensities)
colnames(hourly_steps)
colnames(daily_sleep)

# clean column names
hourly_calories <- clean_names(hourly_calories)
hourly_intensities <- clean_names(hourly_intensities)
daily_activity <- clean_names(daily_activity)
daily_sleep <-clean_names(sleep_day)
hourly_steps <- clean_names(hourly_steps)

# cross-check
colnames(daily_activity)
colnames(hourly_calories)
colnames(hourly_intensities)
colnames(hourly_steps)
colnames(daily_sleep)



#unique users
n_unique(hourly_calories$id)
n_unique(hourly_steps$id)
n_unique(hourly_intensities$id)
n_unique(daily_sleep$id)
n_unique(daily_activity$id)



#duplicates
sum(duplicated(hourly_calories))
sum(duplicated(hourly_steps))
sum(duplicated(hourly_intensities))
sum(duplicated(daily_sleep))
sum(duplicated(daily_activity))


#remove duplicates and null values
hourly_calories <- hourly_calories %>%
  distinct() %>%
  drop_na()

hourly_steps <- hourly_steps %>%
  distinct() %>%
  drop_na()

hourly_intensities <- hourly_intensities %>%
  distinct() %>%
  drop_na()

daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_sleep <- daily_sleep %>%
  distinct() %>%
  drop_na()



# verify duplicates
sum(duplicated(hourly_calories))
sum(duplicated(hourly_steps))
sum(duplicated(hourly_intensities))
sum(duplicated(daily_sleep))
sum(duplicated(daily_activity))

sum(is.na(hourly_calories))
sum(is.na(hourly_steps))
sum(is.na(hourly_intensities))
sum(is.na(daily_sleep))
sum(is.na(daily_activity))


# formatting date and time
#rename columns

hourly_calories <- hourly_calories %>%
  rename(date_time = activity_hour)

hourly_steps <- hourly_steps %>%
  rename(date_time = activity_hour)

hourly_intensities <- hourly_intensities %>%
  rename(date_time = activity_hour)

daily_activity <- daily_activity %>%
  rename(date = activity_date) 

daily_sleep <- daily_sleep %>% 
  rename(date = sleep_day)



#formatting the data type of the date and time columns
hourly_calories$date_time=as.POSIXct(hourly_calories$date_time, format = "%m/%d/%Y %I:%M:%S %p",tz = Sys.timezone())

hourly_intensities$date_timer=as.POSIXct(hourly_calories$date_time, format = "%m/%d/%Y %I:%M:%S %p",tz = Sys.timezone())

hourly_steps$date_time=as.POSIXct(hourly_calories$date_time, format = "%m/%d/%Y %I:%M:%S %p",tz = Sys.timezone())

daily_sleep$date=as.POSIXct(daily_sleep$date, format = "%m/%d/%Y %I:%M:%S %p",tz = Sys.timezone())

daily_activity$date=as.POSIXct(daily_activity$date, format = "%m/%d/%Y")

head(daily_activity,5)
head(hourly_calories,5)
head(hourly_intensities,5)
head(hourly_steps,5)
head(daily_sleep,5)

###obtaining Day of Week from date
daily_activity <- daily_activity %>% 
  mutate(Day = format(ymd(date), format = '%a'))



## merging dataframes
# merge daily_activity and daily_sleep dataframes
daily_activity_sleep <- merge(daily_activity, daily_sleep, by= c("id", "date"))

glimpse(daily_activity_sleep)
head(daily_activity_sleep)
n_distinct(daily_activity_sleep$id)




# analyse phase
#category of users
#user average daily steps, minutes asleep and calories burnt
# classification of users based on daily average steps,users were  categorized according to National center for biotechnology information https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5488109/ 
#into five categories
#Steps per day	Classification
#<5000	Sedentary lifestyle
#5000–7499	Physically inactive
#7500–9999	Moderately active
#≥10,000	Physically active
#≥12,500	Very active
#We will now classify our users by the daily average steps
user_type_by_steps <- daily_activity_sleep   %>%  
  group_by(id) %>% 
  summarise(average_steps = mean(total_steps)) %>% 
  mutate(user_type = case_when(average_steps >= 12500 ~ "Highly Active",
                              average_steps >= 10000 ~ "Active",
                              average_steps >= 7500 ~ "Fairly Active" ,
                              average_steps >= 5000  ~ "Low Active", 
                              average_steps < 5000 ~ "Sedentary"),
        user_type = factor(user_type, levels = c("Sedentary", 'Low Active', "Fairly Active", "Active", "Highly Active")))

head(user_type_by_steps_percent)
         

### to visualize on a pie chart for user type we create a dataframe with the percentage of each user type
user_type_by_steps_percent<- user_type_by_steps%>%
   group_by(user_type) %>% summarise(users =n())%>%
   mutate(total_users = sum(users))%>%  group_by(user_type)%>% 
   summarise(percent =(users/total_users))%>%
   mutate(labels = scales::percent(percent))

head(user_type_by_steps_percent)
##visualize on a pie chart

user_type_by_steps_percent%>%
ggplot(aes(x="", y = percent, fill = user_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_brewer()+
  theme(  axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels, x= 1.2),
                position = position_stack(vjust = 0.6),
                color = "black") +
  labs(title = " User type by Steps") +
    guides(fill = guide_legend(title = "User Type"))
  options(repr.plot.width = 1, repr.plot.height = 1)


##Daily use of smart device; usage type distribution; how the smart device was used by the participants
  min(daily_activity$total_steps)
  max(daily_activity$total_steps)
  mean(daily_activity$total_steps)
  
##the average total steps is 7637.91,
#lets assume that participants didn't make use of their smart watches on days with the total steps <200 so they are filtered out.
##We assign the following usage types;
  #Low use - 1- 10 days
  #moderate use - 11-20 days
  #high use  21-31 days
  
  daily_usage_group <- daily_activity %>%
    filter(total_steps >200 ) %>% 
    group_by(id) %>%
    summarize(date=sum(n())) %>%
    mutate(usage = case_when(
      date >= 1 & date <= 10 ~ "Low Use",
      date >= 11 & date <= 20 ~ "Moderate Use", 
      date >= 21 & date <= 31 ~ "High Use")) %>% 
    mutate(usage = factor(usage, level = c('High Use','Moderate Use', 'Low Use'))) %>% 
    rename(daysused = date) %>% 
    group_by(usage)
 
  head(daily_usage_group)
  
### to visualize on a pie chart for user type we create a dataframe 
  daily_usage_group_pc<- daily_usage_group%>%
     group_by(usage) %>%
     summarise(participants =n())%>%
     mutate(total_participants=sum(participants))%>%
    group_by(usage)%>%
     summarise(percent= participants/total_participants)%>% 
     arrange(percent)%>% 
     mutate(labels = scales::percent(percent))
  
     head(daily_usage_group_pc)
     
     #visualizing on pie chart

     daily_usage_group_pc%>%
  ggplot(aes(x="", y = percent, fill =usage)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
      theme(  axis.ticks = element_blank(), 
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels, x= 1.2),
            position = position_stack(vjust = 0.6)) +
 scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"),
                  labels = c("High use - 21 to 31 days",
                             "Moderate use - 11 to 20 days",
                             "Low use - 1 to 10 days"))+
      labs(title = " Daily use of Smart devive") 
##Observations: 73% of users who frequently used their devices between 22 - 31 days.
#This makes up 24 out of 33 participants.:::12% of users who moderately used their devices between 15 - 21 days.
#This makes up 7 out of 33 participants.:::6% of users who used their devices least frequently between 1- 14 days.
#This makes up 2 out of 33 participants.:::A large majority of users use the device frequently between 22-31 days.
#I believe this reflects an innate understanding of the nature of use of the device. 

#Average steps by day: leftjoin daily activity with usage groups df
daily_activity_usage <- daily_activity%>%
  left_join(daily_usage_group, by= 'id')%>%
  mutate(day= format(ymd(date), format = '%a'))%>%
  mutate(total_mintutes_worn = sedentary_minutes+lightly_active_minutes+fairly_active_minutes+very_active_minutes)%>%
  mutate(total_hours= seconds_to_period(total_mintutes_worn*60))

head(daily_activity_usage)


# average steps per day df

steps_per_day<- daily_activity_usage%>%
  group_by(day)%>%
  summarise(average_steps = round(mean(total_steps)))%>%
  mutate(day= factor(day, levels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')))

head(steps_per_day)

#Viz for average steps per day
ggplot(steps_per_day, aes(x=day, y= average_steps, fill= average_steps))+
  geom_col(color="darkblue", size = 0.1) +  
  geom_hline(yintercept = 7500)+
  scale_fill_gradientn(limits=c(0,10000), breaks=seq(0,10000, by = 2500), 
                       colours = brewer.pal(9, "Blues")) +
  scale_y_continuous(limits=c(0,10000), breaks=seq(0, 10000, by = 2500))+ 
  labs(title= ("Average Steps"), subtitle = ('By Day'), x="" , y="Calories")+
  theme(plot.title=element_text(size = 16,hjust = 0))+
  theme(plot.subtitle=element_text(size = 14,hjust = 0))+
  theme(axis.text.y=element_text(size=14)) +
  theme(axis.text.x=element_text(size=14,hjust= 0.5))+
  theme(axis.title.x = element_text(margin = margin(t = 14, r = 0, b = 0, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(legend.position = "top")+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=8))+
  guides(fill = guide_colourbar(barwidth = 12))
options(repr.plot.width = 10, repr.plot.height = 8)

##--Tuesday and Saturday are recorded the most active days with Sunday with
#the lowest number of steps unsurprisingly a rest day
#Average steps by usage groups



steps_by_ug<- daily_activity_usage%>%
  group_by(day, usage)%>%
select(usage, total_steps,day)%>%
  mutate(day= factor(day, level=c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')))

head(steps_by_ug)

#boxplot for the average steps per usage group
ggplot(steps_by_ug, aes(x= , y=total_steps, fill=usage))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,40000), breaks = seq(0,40000, by =5000))+
  theme(legend.position = "none", plot.title = element_text(size = 11))+
  ggtitle("A boxplot with jitter")+
  xlab(" ")+
  labs(title = ("Average Steps"), subtitle = ('By Usage Group'), x=" ", y="Steps")+
  theme(plot.title=element_text(size = 16,hjust = 0))+
  theme(plot.subtitle=element_text(size = 14,hjust = 0))+
  theme(axis.text.y=element_text(size=14)) +
  theme(axis.text.x=element_text(size=14,hjust= 0.5))+
  theme(axis.title.x = element_text(margin = margin(t = 14, r = 0, b = 0, l = 0)))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  theme(legend.position = "top")+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=8))+
  facet_grid(~usage)
options(repr.plot.width = 5, repr.plot.height = 6)
  
## the high use group has the highest number of steps with an average of 9500 steps
## Hourly steps through the day
hourly_steps<-  select (hourly_steps,-2,-5)
hourly_steps<- hourly_steps%>%
  separate(date_time, into = c("date", "time"), sep= " ")%>%
  mutate(date= ymd(date))

head(hourly_steps)

##hourly steps through the day

hourly_steps%>%
  group_by(time)%>%
  summarise(average_steps = mean(step_total))%>%
  ggplot()+
geom_col(mapping = aes(x=time, y= average_steps, fill= average_steps))+
  labs(title = "Hourly steps throughout the day", x="", y="")+
  scale_fill_gradient(low = "red", high = "green")+
  theme(axis.text.x= element_text(angle = 90))

#most active hours is between 8am to 7pm, more steps during lunch time(12pm-2pm) and evenings(5pm-7pm) 

##correlation between daily steps and calories
ggplot(data=daily_activity)+
  geom_smooth(mapping=aes(x= total_steps,y= calories))+
   geom_point(mapping=aes(x=   total_steps,y= calories))+
  labs(title = "Daily steps vs Calories", x= "calories",y= "total_steps")+
      theme(panel.background = element_blank(), plot.title = element_text(size = 14))

##observation; their is a positive correlation between the total steps and calories burnt.



#Daily use of smart device in minutes
#WE want to know how long each user had their smart device on per day
#daily activity and daily usage group df will be merged 

daily_usage_group_merged<- merge(daily_activity,daily_usage_group, by = c("id"))
head(daily_usage_group_merged)

# There are 1440 minutes in a day, sum: veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes and sedentaryminutes to get of the total minutes the smart device was used by the user
minutes_worn <- daily_usage_group_merged %>% 
  mutate(total_minutes_worn = very_active_minutes+fairly_active_minutes+lightly_active_minutes+sedentary_minutes)%>%
  mutate (minutes_worn_percent = (total_minutes_worn/1440)*100) %>%
  mutate (worn = case_when(
    minutes_worn_percent == 100 ~ "All day",
    minutes_worn_percent < 100 & minutes_worn_percent >= 50~ "More than half day", 
    minutes_worn_percent < 50 & minutes_worn_percent > 0 ~ "Less than half day"
  ))

head(minutes_worn)



### to visualize on a pie chart for user type we create a dataframe to show the total users and will calculate percentage of minutes worn
total_minutes_worn_percent<- minutes_worn%>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

total_minutes_worn_percent$worn <- factor(total_minutes_worn_percent$worn, levels = c("All day", "More than half day", "Less than half day"))

head(total_minutes_worn_percent)


#further analysis; lets us check how each of the usage group had their smart device on per day
#we create a dataframe for each of the usage group


minutes_worn_high_use <- minutes_worn%>%
  filter (usage == "High Use")%>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_high_use$worn <- factor(minutes_worn_high_use$worn, levels = c("All day", "More than half day", "Less than half day"))


minutes_worn_moderate_use <- minutes_worn%>%
  filter(usage == "Moderate Use") %>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_moderate_use$worn <- factor(minutes_worn_moderate_use$worn, levels = c("All day", "More than half day", "Less than half day"))


minutes_worn_low_use <- minutes_worn%>%
  filter (usage == "Low Use") %>%
  group_by(worn) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(worn) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

minutes_worn_low_use$worn <- factor(minutes_worn_low_use$worn, levels = c("All day", "More than half day", "Less than half day"))



head(minutes_worn_high_use)
head(minutes_worn_moderate_use)
head(minutes_worn_low_use)

##visualizing the results on pie charts
ggarrange(
  ggplot(total_minutes_worn_percent, aes(x="",y=total_percent, fill=worn)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size = 3.5)+
  labs(title="Time worn per day", subtitle = "Total Users"),
ggarrange(
    ggplot(minutes_worn_high_use, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5), 
            legend.position = "none")+
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                      position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "High use - Users"), 
    
    ggplot(minutes_worn_moderate_use, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Moderate use - Users"), 
    
    ggplot(minutes_worn_low_use, aes(x="",y=total_percent, fill=worn)) +
      geom_bar(stat = "identity", width = 1)+
      coord_polar("y", start=0)+
      theme_minimal()+
      theme(axis.title.x= element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(), 
            panel.grid = element_blank(), 
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size=14, face = "bold"), 
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none") +
      scale_fill_manual(values = c("#004d99", "#3399ff", "#cce6ff"))+
      geom_text(aes(label = labels),
                position = position_stack(vjust = 0.5), size = 3)+
      labs(title="", subtitle = "Low use - Users"), 
    ncol = 3), 
  nrow = 2)


# we can see that about 50% of the total users wore the smart device all day, 47% more than half day and 3% less than half day
#Let's reminder that
#"High use - 21 to 31 days",
#"Moderate use - 11 to 20 days",
#"Low use - 1 to 10 days

#High users- 45.8% of the users who used the devices for 21 to 31 days used their smart devices all day, 51.7% used it more than half a day.
#moderate users- wear the device more on a daily basis
#Low users- they mostly wear the device all day on the day they use it, the graph show that they do not wear the device less tahn half a day

## 6. Recommendations
#1. To enhance user experiences give real-time feedback to users - providing regular daily (even hourly) engagement with users so that they can make quick informed decisions to optimize their routine and through this product involvement increases.

#2. Device's Accuracy: Improving a product's accuracy in measurement is crucial to any fitness device, such features as sensor, connectivity and charging. This will help prevent lost meaningful data.

#3. Product design: modify smart device so it can be worn all day to fit activities without interfering, modifications such as water resistance, long lasting batteries and fashionable.

#4. Further Research: Surveys to be sent out at intervals in order to gain a more in-depth knowledge of consumer behavior and preferences such as:
  
 # * Consumers' primary interest to pick up a fitness-focused wearable device (e.g, preference of functions)?
# * Is there an issue with the ease of use of the device? i.e, syncing, integrating, charging, comfortableness for the user to sleep on with
