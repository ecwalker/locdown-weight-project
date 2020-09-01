library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(rstatix)

#Useful variables
st_2_kg = 6.35029
lbs_2_kg = 0.453592

#Import CSV file
lockdown_weight_df <- read_csv("lockdown_weight.csv")

#Weight to kg
lockdown_weight_df <- lockdown_weight_df %>% 
  mutate(weight_kg = weight_st*st_2_kg + weight_lbs*lbs_2_kg)

#Convert weight in kg to percentage change
wt_day_1 = lockdown_weight_df$weight_kg[1]
lockdown_weight_df <- lockdown_weight_df %>%
  mutate(weight_perc = weight_kg*100/wt_day_1)

#Remove redundant columns
wt_perc_df <- lockdown_weight_df[c("date",
                                   "cycle_day",
                                   "cycle_num",
                                   "weight_perc")]

#Export new CSV file
write_csv(wt_perc_df, "lockdown_weight_perc.csv")

#Import new CSV file
wt_perc_df <- read_csv("lockdown_weight_perc.csv")

#Date to YYYY-MM-DD format
wt_perc_df$date <- as.Date(wt_perc_df$date, "%d/%m/%y")

#Check for NA
total_na <- sum(is.na(wt_perc_df))
wt_na <- sum(is.na(wt_perc_df$weight_perc))

#Exclude NA
wt_perc_df <- wt_perc_df %>% drop_na()

#Change weight percentage to numeric
wt_perc_df <- wt_perc_df %>% 
  transform(weight_perc = as.numeric(weight_perc))

#Now admire our tidy dataframe
head(wt_perc_df)

#Simple line plot
wt_perc_df %>% ggplot(aes(x = date, y = weight_perc)) +
  geom_line() +
  labs(
    title = "My Weight between May and August 2020",
    x = "Date",
    y = "Weight as % of Initial"
  ) +
  scale_x_date(labels=date_format("%b %y"))

#Boxplot by cycle number
wt_perc_df %>% mutate(num = as.factor(cycle_num)) %>%
  ggplot(aes(x=num, y=weight_perc)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Weight for Each Cycle",
    x = "Cycle Number",
    y = "Weight as % of Initial"
    )

#QQ plots
wt_perc_df %>% 
  ggplot(aes(sample=weight_perc)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(cycle_num ~ .) +
  labs(
    title = "Comparing Distribution of Weight in Each Cycle with a Normal Distribution",
    x = "Theoretical Normal Distribution",
    y = "Weight as % of Initial"
  )

#Weight loss
median_wt <- wt_perc_df %>% group_by(cycle_num) %>%
  summarize(med_wt = median(weight_perc)) %>%
  transform(med_wt = as.numeric(med_wt))

loss = median_wt$med_wt[1] - median_wt$med_wt[5]

#PLot median weight of each cycle
wt_perc_df %>% group_by(cycle_num) %>%
  summarise(avg_wt = median(weight_perc)) %>%
  ggplot() +
  geom_line(aes(x = cycle_num, y = avg_wt)) +
  labs(
    title = "Median Weight per Cycle",
    x = "Cycle Number",
    y = "Median Weight as % of initial"
  )

#Checking significance
kruskal_df <- wt_perc_df %>% mutate(num=as.factor(cycle_num)) 
kruskal_test <- kruskal.test(kruskal_df$weight_perc ~ kruskal_df$num)

#Scatterplot of weight by cycle day
wt_perc_df %>% 
  mutate(num = as.factor(cycle_num)) %>%
  ggplot(aes(x = cycle_day, y = weight_perc, color = num)) +
  geom_point() +
  labs(
    title = "Weight % Plotted Against Day of Cycle",
    x = "Cycle Day",
    y = "Weight as % of 2020-05-07",
    color = "Cycle Number"
  )

#Scatterplot of weight by cycle day, normalised for median cycle weight
rel_wt_df <- wt_perc_df %>% 
  mutate(rel_wt = weight_perc - median_wt$med_wt[cycle_num]) 

rel_wt_df %>% mutate(num = as.factor(cycle_num)) %>%
  ggplot(aes(x = cycle_day, y = rel_wt)) +
  geom_point(aes(color=num)) +
  labs(
    title= "Weight normalised with median of each cycle",
    x = "Cycle Day",
    y = "Relative Weight %",
    color = "Cycle Number"
  ) +
  geom_smooth()