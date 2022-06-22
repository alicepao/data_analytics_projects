---
title: "Case Study 2: Reducing Gun Deaths"
author: "Alice Pao"
date: "May 14, 2022"
output: 
  html_document:
    keep_md: TRUE
    toc: true
    toc_float: true
    code_folding: hide
---




```r
library(tidyverse)
library(gghighlight)
dat <- read_csv('https://raw.githubusercontent.com/fivethirtyeight/guns-data/master/full_data.csv')
```

### Background
***
You are working for a client who wants to create a marketing campaign that helps reduce gun deaths in the US. The client would like to identify several target audiences that could benefit from such a campaign, as well identify any seasonal trends in gun deaths in these target audiences.

### Introduction 
***
This project, we dived into the causes and different demographics of gun shot deaths. We also determined the two main target audiences: 15-29 years old male and female high school students. 

### Graph Insights
***
The first graph shows that the majority of gun deaths victims are male. The largest gun deaths group is caused by suicide. The second largest gun deaths group is a result of homicide. Only a small portion of women died from gun usage. 


```r
dat %>% 
  ggplot()+
  geom_bar(aes(x=intent, fill=sex), position="dodge")+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("#d8b365", "#5ab4ac"))+
  coord_flip()+
  labs(
    title = "Gun Shot Deaths Summary", 
    subtitle = "1. Most gun shot deaths are male, committing suicide \n2. Second gun shot deaths group is homicide \n3. Women are less likely to become gun shot victims", 
    y= "porportion", 
    fill= "gender", 
    caption = "data provided by FiveThirtyEight"
  )+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
```

![](Case-Study-2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The second graph shows that most homicide male deaths are young men aging 15-29.


```r
age_dat <- dat %>% 
  mutate(age_range = case_when(age > 60 ~ '60+', age >=45 & age <=59 ~ '45-59', age>=30 & age <=44 ~ '30-44', age>=15 & age<=29 ~ '15-29', age>=0 & age<=14 ~ '0-14'))
age_dat %>% 
  ggplot(aes(x=intent, fill=age_range))+
  geom_bar(position="dodge")+
  scale_fill_hue(c = 40)+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_discrete(name = "age range")+
  facet_wrap(.~sex)+
  labs(
    title = "Top 1 Homicide Male Victims Age Group", 
    subtitle = "Age 15-29", 
    caption = "data provided by FiveThirtyEight", 
    x= "intents", 
    y= "porportion"
  )+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
```

![](Case-Study-2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Target Audiences
***
After data analysis, we decided to choose _male aging 15-29_ & _female high school students_. 
From the first provided graph, we see that more than 30,000 young men who are 15-29 years old are gun death victims. 
As for female demographic, based on the summary bar graph, we noticed that the highest education most women dying from gun shots received is high school diploma. 
In conclusion, based on what we gathered and analyzed, we propose targeting High School girls and 15-29 year-old males. 


```r
race_dat <- dat %>% 
  group_by(race)

race_chart <- ggplot(race_dat)+
  geom_bar(aes(x=race, fill=race))
```



```r
gender_dat <- dat %>% 
  group_by(sex)

gender_chart <- ggplot(gender_dat)+
  geom_bar(aes(x=sex, fill=sex))
```



```r
intent_dat <- dat %>% 
  group_by(intent)

intent_chart <- ggplot(intent_dat)+
  geom_bar(aes(x=intent, fill=intent))
```



```r
age_dat <- dat %>% 
  mutate(age_range = case_when(age > 60 ~ '60+', age >=45 & age <=59 ~ '45-59', age>=30 & age <=44 ~ '30-44', age>=15 & age<=29 ~ '15-29', age>=0 & age<=14 ~ '0-14'))

age_chart <- ggplot(age_dat)+
  geom_bar(aes(x=age_range, fill=age_range))
```



```r
education_dat <- dat %>% 
  group_by(education)

education_chart <- ggplot(education_dat)+
  geom_bar(aes(x=education, fill=education))
```



```r
gender_age_dat <- age_dat %>% 
  group_by(sex, age_range)

gender_age_chart <- ggplot(gender_age_dat)+
  geom_bar(aes(x=age_range, fill=age_range))+
  scale_fill_discrete(name="age range")+
  facet_wrap(.~sex, ncol=2)+
  labs(
    title = "Male Gun Shot Victims By Age Group", 
    subtitle = "Target Audience 1: 15-29 years old Male", 
    x = "age range", 
    fill = "", 
    caption = "data provided by FiveThirtyEight"
  )+
  theme_bw()+
  theme(axis.title.y = element_blank())+
  # change bar color 
  scale_fill_hue(c = 40)+
  theme(legend.position = "top")
  
gender_age_chart
```

![](Case-Study-2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



```r
female_education_dat <- dat %>% 
  group_by(sex, education)

female_education_chart <- ggplot(female_education_dat)+
  geom_bar(aes(x=education, fill=education))+
  scale_fill_hue(c = 40)+
  labs(
    title = "Highest Education Received for Female Gun Shot Victims", 
    subtitle = "Targe Audience 2: Female High School Students", 
    caption = "data provided by FiveThirtyEight", 
    # remove legend name 
    fill = ""
  )+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  theme(legend.position = "top")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  theme(legend.position = "top")

female_education_chart
```

![](Case-Study-2_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Seasonal Trend 1: Target Audience 2 (Female High Schoolers)
***
From the collected data, we found out that starting from April to July, gun shot victims of our target audience increased. Therefore, we recommend reach out to them through social media ads and campaigns to help raise awareness. 

```r
age_dat %>% 
  filter(sex=="F" & education=="HS/GED") %>% 
  group_by(month) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=month, y=count))+
  geom_point(shape=20, color='black', fill='black', size=3)+
  # set y axis tick mark range
  scale_y_continuous(breaks = seq(445, 490, by = 10))+
  # group=1 helps the line to show up 
  geom_line(group=1, color='blue')+
  # add annotation
  annotate("text", x = 9, y = 482, label = "Large number of female victims\n are from May to July")+
  # add annotation
  annotate("text", x = 11.5, y = 445, label = "445" )+
  # add annotation
  annotate("rect", xmin = 3.7, xmax = 7.3, ymin = 470, ymax = 490,
  alpha = .2)+
  # change x tick mark label
  scale_x_discrete(labels=c("01"="Jan", "02"="Feb", "03"="Mar", "04"="Apr", "05"="May", "06"="Jun", "07"="Jul", "08"="Aug", "09"="Sep", "10"="Oct", "11"="Nov", "12"="Dec"))+
  # change grid background
  theme_bw()+
  labs(
    title = "Number of Female Gun Shot Victims with High School Diploma Each Month", 
    subtitle = "Highest Deaths: May \nLowest Deaths: December ", 
    caption = "data provided by FiveThirtyEight"
  )+
  # remove x, y axis label
  theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
```

![](Case-Study-2_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Seasonal Trend 2: Target Audience 2 (15-29 Years old Male)
***
From the findings, the numbers of gun deaths for our target audience each month do not have an obvious high or low, meaning there isn't any month that has the highest deaths. However, homicide is the second dominant cause of gun shot deaths after homicide, we would suggest that the campaign focus more on suicide prevention. 

```r
age_dat %>% 
  filter(sex=="M" & age_range=="15-29") %>% 
  group_by(month) %>% 
  mutate(month_count=n())%>% 
  ggplot(aes(x=month))+
  geom_bar(aes(fill=intent), position="dodge", width=1)+
  scale_fill_hue(c = 40)+
  # group=1 helps the line to show up 
  geom_line(mapping=aes(y=month_count), group=1, color='black')+
  geom_point(mapping=aes(y=month_count))+
  scale_x_discrete(labels=c("01"="Jan", "02"="Feb", "03"="Mar", "04"="Apr", "05"="May", "06"="Jun", "07"="Jul", "08"="Aug", "09"="Sep", "10"="Oct", "11"="Nov", "12"="Dec"))+
  labs(
    title = "Cuase of Gun Shot Death for 15-29 Years Old Male", 
    subtitle = "Suicide is the Second Largest Group", 
    fill = "", 
    caption = "data provided by FiveThirtyEight"
  )+
  theme_bw()+
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank())+
  theme(legend.position = "top")
```

![](Case-Study-2_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



