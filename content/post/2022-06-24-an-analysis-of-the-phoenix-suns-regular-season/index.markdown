---
title: An Analysis of the Phoenix Suns Regular Season
author: ''
date: '2022-06-24'
slug: an-analysis-of-the-phoenix-suns-regular-season
categories: []
tags: []
subtitle: 'Taking a closer look at the trends observed in the Suns regular season'
summary: 'In this post, visualizations and statistical methods like linear models were used to analyze trends within the 2021-22 Phoenix Suns Regular Season. '
authors: []
lastmod: '2022-06-24T22:20:10Z'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

## Some Context
The Phoenix Suns had a remarkable regular season recently, and although it ended in disappointment in the playoffs, that should not take away from the success that the team from the valley had during the regular season. The Suns won a franchise record 64 games this year and were owners of the NBA's best record and the first seed in the Western Conference. Let's take a look at some of the trends that can be observed from this season by the Suns. 

## Load in the appropriate libraries

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
## ✔ tibble  3.1.6     ✔ dplyr   1.0.9
## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
## ✔ readr   2.1.2     ✔ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(htmltools)
library(rvest)
```

```
## 
## Attaching package: 'rvest'
```

```
## The following object is masked from 'package:readr':
## 
##     guess_encoding
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

## Bring in the data 

```r
## Create a URL for the Phoenix Suns 2021-22 season
suns_url <- read_html("https://www.basketball-reference.com/teams/PHO/2022/gamelog/")

## Convert the information in the URL into a list
suns_info_lst = suns_url %>%
  html_nodes("#tgl_basic") %>%
  html_table()

## Create a temporary data frame to hold the data from the info list
temp_suns_df = suns_info_lst[[1]]

## Clean the data
## Start by removing team and opponent from the column names
colnames(temp_suns_df) = temp_suns_df[1,]

## Make the team and oppenent stats discernable in the column names
colnames(temp_suns_df)[c(4,6,7,8,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)] = c("game_location","win_or_loss","Tm_Score","Opp_Score","Opp_FG","Opp_FGA","Opp_FG%","Opp_3P","Opp_3PA","Opp_3P%","Opp_FT","Opp_FTA","Opp_FT%","Opp_ORB","Opp_TRB","Opp_AST","Opp_STL","Opp_BLK","Opp_TOV","Opp_PF")

## Remove the first row with redundant column names 
mod1_temp_suns_df = temp_suns_df %>%
  filter(Rk != "Rk") %>%
  filter(!row_number() %in% c(21,42,63,84)) ## Removes rows that contain the values "team" and "opponent"

## Remove the column with NA values and the Rk column
mod2_temp_suns_df = mod1_temp_suns_df %>%
  select(-"NA", -"Rk")

## Change the values that game_location provides
mod3_temp_suns_df = mod2_temp_suns_df %>%
  mutate(game_location = case_when(
    game_location == "@" ~ "Away",
    TRUE ~ "Home"
  ))

## Change the variables in the data frame to be numeric
mod4_temp_suns_df = mod3_temp_suns_df %>%
  mutate_if(~any(str_detect(string = ., pattern = '^-?[0-9]\\d*(\\.\\d+)?$')), 
            ~ as.numeric(.))

## Change the class of the date column to be 'Date' instead of 'character'
phoenix_suns_df = mod4_temp_suns_df %>%
  mutate(proper_date = as_date(Date))
```

## Looking for trends in the data
Now that the data has been completely cleaned, lets take a look at some interesting trends that were present in this past season for the Suns and how their performance varied as the season went on.

## Statistical Question 1: Is there a trend between the number of offensive rebounds and the number of free throw attempts by the Suns?

```r
## Visualize the data from the dataset
phoenix_suns_df %>%
  ggplot(aes(x = ORB, y = FTA)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(x = 'Offensive Rebounds', 
       y = 'Free Throws Attempted', 
       title = 'Offensive Rebounds in Games vs the Free Throws Attempted') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) 
```

```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />

## Summary
From taking a brief look at this graph, it is evident that as the number of offensive rebounds increased, the number of free throws attempted decreased, as shown by the downward line of best fit for the graph. 


```r
## Find the correlation between the two variables
phoenix_suns_df %>%
  select(ORB, FTA) %>%
  cor()
```

```
##            ORB        FTA
## ORB  1.0000000 -0.1769485
## FTA -0.1769485  1.0000000
```
## Summary
As seen in here, there is a slight negative correlation between the variables of ORB, offensive rebounds, and FTA, free throw attempts. This could be seen in the graph shown above, but this value gives us a concrete number to base the correlation of the data on. 


```r
## Create a linear model with the two variables and summarize that model
model_1 = lm(ORB ~ FTA, data = phoenix_suns_df)
summary(model_1)
```

```
## 
## Call:
## lm(formula = ORB ~ FTA, data = phoenix_suns_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.6627 -2.8142  0.1405  2.2378 10.2423 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 11.75325    1.31258   8.954 1.08e-13 ***
## FTA         -0.09955    0.06191  -1.608    0.112    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.041 on 80 degrees of freedom
## Multiple R-squared:  0.03131,	Adjusted R-squared:  0.0192 
## F-statistic: 2.586 on 1 and 80 DF,  p-value: 0.1118
```
## Summary
From this model, a number of different things can be ascertained, most notably. the variation within the data. The variation in the data can be represented in the multiple R-squared value, which is 3.131% here. A low multiple R-squared value generally indicates that there is more variance than is to be expected, which is what we see here. 

## Analysis
From all three of these tools, we can see that when the Suns got more offensive rebounds, they attempted less free throws. This may sound counter intuitive, because if a team gets the ball back after a missed shot near the rim, which is where many offensive rebounds are gathered, there is a chance that they will get fouled when going back up for another shot. However, as shown in both the graph and the correlation value, the relationship between the variables of offensive rebounds and free throws attempted is clearly negative. This assumption should be taken with a grain of salt though, as there is a lot of variance in the data, as shown by the multiple R-squared value. If more data was collected, there would have been a clearer trend, rather than what was shown from the 82 games of the NBA regular season. 

## Statistical Question 2: How did the Suns performance vary as the season went on?

```r
## Create a scatterplot that displays the points the Suns scored in each game as the season went on
phoenix_suns_df %>%
  ggplot(aes(x = proper_date, y = Tm_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_date(date_breaks = '4 week', date_labels = '%Y-%b-%d') +
  theme_bw() +
  labs(x = 'Date', 
       y = 'Team Score', 
       title = 'Phoenix Suns Team Score in Games Over the Season') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

## Summary
As shown in this plot, as the months passed by, the Suns managed to continue their excellent performance and increase the number of points they scored in each game, as shown the by line of best fit. However, there is a lot of variance in this data, and a larger sample size would show a better overall trend. Let's take a look at the specifics of the Suns performance to see why they managed to improve as the season progressed. 


```r
## Create a scatterplot to show how the number of assists by the Suns changed over the season
phoenix_suns_df %>%
  ggplot(aes(x = proper_date, y = AST)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_date(date_breaks = '4 week', date_labels = '%Y-%b-%d') +
  theme_bw() +
  labs(x = 'Date', 
       y = 'Assists', 
       title = 'The Number of Assists by the Suns in Games Over the Season') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

## Summary
This seems to explain the reason for the Suns improvement in scoring over the season. The team managed to improve their ball movement as the season progressed, which led to an increase in the number of assists, and in turn, the number of field goals. A possible explanation for why the assist number increased as the season went on could be explained by the injury to Chris Paul, which left the team without their best playmaker. As a result, more ball movement was required to replicate the effect Paul had for the Suns offense. This remarkable ball movement is why the Suns managed to improve scoring the ball as the season went on, and it looks even more impressive when taking a look at the next visualization...


```r
## Create a scatterplot that displays the team score Suns opponents had in each game as the season went on
phoenix_suns_df %>%
  ggplot(aes(x = proper_date, y = Opp_Score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_date(date_breaks = '4 week', date_labels = '%Y-%b-%d') +
  theme_bw() +
  labs(x = 'Date', 
       y = 'Opposing Team Score', 
       title = 'Suns Opposing Team Score in Games Over the Season') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

## Summary
The excellent ball movement and offense was what kept the Suns in the first seed throughout the regular season, as the team couldn't rely on their defense to clamp down and prevent opposing teams from scoring. As the season went on, the Phoenix Suns allowed more points on defense, as shown by the upward trend in the line of best fit. Let's take a quick look into a possible reason why this was the case. 


```r
## Create a scatterplot that displays the turnovers the Suns forced in each game as the season went on
phoenix_suns_df %>%
  ggplot(aes(x = proper_date, y = Opp_TOV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_date(date_breaks = '4 week', date_labels = '%Y-%b-%d') +
  theme_bw() +
  labs(x = 'Date', 
       y = 'Turnovers by Opposing Team', 
       title = 'Phoenix Suns Turnovers Forced in Games Over the Season') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

## Summary
Here is one possible reason as to why the Suns defense started to break down as the season went on. There is a clear downward trend in the number of turnovers the team was able to force on defense as the season went on, shown by the line of best fit for the plot. By failing to force turnovers, opposing teams had more chances to score the ball, leading to an increase in opponent team scores as the season went on.

## Analysis
From the information shown in these graphs, it is clear that the reason why the Suns were able to persevere throughout the season and continue their stretch of dominance en route to 64 wins was their excellent offense, which improved throughout the season. The Suns managed to preserve their pristine record while improving their offensive output, even in the absence of their star point guard, Chris Paul, by moving the ball around and getting more assists than they had been getting at the beginning of the season. However, their worsening defense, which allowed more points as the season went on, provides insight as to why this excellent Suns team lost in the 2nd round of the NBA Playoffs to the Dallas Mavericks. 

## Hope you enjoyed the read!
Thank you for visiting my website and checking out this post. Stay tuned for more posts in the future!
