---
title: Data Explorations Within NBA 2019
author: ''
date: '2022-04-30'
slug: data-explorations-within-nba-2019
categories: [NBA, tidyverse]
tags: []
subtitle: 'Analyzing NBA data from the 2019 season'
summary: 'In this post, statistical methods like linear models were used to analyze patterns within the 2019 NBA season'
authors: []
lastmod: '2022-04-30T16:18:32Z'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---
## Context Behind Data:
  This data set contains the outcomes of every game in the 2019 NBA regular season. The information contained in this data set would be particularly useful to the front offices of each NBA team, as it contains valuable information and trends on some major statistics like points scored by a team, field goal percentage by a team, etc. The trends observed from this could prove useful to teams by providing them which areas they need to improve upon.
  The population of interest in this data set is all the games played in the 2019 season. The observational unit in this data set is a game. The majority of the variables in this data set are quantitative, with a few exceptions. Game location, opponent, game outcome, date, and team name, are all categorical variables. Specifically, game location and game outcome are categorical binary variables. 

## Load in the libraries needed

```r
library(tidyverse)
library(corrplot)
```


## Load in the data

```r
nba_df = read.csv('cleaned_nba_2019_outcomes_df.csv')
```

## Clean the data frame

```r
## Changing the names of the teams from links to their actual names
mod_1_nba_df = nba_df %>%
  mutate(team_name = case_when(
    team_name == 'https://www.basketball-reference.com/teams/ATL/2019/gamelog/' ~ 'Atlanta Hawks', 
    team_name == 'https://www.basketball-reference.com/teams/BOS/2019/gamelog/' ~ 'Boston Celtics',
    team_name == 'https://www.basketball-reference.com/teams/CHI/2019/gamelog/' ~ 'Chicago Bulls',
    team_name == 'https://www.basketball-reference.com/teams/CLE/2019/gamelog/' ~ 'Cleveland Cavaliers',
    team_name == 'https://www.basketball-reference.com/teams/DAL/2019/gamelog/' ~ 'Dallas Mavericks',
    team_name == 'https://www.basketball-reference.com/teams/DEN/2019/gamelog/' ~ 'Denver Nuggets',
    team_name == 'https://www.basketball-reference.com/teams/DET/2019/gamelog/' ~ 'Detroit Pistons',
    team_name == 'https://www.basketball-reference.com/teams/GSW/2019/gamelog/' ~ 'Golden State Warriors',
    team_name == 'https://www.basketball-reference.com/teams/HOU/2019/gamelog/' ~ 'Houston Rockets',
    team_name == 'https://www.basketball-reference.com/teams/IND/2019/gamelog/' ~ 'Indiana Pacers',
  team_name == 'https://www.basketball-reference.com/teams/LAC/2019/gamelog/' ~ 'Los Angeles Clippers',  
  team_name == 'https://www.basketball-reference.com/teams/LAL/2019/gamelog/' ~ 'Los Angeles Lakers',
  team_name == 'https://www.basketball-reference.com/teams/MEM/2019/gamelog/' ~ 'Memphis Grizzlies',
  team_name == 'https://www.basketball-reference.com/teams/MIA/2019/gamelog/' ~ 'Miami Heat',
  team_name == 'https://www.basketball-reference.com/teams/MIL/2019/gamelog/' ~ 'Milwaukee Bucks',
  team_name == 'https://www.basketball-reference.com/teams/MIN/2019/gamelog/' ~ 'Minnesota Timberwolves',
  team_name == 'https://www.basketball-reference.com/teams/NOP/2019/gamelog/' ~ 'New Orleans Pelicans',
  team_name == 'https://www.basketball-reference.com/teams/NYK/2019/gamelog/' ~ 'New York Knicks',
  team_name == 'https://www.basketball-reference.com/teams/OKC/2019/gamelog/' ~ 'Oklahoma City Thunder',
  team_name == 'https://www.basketball-reference.com/teams/ORL/2019/gamelog/' ~ 'Orlando Magic',
  team_name == 'https://www.basketball-reference.com/teams/PHI/2019/gamelog/' ~ 'Philadelphia 76ers',
  team_name == 'https://www.basketball-reference.com/teams/POR/2019/gamelog/' ~ 'Portland Trailblazers',
  team_name == 'https://www.basketball-reference.com/teams/SAC/2019/gamelog/' ~ 'Sacramento Kings',
  team_name == 'https://www.basketball-reference.com/teams/SAS/2019/gamelog/' ~ 'San Antonio Spurs',
  team_name == 'https://www.basketball-reference.com/teams/TOR/2019/gamelog/' ~ 'Toronto Raptors',
  team_name == 'https://www.basketball-reference.com/teams/UTA/2019/gamelog/' ~ 'Utah Jazz',
  team_name == 'https://www.basketball-reference.com/teams/WAS/2019/gamelog/' ~ 'Washington Wizards'
  ))
```


```r
## Filtering out the home games in the data set to avoid duplication of data
mod_2_nba_df = mod_1_nba_df %>%
  filter(Game_Location == 'Away')
```


```r
## Changing the date variable from a character to a date
mod_3_nba_df = mod_2_nba_df %>%
  mutate(mod_1_date = dates<- as.Date(Date, "%Y-%m-%d"))
```



## Question 1: Is the mean free throw percentage the same across wins and losses?


### Visualization 

```r
mean_w = mod_3_nba_df %>% 
  filter(Game_Outcome == 'W') %>% 
  pull(FT_percent_team) %>% 
  mean(na.rm=TRUE)

mean_l= mod_3_nba_df %>% 
  filter(Game_Outcome == 'L') %>% 
  pull(FT_percent_team) %>% 
  mean(na.rm=TRUE)

mod_3_nba_df %>%
  ggplot(aes(x = FT_percent_team, group = Game_Outcome, fill = Game_Outcome)) +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = mean_l, color = 'red') +
  geom_vline(xintercept = mean_w, color = 'blue') +
  # facet_grid(.~Game_Outcome) +
  theme_bw() +
  labs(title = 'Distribution of Data in Free Throw Percentage in Wins and Losses',
       x = 'Free Throw Percentage') +
  theme(plot.title = element_text(hjust = 0.5)) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />
Description: This graph is a density plot of the distribution of free throw percentage in wins and loss of away games in the NBA. There are also two vertical lines, which signify the mean free throw percentage in a win, the blue line, and a loss, the red line. 


```r
mod_3_nba_df %>%
  ggplot(aes(x = Game_Outcome, y = FT_percent_team)) +
  geom_violin(fill = 'blue') +
  theme_bw() +
  labs(title = 'Distribution of Data in Free Throw Percentage in Wins and Losses', x = 'Game Outcome', y = 'Free Throw Percentage') +
  theme(plot.title = element_text(hjust = 0.5)) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />
Description: This graph is a violin plot that shows the distribution of free throw percentage in wins and losses in away games for NBA teams. 

### Summary

```r
mod_3_nba_df %>% 
  group_by(Game_Outcome) %>% 
  summarise(mean_FT_P = mean(FT_percent_team,na.rm=TRUE), sd_FT_P = sd(FT_percent_team,na.rm=TRUE),sample_size = n() )
```

```
## # A tibble: 2 × 4
##   Game_Outcome mean_FT_P sd_FT_P sample_size
##   <chr>            <dbl>   <dbl>       <int>
## 1 L                0.758  0.109          646
## 2 W                0.779  0.0983         461
```




### Analysis

```r
model_1 = aov(FT_percent_team~Game_Outcome, data = mod_3_nba_df)
summary(model_1)
```

```
##                Df Sum Sq Mean Sq F value   Pr(>F)    
## Game_Outcome    1  0.119  0.1193   10.95 0.000968 ***
## Residuals    1105 12.047  0.0109                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



```r
t.test(FT_percent_team~Game_Outcome, data = mod_3_nba_df)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  FT_percent_team by Game_Outcome
## t = -3.3643, df = 1045.1, p-value = 0.0007953
## alternative hypothesis: true difference in means between group L and group W is not equal to 0
## 95 percent confidence interval:
##  -0.033346966 -0.008777645
## sample estimates:
## mean in group L mean in group W 
##       0.7580201       0.7790824
```

By looking at the distribution of the data from the density plot and the violin plot, the data for free throw percentage in wins is more concentrated in the area from 50% to 100% while the free throw percentage in losses is more spread out. We can also look at the p value in the model, which is 0.000968. This is a relatively small number, which means that there is some evidence to reject the hypothesis our null hypothesis of the mean free throw percentage being the same across wins and losses. The reason for this distribution of data might be because in games that teams lose, they may not be taking full advantage of the free points at the free throw line, which leads to them not being able to keep up with teams that score more points. 






## Question 2: Does the number of steals and turnovers predict the team score?

```r
mod_4_nba_df = mod_3_nba_df %>%
  select(STL_team, TOV_team, Team_Score)
```

### Visualization

```r
mod_4_nba_df %>% 
  cor() %>%
  corrplot()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />
Description: This is a correlation plot that shows whether two variables have a strong positive relationship, a weak positive relationship, a strong negative relationship, or a weak negative relationship. The variables that are being observed are steals by team, turnovers by team, and the team score. 



```r
mod_4_nba_df %>%
  ggplot(aes(x = STL_team, y = Team_Score))+#, size = TOV_team, alpha = TOV_team)) +
  geom_point(colour = 'dark blue') +
  theme_bw() +
  labs(title = 'Team Score Versus Steals', x = 'Steals', y = 'Team Score') +
  theme(plot.title = element_text(hjust = 0.5)) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />
Description: This graph is a scatter plot that plots the relationship between steals and the team score for away games in the 2019 NBA season. 


### Summarization

```r
#### To Do 1: Mean & SD of all three variables
mod_3_nba_df %>%
  summarise(mean_team_score = mean(Team_Score, na.rm = TRUE), 
            mean_stl = mean(STL_team, na.rm = TRUE), 
            mean_tov = mean(TOV_team, na.rm = TRUE), 
            sd_team_score = sd(Team_Score, na.rm = TRUE), 
            sd_stl = sd(STL_team, na.rm = TRUE),
            sd_tov = sd(TOV_team, na.rm = TRUE))
```

```
##   mean_team_score mean_stl mean_tov sd_team_score   sd_stl   sd_tov
## 1        109.9612  7.66757 13.54291       12.3376 2.873724 4.056196
```


```r
#### To Do 2: Correlation between Variables
mod_4_nba_df %>% 
  cor()
```

```
##             STL_team   TOV_team Team_Score
## STL_team   1.0000000  0.1123018  0.0526025
## TOV_team   0.1123018  1.0000000 -0.1104577
## Team_Score 0.0526025 -0.1104577  1.0000000
```

### Analysis:

```r
model_2 = lm(Team_Score ~ TOV_team + STL_team, data = mod_4_nba_df)
summary(model_2)
```

```
## 
## Call:
## lm(formula = Team_Score ~ TOV_team + STL_team, data = mod_4_nba_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -40.230  -8.222  -0.099   8.022  58.260 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 112.64853    1.53928  73.182  < 2e-16 ***
## TOV_team     -0.35846    0.09136  -3.923 9.27e-05 ***
## STL_team      0.28266    0.12896   2.192   0.0286 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.25 on 1104 degrees of freedom
## Multiple R-squared:  0.01648,	Adjusted R-squared:  0.0147 
## F-statistic:  9.25 on 2 and 1104 DF,  p-value: 0.0001038
```

 
There appears to be no linear relationship between the amount of steals and turnovers compared to the amount of points scored. Evidence for this can be seen in the p values for the intercept, turnovers for team, and steals for team in the summary of the model. All three p values are very small, which means that there is a lot of evidence available to reject the notion that there is a linear relationship present. In addition to the p value, we can see that there isn't a linear relationship by looking at the plot of the data. There is no clear association between the three variables as they are all scattered around. The reason for this might be because while steals and turnovers can impact a basketball game and whether a team wins or not, they do not have much of an effect on the team score. For example, if a team manages to get a lot of steals, but fails to score the ball off of those steals, the team score won't be affected very much. 


## Question 3: If the number of field goals attempted by a team increases, does the number of offensive rebounds also increase?

```r
mod_3_nba_df %>%
  ggplot(aes(x = FGA_team, y = ORB_team)) +
  geom_point(size = 2, colour = 'dark blue') +
  theme_bw() +
  labs(title = 'Field Goals Attempted Versus Offensive Rebounds', x = 'Field Goals Attempted', y = 'Offensive Rebounds') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = 'lm', formula = y~x, se =F)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />
Description: This graph is a scatter plot that shows the relationship between the field goals attempted by a team in an away game, and the offensive rebounds collected. 

### Summarization

```r
#### To Do 3: Mean & SD of all three variables
mod_3_nba_df %>%
  summarise(mean_FGA = mean(FGA_team, na.rm = TRUE),
            mean_ORB = mean(ORB_team, na.rm = TRUE),
            sd_FGA = sd(FGA_team, na.rm = TRUE),
            sd_ORB = sd(ORB_team, na.rm = TRUE))
```

```
##   mean_FGA mean_ORB   sd_FGA   sd_ORB
## 1 89.17706 10.22222 7.082649 3.777172
```


```r
#### To Do 4: Correlation between Variables
mod_3_nba_df %>%
  select(FGA_team, ORB_team) %>%
  cor()
```

```
##           FGA_team  ORB_team
## FGA_team 1.0000000 0.5461798
## ORB_team 0.5461798 1.0000000
```


### Analysis

```r
model_3 = lm(FGA_team ~ ORB_team, data = mod_3_nba_df)
summary(model_3)
```

```
## 
## Call:
## lm(formula = FGA_team ~ ORB_team, data = mod_3_nba_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.9253  -3.9495   0.0022   4.0264  29.0264 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 78.70794    0.51491  152.86   <2e-16 ***
## ORB_team     1.02415    0.04725   21.67   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.936 on 1105 degrees of freedom
## Multiple R-squared:  0.2983,	Adjusted R-squared:  0.2977 
## F-statistic: 469.8 on 1 and 1105 DF,  p-value: < 2.2e-16
```

There appears to be a positive association between the number of field goals attempted and offensive rebounds. That is, as the number of field goals attempted increases, the number of offensive rebounds increases too. This could be explained by the fact that as the amount of field goals attempted increases, there are more chances for the shot to be off target. This results in more chances to grab an offensive rebound during a game. There is also a lot of variance within this data set, as is shown by the distribution of the data points in the scatterplot and the multiple r squared value, which is 29.83%. The 29.83% is the proportion of variability that can be explained by the model, which shows that there is a lot of variability in the model that is left unexplained. 





```r
mod_3_nba_df %>% 
  select_if(is.numeric) %>% 
  select(contains('team')) %>% 
  cor() %>% 
  corrplot()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-21-1.png" width="672" />

## Question 4: If teams turn the ball over more, does that mean that they attempt fewer shots?

### Visualization

```r
mod_3_nba_df %>%
  ggplot(aes(x = TOV_team, y = FGA_team)) +
  geom_point(size = 2, colour = 'red') +
  theme_bw() +
  labs(title = 'Turnovers Versus Field Goals Attempted', x = 'Turnovers', y = 'Field Goals Attempted') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = 'lm', formula = y~x, se =F)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-22-1.png" width="672" />
Description: This graph is a scatter plot that shows the relationship between the turnovers by a team in an away game, and the field goals attempted. The independent variable is turnovers and the dependent variable is the number of field goals attempted. 
  
### Summarization

```r
#### Mean and SD of both variables
mod_3_nba_df %>%
  summarise(mean_TOV = mean(TOV_team, na.rm = TRUE),
            mean_FGA = mean(FGA_team, na.rm = TRUE),
            sd_TOV = sd(TOV_team, na.rm = TRUE),
            sd_FGA = sd(FGA_team, na.rm = TRUE))
```

```
##   mean_TOV mean_FGA   sd_TOV   sd_FGA
## 1 13.54291 89.17706 4.056196 7.082649
```

```r
#### Correlation betweenn variables
mod_3_nba_df %>%
  select(TOV_team, FGA_team) %>%
  cor()
```

```
##            TOV_team   FGA_team
## TOV_team  1.0000000 -0.3187027
## FGA_team -0.3187027  1.0000000
```

### Analysis

```r
model_4 = lm(TOV_team ~ FGA_team, data = mod_3_nba_df)
summary(model_4)
```

```
## 
## Call:
## lm(formula = TOV_team ~ FGA_team, data = mod_3_nba_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.4801 -2.7577 -0.2102  2.5996 13.3451 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 29.81945    1.46083   20.41   <2e-16 ***
## FGA_team    -0.18252    0.01633  -11.18   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.846 on 1105 degrees of freedom
## Multiple R-squared:  0.1016,	Adjusted R-squared:  0.1008 
## F-statistic: 124.9 on 1 and 1105 DF,  p-value: < 2.2e-16
```
There appears to be a negative association between the number of turnovers and the field goals attempted. This can be seen in the graph of the data, where the line of best fit shows that as turnovers increase, the number of field goals decrease. Additionally, the correlation value for the two variables lies at -0.3187027, which indicates that there is a negative association because the value is less than zero. However, this association is not very strong. This can be attributed to the fact that if a team turns the ball over, they waste a possession and a chance to take a shot at the basket. There is some variance in this data set, which can be seen in the multiple R squared value, which lies at 10.16%. 


## Question 5: If teams have more assists, do they score more points in a game?

### Visualization

```r
mod_3_nba_df %>%
  ggplot(aes(x = AST_team, y = Team_Score)) +
  geom_point(size = 2, colour = 'light green') +
  theme_bw() +
  labs(title = 'Assists Versus Points Scored', x = 'Assists', y = 'Points Scored') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = 'lm', formula = y~x, se =F)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-26-1.png" width="672" />
Description: This graph is a scatter plot that shows the relationship between the number of assists by a team and the points scored. The variable on the x axis is the number of assists by a team and the variable on the y axis is points scored by a team. 
  
### Summarization

```r
#### Mean and SD of both variables
mod_3_nba_df %>%
  summarise(mean_ast = mean(AST_team, na.rm = TRUE),
            mean_team_score = mean(Team_Score, na.rm = TRUE),
            sd_ast = sd(AST_team, na.rm = TRUE),
            sd_team_score = sd(Team_Score, na.rm = TRUE))
```

```
##   mean_ast mean_team_score   sd_ast sd_team_score
## 1 24.13189        109.9612 5.078831       12.3376
```

```r
#### Correlation between variables
mod_3_nba_df %>%
  select(AST_team, Team_Score) %>%
  cor()
```

```
##             AST_team Team_Score
## AST_team   1.0000000  0.5490799
## Team_Score 0.5490799  1.0000000
```

### Analysis

```r
model_5 = lm(AST_team ~ Team_Score, data = mod_3_nba_df)
summary(model_5)
```

```
## 
## Call:
## lm(formula = AST_team ~ Team_Score, data = mod_3_nba_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.0448  -2.6099   0.0511   2.9895  13.2771 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.72278    1.14523  -0.631    0.528    
## Team_Score   0.22603    0.01035  21.839   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.247 on 1105 degrees of freedom
## Multiple R-squared:  0.3015,	Adjusted R-squared:  0.3009 
## F-statistic: 476.9 on 1 and 1105 DF,  p-value: < 2.2e-16
```
There is a positive association between the assists a team has and the points that they score. This is evidenced by the distribution of the data in the graph and the correlation between the two variables. The correlation value is 0.5490799, which is close to 1. This means that there is a strong positive association between the two variables. Additionally, the slope of the graph is positive, which means that a positive association is present. This can be attributed to the fact that in order to get an assist, the player who receives a pass must score the ball. As a result, as teams get more assists, that means that they are able to put the ball in the basket more, which leads to them scoring more points in games. 
