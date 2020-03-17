#set.seed(1989) if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later

library(tidyverse)
library(dslabs)
install.packages("HistData")
library(HistData)
data("GaltonFamilies")

options(digits = 3)    # report 3 significant digits
 
female_heights <- GaltonFamilies %>%     
   filter(gender == "female") %>%     
   group_by(family) %>%     
   sample_n(1) %>%     
   ungroup() %>%     
   select(mother, childHeight) %>%     
   rename(daughter = childHeight)

m <- lm (formula = mother ~ daughter, data = female_heights)
summary(m)

Call:
  lm(formula = mother ~ daughter, data = female_heights)

Residuals:
  Min     1Q Median     3Q    Max 
-6.659 -1.211 -0.211  1.496  7.176 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  44.1785     4.4105   10.02  < 2e-16 ***
  daughter      0.3103     0.0686    4.53  1.1e-05 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.17 on 174 degrees of freedom
Multiple R-squared:  0.105,	Adjusted R-squared:   0.1 
F-statistic: 20.5 on 1 and 174 DF,  p-value: 1.11e-05


> female_heights[1]
# A tibble: 176 x 1
mother
<dbl>
1   67  
2   66.5
3   64  
4   64  
5   58.5
6   68  
7   68  
8   66.5
9   66  
10   65.5
# ... with 166 more rows
> female_heights[2]
# A tibble: 176 x 1
daughter
<dbl>
1     69  
2     65.5
3     68  
4     64.5
5     66.5
6     69.5
7     70.5
8     70.5
9     66  
10     65.5
# ... with 166 more rows

library(Lahman)
bat_03 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, yearID, singles, bb)


library(plyr)
bat_03_mean <-= ddply(bat_03, .(playerID), summarize, mean_singles=mean(singles), mean_bb=mean(bb))

nrow(filter(bat_03_mean, mean_singles > 0.2))
