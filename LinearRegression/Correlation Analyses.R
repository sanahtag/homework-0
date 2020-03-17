#################################################################################
# Correlation analysis                                                          #
# http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r #
#################################################################################

#################################################################################
# A. learn to manually load table and convert it to data frame                  #
#################################################################################
#                      mpg, cyl, disp,  hp, drat,    wt,  qsec, vs, am, gear, carb,
#"Mazda RX4",         21.0,   6,  160, 110, 3.90, 2.620, 16.46,  0,  1,    4,    4,
#"Mazda RX4 Wag",     21.0,   6,  160, 110, 3.90, 2.875, 17.02,  0,  1,    4,    4,
#"Datsun 710",        22.8,   4,  108,  93, 3.85, 2.320, 18.61,  1,  1,    4,    1,
#"Hornet 4 Drive",    21.4,   6,  258, 110, 3.08, 3.215, 19.44,  1,  0,    3,    1,
#"Hornet Sportabout", 18.7,   8,  360, 175, 3.15, 3.440, 17.02,  0,  0,    3,    2,
#"Valiant",           18.1,   6,  225, 105, 2.76, 3.460, 20.22,  1,  0,    3,    1)

mydata <- matrix(c(21.0,   6,  160, 110, 3.90, 2.620, 16.46,  0,  1,    4,    4,
            21.0,   6,  160, 110, 3.90, 2.875, 17.02,  0,  1,    4,    4,
            22.8,   4,  108,  93, 3.85, 2.320, 18.61,  1,  1,    4,    1,
            21.4,   6,  258, 110, 3.08, 3.215, 19.44,  1,  0,    3,    1,
            18.7,   8,  360, 175, 3.15, 3.440, 17.02,  0,  0,    3,    2,
            18.1,   6,  225, 105, 2.76, 3.460, 20.22,  1,  0,    3,    1),ncol=11,byrow=TRUE)
colnames(mydata) <- c('mpg','cyl', 'disp','hp','drat','wt','qsec','vs','am','gear','carb')
rownames(mydata) <- c("Mazda RX4","Mazda RX4 Wag","Datsun 710","Hornet 4 Drive","Hornet Sportabout","Valiant")
mydata <- as.table(mydata)
mydata

my_data <- as.data.frame.matrix(mydata) 
my_data


#################################################################################
# B. We’ll use the built-in R data set mtcars as an example                     #
#################################################################################

my_data <- mtcars
head(my_data, 6)

cor(my_data$mpg, my_data$wt)

#Visualize your data using scatter plots
library("ggpubr")
ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

#Preleminary test to check the test assumptions
# Shapiro-Wilk normality test for mpg
shapiro.test(my_data$mpg) # => p = 0.1229
# Shapiro-Wilk normality test for wt
shapiro.test(my_data$wt) # => p = 0.09265

#Visual inspection of the data normality using Q-Q plots
# mpg
ggqqplot(my_data$mpg, ylab = "MPG")
# wt
ggqqplot(my_data$wt, ylab = "WT")

#Pearson correlation test
res <- cor.test(my_data$wt, my_data$mpg, 
                  method = "pearson")
res

#Access to the values returned by cor.test() function
# Extract the p.value
res$p.value  #[1] 1.293959e-10
# Extract the correlation coefficient
res$estimate  # cor -0.8676594 

#Kendall rank correlation test
#  The Kendall rank correlation coefficient or Kendall’s tau statistic 
#  is used to estimate a rank-based measure of association. This test 
#  may be used if the data do not necessarily come from a bivariate 
#  normal distribution.
res2 <- cor.test(my_data$wt, my_data$mpg,  method="kendall")
res2
#  tau is the Kendall correlation coefficient.
#  The correlation coefficient between x and y are -0.7278 
#  and the p-value is 6.70610^{-9}.

#Spearman rank correlation coefficient
#  Spearman’s rho statistic is also used to estimate a rank-based 
#  measure of association. This test may be used if the data do not 
#  come from a bivariate normal distribution.
res2 <-cor.test(my_data$wt, my_data$mpg,  method = "spearman")
res2
#  rho is the Spearman’s correlation coefficient.
#  The correlation coefficient between x and y are -0.8864 and the 
#  p-value is 1.48810^{-11}.


