#1# CPI data
year<- rep(2008:2010, each = 4)
quarter <- rep(1:4, 3)
cpi <- c(162.2, 164.6, 166.5, 166.0,
        166.2, 167.0, 168.6, 169.5,
        171.0, 172.1, 173.3, 174.0)
plot(cpi, xaxt="n", ylab="CPI", xlab="")
# draw x-axis, where las=3 makes text vertical
axis(1, labels=paste(year,quarter,sep="Q"), at=1:12, las=3)

#2# correlation between CPI and year / quarter
cor(year, cpi)
## [1] 0.9096316

cor(quarter, cpi)
## [1] 0.3738028

## build a linear regression model with function lm()
fit <- lm(cpi ~ year + quarter)
fit
##
## Call:
## lm(formula = cpi ~ year + quarter)
##
## Coefficients:
## (Intercept) year quarter
## -7644.488 3.888 1.167

#3# make prediction: What will the CPI be in 2011?
cpi2011 <- fit$coefficients[[1]] +
  fit$coefficients[[2]] * 2011 +
  fit$coefficients[[3]] * (1:4)
cpi2011
## [1] 174.4417 175.6083 176.7750 177.9417

#4# attributes of the model
attributes(fit)
## $names
## [1] "coefficients" "residuals" "effects"
## [4] "rank" "fitted.values" "assign"
## [7] "qr" "df.residual" "xlevels"
## [10] "call" "terms" "model"
##
## $class
## [1] "lm"

fit$coefficients
## (Intercept) year quarter
## -7644.487500 3.887500 1.166667

#5# differences between observed values and fitted values
residuals(fit)
##           1          2          3           4           5
## -0.57916667 0.65416667 1.38750000 -0.27916667 -0.46666667
##           6           7           8          9         10
## -0.83333333 -0.40000000 -0.66666667 0.44583333 0.37916667
##         11          12
## 0.41250000 -0.05416667

summary(fit)
##
## Call:
## lm(formula = cpi ~ year + quarter)
##
## Residuals:
## Min 1Q Median 3Q Max
## -0.8333 -0.4948 -0.1667 0.4208 1.3875
##
## Coefficients:
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) -7644.4875 518.6543 -14.739 1.31e-07 ***
## year 3.8875 0.2582 15.058 1.09e-07 ***
## quarter 1.1667 0.1885 6.188 0.000161 ***

#6# 3D Plot of the Fitted Model
library(scatterplot3d)
s3d <- scatterplot3d(year, quarter, cpi, highlight.3d=T, type="h",
                     lab=c(2,3)) # lab: number of tickmarks on x-/y-axes
s3d$plane3d(fit) # draws the fitted plane

#7# Prediction of CPIs in 2011
data2011 <- data.frame(year=2011, quarter=1:4)
cpi2011 <- predict(fit, newdata=data2011)
style <- c(rep(1,12), rep(2,4))
plot(c(cpi, cpi2011), xaxt="n", ylab="CPI", xlab="",
     pch=style, col=style)
txt <- c(paste(year,quarter,sep="Q"),
         "2011Q1", "2011Q2", "2011Q3", "2011Q4")
axis(1, at=1:16, las=3, labels=txt)
