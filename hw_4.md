Homework 4
================
Erik Carlson
10/14/2020

## Ordinary Least Squares

This homework was done in a study group with Emmanuel Mendez, Emily
Vasquez, and Joe Correa.

First we limited our sample by including only those within the age range
of 25-55, those that were in the labor force, and worked a sufficnent
number of weeks and hours. This is because those outside this range will
be outliers in regards to relevant variables such as Income.

``` r
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) # 
detach(acs2017_ny)
attach(dat_use)
```

For our analysis we wished to first find the means of wage in regard to
certain educations across gender.  
So we found the mean wage for females with no high school education and
those with advanced degrees. Then we found the same mean wages for males
with the same educations.

``` r
print(c("The mean wage for females with only a high school education is", round(mean(INCWAGE[female==1|educ_hs==1]),2)))
```

    ## [1] "The mean wage for females with only a high school education is"
    ## [2] "59171.19"

``` r
print(c("The mean wage for females with an advanced degree is", round(mean(INCWAGE[female==1|educ_advdeg==1]),2)))
```

    ## [1] "The mean wage for females with an advanced degree is"
    ## [2] "75758.27"

``` r
print(c("The mean wage for males with only a high school education is", round(mean(INCWAGE[female==0|educ_hs==1]),2)))
```

    ## [1] "The mean wage for males with only a high school education is"
    ## [2] "74657.68"

``` r
print(c("The mean wage for males with an advanced degree is", round(mean(INCWAGE[female==0|educ_advdeg==1]),2)))
```

    ## [1] "The mean wage for males with an advanced degree is"
    ## [2] "82560.25"

From this we found that there was a noticable difference between the
mean wages between the mean wages when comparng females and males. As
females with advanced degree’s mean wage was only slightly greater than
a male with no high school education. Therefore we conducted a more
robust OLS regression analysis.

When investigating the variable of gender it is important to control for
any omitted variable bias, thus regressors for education and age were
added. In order to not have perfect collinearity and avoid the dummy
variable trap the variable no\_hs was ommitted. This is similar to how
we do not have a separate binary variable for male when we already have
one for female.

``` r
model_temp1 <- lm(INCWAGE ~ AGE + female + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)
```

    ## 
    ## Call:
    ## lm(formula = INCWAGE ~ AGE + female + educ_hs + educ_somecoll + 
    ##     educ_college + educ_advdeg)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -146859  -33148  -10757   12908  627834 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -13788.22    2344.93  -5.880 4.13e-09 ***
    ## AGE             1341.05      39.62  33.850  < 2e-16 ***
    ## female        -25664.63     719.00 -35.695  < 2e-16 ***
    ## educ_hs        12856.93    1786.14   7.198 6.19e-13 ***
    ## educ_somecoll  25300.69    1819.51  13.905  < 2e-16 ***
    ## educ_college   60817.28    1783.93  34.092  < 2e-16 ***
    ## educ_advdeg    86890.04    1825.83  47.589  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 76890 on 46964 degrees of freedom
    ## Multiple R-squared:  0.1469, Adjusted R-squared:  0.1468 
    ## F-statistic:  1347 on 6 and 46964 DF,  p-value: < 2.2e-16

``` r
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(1, 0.2, 0.6, alpha = 0.2), main = "Wage vs Age for a female with an advanced degree", xlab = "Age", ylab = "Wage", ylim = c(60000,150000), data = dat_graph)
# d

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1,  educ_hs = 0, educ_somecoll = 0, educ_college = 0, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
```

![](hw_4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

We wished to test whether there is a gender disparity in regards to
wage.

The hypothesis is that when we look at the wage variable, that H0 B2 ==
0  
The null hypothesis is that the regressor for the variable female is
equal to 0. Therefore that that there is no statistical difference
between genders in regards to wage.

Ha B2 \!= 0 Our alternative hypothesis is that the regressor for the
variable female is not equal to zero. Therefore we would reject the null
hypothesis and we would assume that there is a difference in wage due to
gender.

To find the t value the regressor is divided by the standard error:
Where -25664.63/719.00 = -35.695

``` r
length(female)
```

    ## [1] 46971

``` r
t<-- -25664.63/719.00
2*pt ( -abs(t) ,60060-1)
```

    ## [1] 3.7548e-276

Our p value is less than 2e-16 thus the result is significant at p\<0.05
and therefore we can say with 95% percent confidence that we reject our
null hypothesis.

It cannot be assumed that the distribution is homoskedastic, that the
disturbances are same across the independent variables. Thus a robust
regression with heterskedasticity was done using coeftest.

``` r
  coeftest(model_temp1,vcovHC)
```

    ## 
    ## t test of coefficients:
    ## 
    ##                 Estimate Std. Error  t value  Pr(>|t|)    
    ## (Intercept)   -13788.217   1749.606  -7.8808 3.324e-15 ***
    ## AGE             1341.047     37.244  36.0070 < 2.2e-16 ***
    ## female        -25664.628    729.323 -35.1896 < 2.2e-16 ***
    ## educ_hs        12856.930   1002.635  12.8231 < 2.2e-16 ***
    ## educ_somecoll  25300.686   1047.372  24.1563 < 2.2e-16 ***
    ## educ_college   60817.275   1242.265  48.9568 < 2.2e-16 ***
    ## educ_advdeg    86890.040   1557.879  55.7746 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Using a robust regression there are no significant differences, however
it is more appropriate since homoskedasticity is very rare and cannot be
assumed.

Following the regression with INCWAGE we wished to instead use the log
of wage to show the effects in regards to percent changes rather than
absolute numbers. We also wished to show the difference in the regressor
for age after gender and education is accounted for.

``` r
model_temp2 <- lm(log1p(INCWAGE) ~ AGE)
coeftest(model_temp2,vcovHC)
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept) 10.4108371  0.0442553 235.2452  < 2e-16 ***
    ## AGE          0.0023010  0.0011115   2.0702  0.03844 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Using the log of wage shows that every increase of 1 in age, wage
increases by 0.23%. Furthermore with p\<0.05 this increase in wage is
statistically significant with 95% confidence.

``` r
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)

plot(log1p(INCWAGE) ~ jitter(AGE, factor = 2), pch = 16, col = rgb(1, 0.2, 0.6, alpha = 0.2), main = "Wage vs Age", xlab = "Age", ylab = "Log Wage", ylim = c(0,14), data = dat_graph)
# d

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55)
to_be_predicted2$yhat <- predict(model_temp2, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
```

![](hw_4_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Following this bivariate regression we wished to include other control
variables to reduce the influence of omitted variable bias.

``` r
model_temp3 <- lm(log1p(INCWAGE) ~ AGE + female + educ_hs + educ_somecoll + educ_college + educ_advdeg)
coeftest(model_temp3,vcovHC)
```

    ## 
    ## t test of coefficients:
    ## 
    ##                 Estimate Std. Error  t value  Pr(>|t|)    
    ## (Intercept)    9.2408447  0.0753115 122.7017 < 2.2e-16 ***
    ## AGE            0.0079356  0.0011143   7.1214 1.084e-12 ***
    ## female        -0.1246766  0.0198966  -6.2662 3.731e-10 ***
    ## educ_hs        0.5431287  0.0619130   8.7724 < 2.2e-16 ***
    ## educ_somecoll  0.8361732  0.0620692  13.4716 < 2.2e-16 ***
    ## educ_college   1.2853214  0.0612637  20.9801 < 2.2e-16 ***
    ## educ_advdeg    1.5671591  0.0617884  25.3633 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Including for these control variables shows that age has both a larger
and more statistically significant effect on wage than when just the
bivarate analysis was conducted.  
Each increase in age instead increases wage by 0.79% and with a t value
of 7.12 and a p value of 1\*10^-12 it is statistically significant with
at least 95% confidence.

``` r
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)

plot(log1p(INCWAGE) ~ jitter(AGE, factor = 2), pch = 16, col = rgb(1, 0.2, 0.6, alpha = 0.2), main = "Wage vs Age for a female with an advanced degree", xlab = "Age", ylab = "Log Wage", ylim = c(0,14), data = dat_graph)
# d

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1,  educ_hs = 0, educ_somecoll = 0, educ_college = 0, educ_advdeg = 1)
to_be_predicted2$yhat <- predict(model_temp3, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
```

![](hw_4_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Folowing this we conducted a regression including for whether the
individual is hispanic dominican.

``` r
model_temp4 <- lm(INCWAGE ~ AGE + female + educ_hs + educ_somecoll + educ_college + educ_advdeg + Hisp_DomR)
summary(model_temp4)
```

    ## 
    ## Call:
    ## lm(formula = INCWAGE ~ AGE + female + educ_hs + educ_somecoll + 
    ##     educ_college + educ_advdeg + Hisp_DomR)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -146820  -33224  -10727   12906  626829 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -12653.75    2357.73  -5.367 8.05e-08 ***
    ## AGE             1334.80      39.63  33.679  < 2e-16 ***
    ## female        -25562.87     719.20 -35.544  < 2e-16 ***
    ## educ_hs        12291.80    1790.10   6.867 6.66e-12 ***
    ## educ_somecoll  24694.58    1824.03  13.538  < 2e-16 ***
    ## educ_college   60070.46    1791.14  33.538  < 2e-16 ***
    ## educ_advdeg    86059.48    1834.61  46.909  < 2e-16 ***
    ## Hisp_DomR      -9829.31    2166.59  -4.537 5.73e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 76880 on 46963 degrees of freedom
    ## Multiple R-squared:  0.1472, Adjusted R-squared:  0.1471 
    ## F-statistic:  1158 on 7 and 46963 DF,  p-value: < 2.2e-16

There is a statistically significant decrease in net income for Hispanic
dominicans. We wanted to investigate compounding negative factors by
plotting them.

Following the regression we wanted to predict the wage of a hispanic
dominican female with a high school education from ages 25 to 55 and
compare the result to a prediction that does not include ethnicity as a
factor.

The first plot is for females without regard for race.

``` r
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(1, 0.2, 0.6, alpha = 0.2), main = "Wage vs Age, Hispanic dominican female with a highschool education", xlab = "Age", ylab = "Wage", ylim = c(0,50000), data = dat_graph)
# d

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1,  educ_hs = 1, educ_somecoll = 0, educ_college = 0, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
```

![](hw_4_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

The second plot is for females including hispanic dominican ethnicity.

``` r
# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(1, 0.2, 0.6, alpha = 0.2), main = "Wage vs Age, Hispanic dominican female with a highschool education", xlab = "Age", ylab = "Wage", ylim = c(0,50000), data = dat_graph)
# d

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1,  educ_hs = 1, educ_somecoll = 0, educ_college = 0, educ_advdeg = 0, Hisp_DomR = 1)
to_be_predicted2$yhat <- predict(model_temp4, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)
```

![](hw_4_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

The result was that there was a significant differences in wage between
Hispanic dominican females with a high school education and for the
general female populace with a high school education. However these
differences led to odd results on the lower end of the trend line, where
it is approximated that a 25 year old woman who is employed, works
multiple weeks for over 35 hours makes no income. However this obviously
not the case, and is more a result of the overall trend not fitting the
lower section of the dataset.

For possible future analysis, another interesting possibility would be
analyzing using the information of individuals’ places of birth. Rather
than just ethnicity, it may produce interesting results looking into
data for people born outside the United States. It may also be
interesting comparing the income of an indiviual compared to their
parent’s income, looking into the opportunities of generational wealth
or poverty.
