### Lesson 6 - Thursday 10/6/22

* Tonight's topic: interpretation of multi-category independent and multipe indepdendent variables in logistic regression models.
* We begin by reading in the dataset:

```r
df <- read.csv(file="minn.txt",sep=",",header=T)
head(df,n=10)
tail(df,n=10)
```

* Here is the output:

```rout
> df <- read.csv(file="minn.txt",sep=",",header=T)
> head(df,n=10)
   id ta td aggcirc y
1   1  1  1       1 1
2   2  1  1       1 1
3   3  1  1       1 1
4   4  1  1       1 1
5   5  1  1       1 1
6   6  1  1       1 1
7   7  1  1       1 1
8   8  1  1       1 0
9   9  1  1       1 0
10 10  1  1       1 0
> tail(df,n=10)
     id ta td aggcirc y
304 304  3  3       0 0
305 305  3  3       0 0
306 306  3  3       0 0
307 307  3  3       0 0
308 308  3  3       0 0
309 309  3  3       0 0
310 310  3  3       0 0
311 311  3  3       0 0
312 312  3  3       0 0
313 313  3  3       0 0
> 
```

* Next,here is a 2x3 contingency table showing the joint distribution of the outcomes (y) and the randomized treatments (ta).

```r
table(df$y,df$ta)
```

```rout  
> table(df$y,df$ta)
   ta
y    1  2  3
  0 82 87 87
  1 10 21 26
>
```

* A chi-square test can be used to check on the correspondence between observed frequencies in this table and the frequencies that would be expected if the assigned treatment and outcomes are independent of each other. Step 1 is to identify the observed frequencies:

```r
f01 <- 82
f02 <- 87
f03 <- 87
f11 <- 10
f12 <- 21
f13 <- 26
```

* Next, we calculate the frequencies we would expect to see in each cell if the assigned treatment and outcomes are independent:

```r
e01 <- 92*256/313
e01
e02 <- 108*256/313
e02
e03 <- 113*256/313
e03

e11 <- 92*57/313
e11
e12 <- 108*57/313
e12
e13 <- 113*57/313
e13
```

* Then, for each cell of the table, we calculate the squared difference between observed and expected frequencies divided by the the expected cell frequency:

```r
oe01 <- (f01-e01)^2/e01
oe01
oe02 <- (f02-e02)^2/e02
oe02
oe03 <- (f03-e03)^2/e03
oe03
oe11 <- (f11-e11)^2/e11
oe11
oe12 <- (f12-e12)^2/e12
oe12
oe13 <- (f13-e13)^2/e13
oe13
```

* Last, we sum across these quantities to get our test statistic:

```r
oe01+oe02+oe03+oe11+oe12+oe13
```

* Here are the results:

```rout
> f01 <- 82
> f02 <- 87
> f03 <- 87
> f11 <- 10
> f12 <- 21
> f13 <- 26
> 
> e01 <- 92*256/313
> e01
[1] 75.24601
> e02 <- 108*256/313
> e02
[1] 88.33227
> e03 <- 113*256/313
> e03
[1] 92.42173
> 
> e11 <- 92*57/313
> e11
[1] 16.75399
> e12 <- 108*57/313
> e12
[1] 19.66773
> e13 <- 113*57/313
> e13
[1] 20.57827
> 
> oe01 <- (f01-e01)^2/e01
> oe01
[1] 0.6062306
> oe02 <- (f02-e02)^2/e02
> oe02
[1] 0.02009389
> oe03 <- (f03-e03)^2/e03
> oe03
[1] 0.3180541
> oe11 <- (f11-e11)^2/e11
> oe11
[1] 2.72272
> oe12 <- (f12-e12)^2/e12
> oe12
[1] 0.09024625
> oe13 <- (f13-e13)^2/e13
> oe13
[1] 1.428453
> 
> oe01+oe02+oe03+oe11+oe12+oe13
[1] 5.185798
> 
```

* We can confirm these results:

```r
table(df$y,df$ta)
chisq.test(table(df$y,df$ta))
```

which yields:

```rout
> table(df$y,df$ta)
   ta
y    1  2  3
  0 82 87 87
  1 10 21 26
> chisq.test(table(df$y,df$ta))

	Pearson's Chi-squared test

data:  table(y, ta)
X-squared = 5.1858, df = 2, p-value = 0.0748

> 
```

* Next, we check on a logistic regression specification summarizing the information in this table. We start by creating "dummy" or indicator variables for the arrest and advise conditions (the separate condition is the suppressed or reference category):

```r
df$arrest <- rep(NA,313)
df$arrest[df$ta==1] <- 1
df$arrest[df$ta!=1] <- 0
table(df$ta,df$arrest)
df$advise <- rep(NA,313)
df$advise <- rep(0,313)
df$advise[df$ta==2] <- 1
table(df$ta,df$advise)
cat <- paste(df$ta,df$arrest,df$advise)
table(cat)
table(df$arrest,df$advise)
```

* Here are the results of the indicator variable creation process:

```rout
> df$arrest <- rep(NA,313)
> df$arrest[df$ta==1] <- 1
> df$arrest[df$ta!=1] <- 0
> table(df$ta,df$arrest)
   
      0   1
  1   0  92
  2 108   0
  3 113   0
> df$advise <- rep(NA,313)
> df$advise <- rep(0,313)
> df$advise[df$ta==2] <- 1
> table(df$ta,df$advise)
   
      0   1
  1  92   0
  2   0 108
  3 113   0
> cat <- paste(df$ta,df$arrest,df$advise)
> table(cat)
cat
1 1 0 2 0 1 3 0 0 
   92   108   113 
> table(df$arrest,df$advise)
   
      0   1
  0 113 108
  1  92   0
> 
```

* Now, here is an intercept-only logistic regression model:

```r
const <- glm(y~1,family=binomial(link="logit"),data=df)
summary(const)
logLik(const)
table(df$y)
```

yielding these results:


```rout
> summary(const)

Call:
glm(formula = y ~ 1, family = binomial(link = "logit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.6341  -0.6341  -0.6341  -0.6341   1.8456  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.5021     0.1465  -10.26   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 297.08  on 312  degrees of freedom
AIC: 299.08

Number of Fisher Scoring iterations: 4

> logLik(const)
'log Lik.' -148.5423 (df=1)
> 
>
> table(df$y)
y
  0   1 
256  57 
> 
```

* Now, let's notice the following equivalence:

```r
57/(256+57)
exp(-1.5021)/(1+exp(-1.5021))
```

```rout
> 57/(256+57)
[1] 0.1821086
> exp(-1.5021)/(1+exp(-1.5021))
[1] 0.1821125
> 
```

* Next, we estimate the "free" model allowing the failure rate to vary between the treatment-as-assigned groups:

```r
free <- glm(y~1+arrest+advise,family=binomial(link="logit"),data=df)
summary(free)
logLik(free)
```
* Here are the results:

```rout
> free <- glm(y~1+arrest+advise,family=binomial(link="logit"),data=df)
> summary(free)

Call:
glm(formula = y ~ 1 + arrest + advise, family = binomial(link = "logit"))

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7232  -0.7232  -0.6576  -0.4797   2.1067  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.2078     0.2235  -5.404 6.52e-08 ***
arrest       -0.8963     0.4027  -2.226    0.026 *  
advise       -0.2136     0.3303  -0.647    0.518    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.56  on 310  degrees of freedom
AIC: 297.56

Number of Fisher Scoring iterations: 4

> logLik(free)
'log Lik.' -145.7792 (df=3)
> 
```

* Let's reconsider the treatment-as-assigned and failure contingency table:

```r
table(df$y,df$ta)
```



```rout
> table(df$y,df$ta)
   
     1  2  3
  0 82 87 87
  1 10 21 26
> 
```

* Using the information in this table, we can calculate the failure rates for each of the three groups:

```rout
> 10/(82+10)
[1] 0.1086957
> 21/(87+21)
[1] 0.1944444
> 26/(87+26)
[1] 0.2300885
> 
```

* We can use the logistic regression model parameter estimates to recover these same failure rates (but for rounding error:

```rout
> logit.arrested <- -1.2078+(-0.8963)
> logit.arrested
[1] -2.1041
> exp(logit.arrested)/(1+exp(logit.arrested))
[1] 0.108699
> 
> logit.advised <- -1.2078+(-0.2136)
> logit.advised
[1] -1.4214
> exp(logit.advised)/(1+exp(logit.advised))
[1] 0.1944422
> 
> logit.separated <- -1.2078
> exp(logit.separated)/(1+exp(logit.separated))
[1] 0.2300905
> 
```

* Notice how `logit.separated` is calculated using only the intercept term from the regression model.
* Next, we calculate the log-likelihood ratio test comparing the constrained and free models:

```r
> logLik(const)
'log Lik.' -148.5423 (df=1)
> logLik(free)
'log Lik.' -145.7792 (df=3)
> logLik(const)-logLik(free)
'log Lik.' -2.763028 (df=1)
> -2*(logLik(const)-logLik(free))
'log Lik.' 5.526057 (df=1)
> pchisq(q=5.526057,df=2)
[1] 0.9368996
> 1-pchisq(q=5.526057,df=2)
[1] 0.06310038
> 
```

* The obtained test statistic has a value of 5.526 with 2 degrees of freedom (constrained model has one parameter estimate and the free model has 3 parameter estimates so the difference between them is 2).
* The p-value for the test is 0.063 which is higher than 0.05. We conclude that the constrained model is more consistent with the data.
* Next, we add a covariate (aggravating circumstances) to the analysis:

```r
free.agg <- glm(y~1+arrest+advise+aggcirc,family=binomial(link="logit"),data=df)
summary(free.agg)
logLik(free.agg)
```

* This analysis yields the following results:

```rout
> free.agg <- glm(y~1+arrest+advise+aggcirc,family=binomial(link="logit"),data=df)
> summary(free.agg)

Call:
glm(formula = y ~ 1 + arrest + advise + aggcirc, family = binomial(link = "logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7513  -0.7012  -0.6397  -0.4686   2.1278  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -1.1206     0.2776  -4.036 5.44e-05 ***
arrest       -0.8765     0.4045  -2.167   0.0303 *  
advise       -0.2052     0.3308  -0.620   0.5350    
aggcirc      -0.1569     0.3016  -0.520   0.6028    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.29  on 309  degrees of freedom
AIC: 299.29

Number of Fisher Scoring iterations: 4

> logLik(free.agg)
'log Lik.' -145.6446 (df=4)
> 
```

* We now consider the meaning of the intercept term in this new model. The estimate of -1.1206 corresponds to the logit for persons in the separate group with no aggravating circumstances. Unlike the earlier models -- which were saturated (meaning we can perfectly reproduce the frequency table cells from the logit parameter estimates) -- this model is not saturated. We can check to see what failure rate is predicted for the separate group with no aggravating circumstances (versus the rate that is actually observed):

```rout
> table(df$y,df$ta,df$aggcirc)
, ,  = 0

   
     1  2  3
  0 25 32 37
  1  3  9 12

, ,  = 1

   
     1  2  3
  0 57 55 50
  1  7 12 14

> 12/(37+12)
[1] 0.244898
> exp(-1.1206)/(1+exp(-1.1206))
[1] 0.2459
> 
```

* As we can see, the rates are not very different (but they are not identical).
* Let's compare this analysis to a saturated model:

```r
free.agg.int <- glm(y~1+arrest+advise+aggcirc+aggcirc*arrest+aggcirc*advise,
  family=binomial(link="logit"),data=df)
summary(free.agg.int)
exp(-1.1260)/(1+exp(-1.1260))
table(df$y,df$ta,df$aggcirc)
```


```rout
> free.agg.int <- glm(y~1+arrest+advise+aggcirc+aggcirc*arrest+aggcirc*advise,
+   family=binomial(link="logit"),data=df)
> summary(free.agg.int)

Call:
glm(formula = y ~ 1 + arrest + advise + aggcirc + aggcirc * arrest + 
    aggcirc * advise, family = binomial(link = "logit"), data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-0.7495  -0.7026  -0.6283  -0.4761   2.1136  

Coefficients:
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -1.1260     0.3322  -3.390   0.0007 ***
arrest          -0.9943     0.6955  -1.430   0.1528    
advise          -0.1425     0.5027  -0.283   0.7768    
aggcirc         -0.1470     0.4492  -0.327   0.7436    
arrest:aggcirc   0.1701     0.8576   0.198   0.8428    
advise:aggcirc  -0.1070     0.6676  -0.160   0.8727    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 297.08  on 312  degrees of freedom
Residual deviance: 291.19  on 307  degrees of freedom
AIC: 303.19

Number of Fisher Scoring iterations: 4

> exp(-1.1260)/(1+exp(-1.1260))
[1] 0.2449
>
> table(df$y,df$ta,df$aggcirc)
, ,  = 0

   
     1  2  3
  0 25 32 37
  1  3  9 12

, ,  = 1

   
     1  2  3
  0 57 55 50
  1  7 12 14

>  
```

* The log-likelihood ratio test comparing the saturated model (with an interaction) to the main-effects only model is:

```rout
> logLik(free.agg)
'log Lik.' -145.6446 (df=4)
> logLik(free.agg.int)
'log Lik.' -145.5942 (df=6)
> -2*(-145.6446-(-145.5942))
[1] 0.1008
```

* With 2 degrees of freedom, this is not a significant difference at any conventional significance level.
* Next, let's check the AIC and BIC results:

```
# AIC results:

> logLik(free.agg)-4
'log Lik.' -149.6446 (df=4)
> logLik(free.agg.int)-6
'log Lik.' -151.5942 (df=6)
> 

# BIC results:

> logLik(free.agg)-4/2*log(313)
'log Lik.' -157.137 (df=4)
> logLik(free.agg.int)-6/2*log(313)
'log Lik.' -162.8328 (df=6)
```

* Based on both AIC and BIC, we would choose the `free.agg model` over the `free.agg.int` model.

##### Practice Problems

1. Use the treatment as delivered and outcome data to construct a chi-square test of independence.
2. Compare the results of the Pearson chi-square test of independence to the likelihood ratio chi-square test.
3. Calculate the failure rates for each of the treatment delivered groups using the contingency table approach compared to the rates implied by the logistic regression analysis.
4. Add the aggravating circumstances variable to the logistic regression model. Interpret the results.
5. Add an interaction term to the logit model you estimated in part 4 to get a saturated model. 
6. Verify that the saturated model recovers the failure rates that can be calculated from the contingency table.
7. Examine the likelihood-ratio test, AIC, and BIC to discern whether the saturated model or the main-effects-only model is more consistent with the data.
