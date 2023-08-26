# GENERALIZED LINEAR MODELLING - LOGISTIC REGRESSION

## **LOAD LIBRARIES**

```{r}
library(tidyverse)
library(ggpubr)
library(lawstat)
library(emmeans)
library(pROC)
library(car)
library(gtsummary)
library(flextable)
library(report)
```

## **ATTACH DATA**

```{r}
df <- read.csv(file.choose())
df <- na.omit(df)
attach(df)
View(df)
```

## **DESCRIPTIVE ANALYSIS**

```{r}
table1 <- tbl_summary(df[c("PClass","Age","Sex","Survived")])
table1
```

## **LOGISTIC REGRESSION ASSUMPTIONS**

1.  Binary response variable: Logistic regression assumes that the response variable only takes on two possible outcomes.

2.  Independence of observations: Logistic regression assumes that the observations in the dataset are independent of each other.

3.  Multicollinearity among explanatory variables: This occurs when two or more explanatory variables are highly correlated to each other, such that they do not provide unique or independent information in the regression model.

    ```{r}
    vif(fitall)
    ```

4.  No extreme outliers: Logistic regression assumes that there are no extreme outliers or influential observations in the dataset.

    ```{r}
    plot(fitall, which=4, id.n=3)
    ```

5.  Linear relationship between explanatory variables and logit of response variable: Logistic regression assumes that there exists a linear relationship between each continuous explanatory variable and the logit of the response variable.

    ```{r}
    logit <- log(fitall$fitted.values/(1 - fitall$fitted.values))
    ggplot(data=df,aes(Age,logit)) + geom_point() + geom_smooth()
    ```

6.  Sufficiently large sample size: Logistic regression assumes that the sample size of the dataset if large enough to draw valid conclusions from the fitted logistic regression model.

    ```{r}
    (10*Number_of_explanatory_variables)/Expected_probability_of_least_frequency_outcome_occuring
    ```

## **UNIVARIATE LOGISTIC REGRESSION MODELS**

```{r}
fit1 <- glm(data=df, family="binomial", Survived ~ PClass)
fit2 <- glm(data=df, family="binomial", Survived ~ Age)
fit3 <- glm(data=df, family="binomial", Survived ~ Sex)
report(fit1); report(fit2); report(fit3)

table2 <- tbl_uvregression(df[c("PClass","Age","Sex","Survived")], method=glm, y="Survived", exponentiate=T)
table2
```

## MULTIVARIATE LOGISTIC REGRESSION MODEL

```{r}
fitall <- glm(data=df, family="binomial", Survived ~ PClass + Age + Sex)
report(fitall)

table3 <- tbl_regression(fitall, exponentiate=T)
table3

table4 <- tbl_merge(tbls = list(table2, table3), tab_spanner = c("**Univariate**", "**Multivariate**"))
table4
```

## ASSESSING MODEL PERFORMANCE

```{r}
roc(Survived, fitall$fitted.values, plot=T)
```
