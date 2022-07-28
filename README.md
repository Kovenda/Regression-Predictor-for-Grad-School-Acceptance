# Regression-Predictor-for-Grad-School-Acceptance
A linear regression predictor with a primary goal of predicting the chance of admission for a candidate. 

# Data 
* The primary goal is to determine what factors are most predictive of Chance of Admit ( ranging from 0 to 1 ). 
* The predictor  variables include GRE Scores ( out of 340 ), TOEFL Scores ( out of 120 ), University Rating ( out of 5 ), Statement of Purpose and Letter of Recommendation Strength ( out of 5 ), Undergraduate GPA ( out of 10 ), Research Experience ( either 0 or 1 ). 
* There are 400 observations in the data set. 
* These data on Graduate Admissions from 2019 has variables about students at a small liberal arts college.

## Loading Data
```{r}
library(readr)
Admission_Predict_withMissingValues <- read_csv("/Volumes/GoogleDrive/My Drive/Luther College 3rd Year/Fall 2020/Math 327/Projects/Project 1/Admission_Predict.csv")
Admission_Predict <- na.omit(Admission_Predict_withMissingValues)
```
|GRE Score|TOEFL Score|University Rating|SOP|LOR|CGPA|Research|Chance of Admit|
|---|---|---|---|---|---|---|---|
|337|118|4|4.5|4.5|9.65|1|0.92|
|324|118|107|4.0|4.5|8.87|1|0.76|

## Data Characteristics:
**Response variable**:
> Chance of Admit - Chance of admission for grad school applicants

**Predictor variables**:

* > GRE Score,TOEFL Score, University Rating, SOP, LOR, CGPA, Research 

# Exploratory Analysis
## Check distribution of repsonse variable 
```{r}
attach (Admission_Predict)
par (mfrow = c(1, 2))
hist (`Chance of Admit`)
boxplot (`Chance of Admit`, horizontal = T, xlab="`Chance of Admit`", main="Boxplot of Chance of Admit")
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/responsedistibution.jpg?raw=true)
> The distribution of `Chance of Admit` is left skewed. We will use residual analysis to guide the need for transformation later on in the report. From our exploratory analysis we saw tha the variable chance of Admit is left skewed. Therefore we will try different transformations.

**Response Transformation**
```{r}
admission_chance=(`Chance of Admit`)^2
par (mfrow = c(1, 2))
hist ((`Chance of Admit`)^2)
boxplot (admission_chance, horizontal = T, xlab="(`Chance of Admit`)^2", main="Boxplot of Chance of Admit")
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/responsetransformation.jpg?raw=true)
> The distribution of (`Chance of Admit`)^2 is more symmetric. We will start by modeling `Chance of Admit` and then use the Box-Cox analysis to determine the most appropriate transformation.

**Distributions of the quantitative predictor variables:**
```{r}
library (ggplot2)
library (tidyr)
ggplot(gather(Admission_Predict [, 2:8]), aes(value)) + 
  geom_histogram(bins = 8) + 
  facet_wrap(~key, scales = 'free_x')
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/DistributionsOfQP.jpg?raw=true)
> The predictor variables are not either exteremly right or left skewwed. We think the variables Research and University Rating are categorical variables but we will use boxplot to see weather they have an influence on the response variable which is chance of admission.

**Scatterplot matrix**
```{r}
pairs (Admission_Predict[,2:9], col=`University Rating`)
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/scatterplotmatrix.jpg?raw=true)
> The Exploratory Analysis shows that all our variables are quantative except University Rating and Research.

**Table of pairwise correlations**
```{r}
cormat = cor (Admission_Predict [,2:9], use = "complete.obs") 
round (cormat, 2)
```
||GRE Score| TOEFL Score| University Rating|  SOP | LOR |CGPA |Research |Chance of Admit|
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| GRE Score | 1.00 | 0.84 | 0.67 | 0.61 | 0.56 | 0.83 | 0.58 | 0.80 |
| TOEFL Score | 0.84 | 1.00 | 0.70 | 0.66 | 0.57 | 0.83 | 0.49 | 0.79 |
| University Rating | 0.67 | 0.70 | 1.00 | 0.73 | 0.66 | 0.75 | 0.45 | 0.71 | 
| SOP | 0.61 | 0.66 | 0.73 | 1.00 | 0.73 | 0.72 | 0.44 | 0.68 |
| LOR | 0.56 | 0.57 | 0.66 | 0.73 | 1.00 | 0.67 | 0.40 | 0.67 |
| CGPA | 0.83 | 0.83 | 0.75 | 0.72 | 0.67 | 1.00 | 0.52 | 0.87 |
| Research | 0.58 | 0.49 | 0.45 | 0.44 | 0.40 | 0.52 | 1.00 | 0.55 |
| Chance of Admit | 0.80 | 0.79 | 0.71 | 0.68 | 0.67 | 0.87|0.55 | 1.00 | 

> The pairs plot shows all our predictor variables have a linear relationship with our response variable Chance of Admit except for Research. This will be examined further via residual analysis. 

> The three predictors GRE Score, TOEFL Score and CGPA are highly correlated with each other (r = 0.83 to 0.84). However, Research and Letter of Recommendation are not highly correlated with the other predictor variables.

> Since CGPA has the higest correlation with chance of admission, both GRE SCore and the Tofel score has the same correlation to CGPA which means students may not need to take both tests since their correlation to CGPA is the same with r = 0.83. This statement is only an assumption we will confirm this and also recognize other issues of multicollinearity with correlated predictors later.

## Simple Linear Regression
* We start with a simple linear regression of GPA and admission_chance
**Is there a relationship between GPA and admission_chance?**

```{r fig.height=3.5, fig.width=4}
plot ( `Chance of Admit` ~ CGPA, data=Admission_Predict)
fit0 = lm (`Chance of Admit` ~ CGPA, data= Admission_Predict)
summary (fit0)
confint (fit0)
abline (fit0)
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/CGPAvsCA.jpg?raw=true)
> With 95% confidence, Chance of Admit increases between 19.7 and 22.0 % for every CGPA point.

**Residual analysis:**
```{r fig.height=3.5, fig.width=7}
par (mfrow = c(1,2))
plot (fit0, which=1:2)
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/CGPAvsCA_residuals.jpg?raw=true)
> From the Residual Vs Fitted we can see that there is no problem with the linearity but there is non-constant variance. And from the Normal Q-Q we can see there is a problem with the normal distribution. We will try a square transformation for Chance of Admit and CGPA. 

# First Order Model
> fitting a first-order linear model will all seven predictors.
```{r}
order1_fit1 = lm (`Chance of Admit` ~ `GRE Score`+`TOEFL Score`+ `University Rating`+ SOP + LOR + CGPA + Research)
summary (order1_fit1)
anova (order1_fit1)
```
> The analysis of variance table suggests that all the predictors are significant because they each have a p-value less than 0.05. The coefficient tests suggest that all of the predictors are significant except for Statement of Purpose (SOP) and University Rating. The R-squared is 0.8035, with adjusted R-squared = 0.8, which indicate that most of the variability in Chance of Admission (Chance of Admit) is being explained by this model.  The residual standard error is  0.06378.

**Residual Analysis of the first order model**
```{r}
par (mfrow = c(1, 2))
plot (order1_fit1, which = c(1, 2))
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/ResponsevsALL_residuals.jpg?raw=true)

**Actuals vs Fitted**
```{r}
boxplot (order1_fit1$residuals, ylab="order1_fit1 Residuals")
plot (order1_fit1$fitted.values,`Chance of Admit`, main="Actual vs. Fitted", ylab="Residuals")
abline (0, 1, col="red")
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/Actualsvsfitted.jpg?raw=true)

* Residual analysis shows linearity but non-constant variance, along with some left skewness in the residual distribution. This could be corrected by transforming the response variable.

**Box-Cox Analysis**
> Box-Cox analysis is one way to choose a response variable transformation from the set of power transformations. It can also just help choose between log and square root.
```{r}
library ("MASS")
boxcox (order1_fit1)
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/boxcox.jpg?raw=true)
> The Box-Cox analysis suggests a power of 2 transformation, The value, 2, is just inside the 95% confidence interval, so we will use a square transformation.

# Second Order Model
```{r}
squareChance_Admit = (`Chance of Admit`)^2
order2_fit1 = lm (squareChance_Admit ~ `GRE Score`+`TOEFL Score`+ `University Rating`+ SOP + LOR + CGPA + Research)
summary (order2_fit1)

anova (order2_fit1)
```
> University Rating is now more significant to the model the p value went from 0.23150 to 0.030033, however Statement of Purpose is still insignificant to the model and we might eliminate it to improve the model. We cant compare the Residual standard error because we changed scales. The Multiple R-squared increased by 0.03.

**Residual analysis of the square-transformed first-order model**
```{r}
par (mfrow = c(1, 2))
plot (order2_fit1, which = c(1, 2))
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/residualsSecondModel.jpg?raw=true)

**Actuals vs Fitted**
```{r}
boxplot (order2_fit1$residuals, horizontal = T, xlab="order2_fit1 Residuals")
plot (order2_fit1$fitted.values,squareChance_Admit, main="Actual vs. Fitted", ylab="Residuals")
abline (0, 1, col="red")
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/actualsvsfitted2ndmodel.jpg?raw=true)

* The residual analysis of the square-transformed model looks fairly like a fairly good improvement from the first order model. There are some residuals at both ends of the scale that are somewhat more spread out compared to a normal distribution. The plot of observed vs fitted squareChance_Admit looks good.

# Backward elimination method - Manual
> I will remove Statement of Purpose (SOP) because its the only one that is insigficant to the square-transformed model.
```{r}
order2_fit2 = lm (squareChance_Admit ~ `GRE Score`+`TOEFL Score`+ `University Rating` + LOR + CGPA + Research)
summary (order2_fit2)

anova (order2_fit2)
```
**Manual Elimination model Residuals**
```{r}
par (mfrow = c(1, 2))
plot (order2_fit2, which = c(1, 2))
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/amanualmodelResiduals.jpg?raw=true)

> I do not see any obvious change in the model after dropping Statement of Purpose (SOP). So we will use ANOVA between the two fits:

```{r}
anova (order2_fit1, order2_fit2)
```
> The p-value for the hypothesis that the two fits are the same is 0.7123. Thus, we conclude that the second fit is not significantly different from the first fit. Therefore removing the Statement of Purpose from the model doesn't improve the model, however, removing it gives us a simpler model so we will do that.

## Changing the order of the predictor variables in the model
* We noticed from our earlier correlation matrix that TOEFL Score and GRE Score are highly correlated to CGPA, we know that this means that the order in we put them into the model affects their Sum of Squares

## Determining Final Order of variables in the model

|Predictors        |      Indivudual SS    |   Individual R Squared|
|---|---|---|
|CGPA               |     12.6889      |       0.7874     |    
|GRE Score           |    10.8206       |      0.6715     |   
|TOEFL Score        |     10.543        |      0.6542      |   
|Research            |    5.2035       |       0.3229 |
|University Rating   |    8.7150        |      0.5408|
|LOR                 |    7.3745        |      0.4576|

We take the varaible CGPA with the highest R-squared value to be first in the model. However to determine which variable comes next in the model, we need to not only to consider the high R-Square values that follow the first but also consider how much of their individual Sum of Squares is lost to CGPA being first in the model since we that CGPA is going to take 12.6889 Sum of Squares of the 13.4253 Total Sum of Squares.

If we re-arrange the table in terms of the set criteria it looks as follows (The number corresponds to the variable's determined position in the coming re-arranged model).

Indivudual SS And Individual R Squared 
1. 12.6889 CGPA 0.7874
2. 10.8206 GRE Score 0.6715
3. 10.543 TOEFL Score 0.6542
4. 8.7150 University Rating 0.5408
5. 7.3745 LOR 0.4576
6. 5.2035 Research 0.3229

This leads us to create a an order of the variables in the following order: 1. CGPA 2. GRE Score 3. TOEFL Score 4. University Rating 5. LOR 6. Research.
```{r}
order2_fit3 = lm (squareChance_Admit ~ CGPA + `GRE Score` + `TOEFL Score` + `University Rating` + LOR + Research )
summary (order2_fit3)

anova (order2_fit3)

```

**Initial Interpretation of the Model**

The predictor variables explain 83.06 % of the change in the square(chance of admission). We think that the intercept does not provide significant information so we won't comment on it. For an increase of 1 CGPA point your chance of admission increases by 16% and coming from a University that is rated 1 point higher than a fellow applicant gives you 1.28% higher chance of admssion. It is interesting to see that an applicant's Letter of Recommedation (LOR) takes precedence over both of the required Standardized test scores, since an increase of 1 point in the score of an applicant's Letter of Recommedation (LOR) increases their chance of admssion by 2.6% while a 1 point increase in the standardized test scores only gives an applicant 0.26% for GRE Score and 0.42% for TOEFL Score. Doing research happens to be the second most important contributor to an applicant's chance of admission after CGPA.

## Stepwise Regression

** Apply a model selection to the first order model**

**First order model**
```{r}
part2_fit1 = lm (`Chance of Admit` ~ `GRE Score`+`TOEFL Score`+ `University Rating`+ SOP + LOR + CGPA + Research)
summary (part2_fit1)
anova (part2_fit1)
```
**Re- fit first order model**
```{r}
par (mfrow = c(1, 2))
plot (part2_fit1, which = c(1, 2))
 
```
**Stepwise Regression**

```{r}
part2_fit2=step(part2_fit1)
```
```{r}

par (mfrow = c(1, 2))
plot (part2_fit2, which = c(1, 2))

```

![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/fit1StepResiduals.jpg?raw=true)

> The Residual vs Fitted plot shows good linearity and constant variables. The noraml Q-Q plot aligns with normal distribution with a left tail.

# Interaction Effects
```{r}
CGPA.c=CGPA-mean(CGPA)
GreScore.c=`GRE Score`-mean(`GRE Score`)
ToeflScore.c=`TOEFL Score`-mean(`TOEFL Score`)
LOR.c=LOR-mean(LOR)
Research.c=Research-mean(Research)

part2_fit3= lm ( `Chance of Admit`~ (CGPA.c + GreScore.c + ToeflScore.c + LOR.c + Research.c)^2 )
summary (part2_fit3)
```

**Stepwise by AIC**

```{r}
part2_fit_AIC = step (part2_fit3, direction="both")
summary (part2_fit_stepAll)
```
**Stepwise by BIC**

```{r}
part2_fit_BIC = step (part2_fit3, direction="both", k=log (part2_fit3$rank + part2_fit3$df.residual))
summary (part2_fit_BIC)
```

**Results from AIC & BIC**

```{r}
summary (part2_fit_AIC)
summary (part2_fit_BIC)
```


```{r}
anova(part2_fit_AIC,part2_fit_BIC)
```

> We conclude from the ANOVA table that the two models are not similar since the p-value of the f-test is 0.0454 which is less than 0.05 and therefore we reject the null hypothesis which says that there is no statistical difference between the two models.

> The the Adjusted R-squared value and the Residual Standard error for both the AIC & BIC Model are as follows:

|Model          |         Adjusted R-square |  Residual Standard error|    
|---|---|---|
|AIC           |          0.8023      |        0.0634   |      
|BIC           |          0.8002        |      0.06374   |    

> The AIC model is the best model because it has a higher Adjusted R-squared value and a lower Residual Standard error.

**Residual Diagnostics for Final Model**

```{r}
#par (mfrow = c(1, 2))
plot (part2_fit2)
 
```
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/lastresiduals.jpg?raw=true)
![alt text](https://github.com/kovenda/Regression-Predictor-for-Grad-School-Acceptance//blob/main/lastresiduals3.jpg?raw=true)














