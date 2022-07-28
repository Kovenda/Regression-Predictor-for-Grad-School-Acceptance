library(readr)
Admission_Predict_withMissingValues <- read_csv("/Volumes/GoogleDrive/My Drive/Luther College 3rd Year/Fall 2020/Math 327/Projects/Project 1/Admission_Predict.csv")
Admission_Predict <- na.omit(Admission_Predict_withMissingValues)
Admission_Predict

jpeg('responsedistibution.jpg')
attach (Admission_Predict)
par (mfrow = c(1, 2))
hist (`Chance of Admit`)
boxplot (`Chance of Admit`, horizontal = T, xlab="`Chance of Admit`", main="Boxplot of Chance of Admit")
dev.off()

jpeg('responsetransformation.jpg')
admission_chance=(`Chance of Admit`)^2
par (mfrow = c(1, 2))
hist ((`Chance of Admit`)^2)
boxplot (admission_chance, horizontal = T, xlab="(`Chance of Admit`)^2", main="Boxplot of Chance of Admit")
dev.off()

jpeg('DistributionsOfQP.jpg')
library (ggplot2)
library (tidyr)
ggplot(gather(Admission_Predict [, 2:8]), aes(value)) + 
  geom_histogram(bins = 8) + 
  facet_wrap(~key, scales = 'free_x')
dev.off()

jpeg('scatterplotmatrix.jpg')
pairs (Admission_Predict[,2:9], col=`University Rating`)
dev.off()

cormat = cor (Admission_Predict [,2:9], use = "complete.obs") 
round (cormat, 2)

jpeg('CGPAvsCA.jpg')
plot ( `Chance of Admit` ~ CGPA, data=Admission_Predict)
fit0 = lm (`Chance of Admit` ~ CGPA, data= Admission_Predict)
summary (fit0)
confint (fit0)
abline (fit0)
dev.off()

jpeg('CGPAvsCA_residuals.jpg')
par (mfrow = c(1,2))
plot (fit0, which=1:2)
dev.off()

sqrChanceOfAdm = (`Chance of Admit`)^2
plot ( sqrChanceOfAdm ~ CGPA, data=Admission_Predict)
fit1 = lm (sqrChanceOfAdm ~ CGPA, data= Admission_Predict)
summary (fit1)
confint (fit1)
abline (fit1)

par (mfrow = c(1,2))
plot (fit1, which=1:2)

order1_fit1 = lm (`Chance of Admit` ~ `GRE Score`+`TOEFL Score`+ `University Rating`+ SOP + LOR + CGPA + Research)

summary (order1_fit1)
anova (order1_fit1)

# New code chunk with residuals vs. `University Rating`
par (mfrow = c(1, 2))
plot (`University Rating`[!is.na(`Chance of Admit`)], order1_fit1$residuals, ylab="order1_fit1 Residuals", xlab="`University Rating`")
lines (lowess (`University Rating`[!is.na(`Chance of Admit`)], order1_fit1$residuals))
boxplot (order1_fit1$residuals ~ `University Rating`, ylab="order1_fit1 Residuals",
         xlab="`University Rating`")

jpeg('Actualsvsfitted.jpg')
boxplot (order1_fit1$residuals, ylab="order1_fit1 Residuals")
plot (order1_fit1$fitted.values,`Chance of Admit`, main="Actual vs. Fitted", ylab="Residuals")
abline (0, 1, col="red")
dev.off()

jpeg('boxcox.jpg')
library ("MASS")
boxcox (order1_fit1)
dev.off()


squareChance_Admit = (`Chance of Admit`)^2
order2_fit1 = lm (squareChance_Admit ~ `GRE Score`+`TOEFL Score`+ `University Rating`+ SOP + LOR + CGPA + Research)
summary (order2_fit1)

anova (order2_fit1)

jpeg('residualsSecondModel.jpg')
par (mfrow = c(1, 2))
plot (order2_fit1, which = c(1, 2))
dev.off()

jpeg('actualsvsfitted2ndmodel.jpg')
boxplot (order2_fit1$residuals, horizontal = T, xlab="order2_fit1 Residuals")
plot (order2_fit1$fitted.values,squareChance_Admit, main="Actual vs. Fitted", ylab="Residuals")
abline (0, 1, col="red")
dev.off()

order2_fit2 = lm (squareChance_Admit ~ `GRE Score`+`TOEFL Score`+ `University Rating` + LOR + CGPA + Research)
summary (order2_fit2)

anova (order2_fit2)

jpeg('manualmodelResiduals.jpg')
par (mfrow = c(1, 2))
plot (order2_fit2, which = c(1, 2))
dev.off()

anova (order2_fit1, order2_fit2)

order2_fit3 = lm (squareChance_Admit ~ CGPA + `GRE Score` + `TOEFL Score` + `University Rating` + LOR + Research )
summary (order2_fit3)

anova (order2_fit3)

part2_fit1 = lm (`Chance of Admit` ~ `GRE Score`+`TOEFL Score`+ `University Rating`+ SOP + LOR + CGPA + Research)
summary (part2_fit1)
anova (part2_fit1)

part2_fit2=step(part2_fit1)

jpeg('fit1StepResiduals.jpg')
par (mfrow = c(1, 2))
plot (part2_fit2, which = c(1, 2))
dev.off()

CGPA.c=CGPA-mean(CGPA)
GreScore.c=`GRE Score`-mean(`GRE Score`)
ToeflScore.c=`TOEFL Score`-mean(`TOEFL Score`)
LOR.c=LOR-mean(LOR)
Research.c=Research-mean(Research)

part2_fit3= lm ( `Chance of Admit`~ (CGPA.c + GreScore.c + ToeflScore.c + LOR.c + Research.c)^2 )
summary (part2_fit3)

part2_fit_AIC = step (part2_fit3, direction="both")
summary (part2_fit_stepAll)

part2_fit_BIC = step (part2_fit3, direction="both", k=log (part2_fit3$rank + part2_fit3$df.residual))
summary (part2_fit_BIC)

summary (part2_fit_AIC)
summary (part2_fit_BIC)

anova(part2_fit_AIC,part2_fit_BIC)

jpeg("lastresiduals.jpg")
plot (part2_fit2)
dev.off()

jpeg("lastresiduals3.jpg")


par (mfrow = c(1, 2))
plot (part2_fit2, which = c(1, 2))

dev.off()

