
######################## Import some useful libraries ##########################
library(lmtest)
library(readr)
library(glmnet)
library(tseries)
############################ Import the Dataset ################################

df = read_csv('df.csv')
head(df)

############################ OLS Regression: Taylor's Rule #####################

lm = lm(i ~ inflationGap + outputGap, data = df)
summary(lm)

############################ Test OLS Assumptions ##############################

## Autocorrelation: Durbin-Watson
dwtest(lm)

## Linearity: Ramsey (Reset)
resettest(lm)

## Homoscedasticity: Breusch-Pagan, White, Goldfeld-Quandt
bptest(lm)
bptest(lm, studentize = FALSE)
gqtest(lm)

## Normality: Jarque - Bera
jarque.bera.test(resid(lm))

############################  OLS Regression2: squared inflation Gap ###########

sqrinflationGap = df$inflationGap^2
lm = lm(i ~ inflationGap + outputGap + sqrinflationGap, data = df)
summary(lm)

## Linearity: Ramsey (Reset)
resettest(lm)

############################  OLS Regression3: Unemployment Rate & RPPI ########

lm = lm(i ~ inflationGap + outputGap + rppi + u, data = df)
summary(lm)

## Linearity: Ramsey (Reset)
resettest(lm)

