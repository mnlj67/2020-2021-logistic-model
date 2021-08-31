#import data 
PL<- read.csv("C:/Users/chenjun/Desktop/2020-2021 Premier League.csv")
View(PL)

X_home <- model.matrix( ~ 0+HomeTeam, data=PL)
X_away <- model.matrix( ~ 0+AwayTeam, data=PL)

View(X_home)
View(X_away)

X <- as.data.frame(X_home - X_away)
View(X)
nrow(X)
ncol(X)

names(X) <- substring(names(X), 9)
View(X)

library(stringr)
names(X)<-str_replace_all(names(X), c(" " = "." , "," = "" ))

# To avoid a singular solution due to collinearity of indicators, we simply
#drop the first team, Arsenal
X <- X[,-1]
View(X)

# Finally, create vector y of home team wins and make data frame
y <- ifelse(PL$FTR == "H", 1, 0)
matchdata <- cbind(y = y, X)

View(matchdata)


# Fit Bradley-Terry model via logistic regression
fit1 <- glm(y ~ 0+., matchdata, family = binomial(link=logit))
summary(fit1)

# sort coefficient
sort(coef(fit1), decreasing = TRUE)

# Recall Arsenal is reference team (estimate is 0)
PL[PL$AwayTeam=="Chelsea", 2:7]
PL[PL$AwayTeam=="Leicester", 2:7]

# We expect a logistic regression to have dispersion parameter 1
# Estimating it from the model fit, we get
fit1$deviance / fit1$df.residual
# which looks not too far from 1

# Home team advantage 
fit2 <- glm(y ~ ., matchdata, family = binomial(link=logit))
summary(fit2)
# Intercept is actually negative!  However, in fact there's quite a lot of
# uncertainty here, so it's just saying there isn't enough data for us to
# distinguish it
confint(fit2, "(Intercept)")

# Compare two teams
# First, get the full Fisher information matrix
Finv_betahat <- summary(fit2)$cov.scaled
# For this confidence region, we just want the submatrix involving Leeds and
# Leicester
# Also, we want the inverse for the Mahalanobis distance
F_lmc <- solve(Finv_betahat[9:10,9:10]) 
F_lmc

# Get the MLEs for these two teams

betahat_lmc <- coef(fit2)[9:10]

# Let's setup a grid of strengths which we'll check the Mahalanobis distance
# against the chi-squared critical value
Leeds <- seq(-1.5, 4, length.out = 300)
Leicester <- seq(-1.5, 4, length.out = 300)

# The outer function evaluates over a grid .... ask for help if you can't
# figure out how this works
HessCR <- outer(Leeds, Leicester, Vectorize(function(beta_l, beta_mc) {
  beta <- c(beta_l, beta_mc)
  t(betahat_lmc - beta) %*% F_lmc %*% (betahat_lmc - beta)
}))

# The image function now lets us colour the part we're interested in
image(Leeds, Leicester, HessCR > qchisq(0.95, 1))
# and mark the location of the MLE for reference
points(betahat_lmc[1], betahat_lmc[2], pch = 3, col = "white")

# You could also add the individual confidence intervals calculated by R as
# horizontal and vertical lines, but note it is doing something called profiling
# to get the confidence interval accounting for all other variables so you will
# notice a small discrepancy between the marginal and the extremes of the joint
abline(h = confint(fit2, "Leicester"))
abline(v = confint(fit2, "Leeds"))



# Hypothesis test
# We need to compute the model with the three teams fixed to have the same
# coefficient.  To do this, simply tell R to remove the individual predictors
# and insert a new predictor formed from all three
fit3 <- glm(y ~ . - Fulham - West.Brom - Sheffield.United
            + I(Fulham + West.Brom + Sheffield.United),
            matchdata, family = binomial(link=logit))
summary(fit3)

# In logistic regression, dispersion is 1, so the likelihood ratio test
# statistic can be found by just taking the difference of the deviances
fit3$deviance - fit2$deviance
# Difference in degrees of freedom (should be 2 as we've replaced 3 coefficients
# with 1)
fit3$df.residual - fit2$df.residual
# Test is against chi-sq(v=2)
qchisq(0.95, 2)
# LR is not larger, therefore not enough evidence to reject ... plausibly all
# teams up for relegation are equally weak



# Prediction
# We need to predict a new match.  Easiest to pull a row from X, zero out and
# then set the home/away teams we need
new_matchdata <- X[1,]
new_matchdata[,1:19] <- 0
new_matchdata$Man.City <- -1
new_matchdata$Man.United <- +1

View(new_matchdata)

# Probability of Man City win is probability of away win, so 1 minus predicted
# probability of home win
1-predict(fit3, new_matchdata, type = "response")
1-predict(fit2, new_matchdata, type = "response") #both fine


# Accuracy
# Split the data up
training <- matchdata[1:150,]
testing <- matchdata[-(1:150),]

# Fit the model on the training data only
fit4 <- glm(y ~ ., training, family = binomial(link=logit))
summary(fit4)

# Predict on the testing
pred <- predict(fit4, testing, type = "response")

# Now produce a table, where we want to compare the truth to our prediction
res <- table(truth = testing$y,
             prediction = ifelse(pred > 0.5, 1, 0))
res

# Hence overall accuracy (%) is
(res[1,1]+res[2,2])/sum(res)*100

