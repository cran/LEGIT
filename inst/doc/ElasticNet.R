## ------------------------------------------------------------------------
set.seed(1)
library(LEGIT)
N = 500
train = example_2way(N, sigma=.5, logit=FALSE, seed=1)

## ------------------------------------------------------------------------
g1_bad = rbinom(N,1,.30)
g2_bad = rbinom(N,1,.30)
g3_bad = rbinom(N,1,.30)
g4_bad = rbinom(N,1,.30)
g5_bad = rbinom(N,1,.30)
train$G = cbind(train$G, g1_bad, g2_bad, g3_bad, g4_bad, g5_bad)
lv = list(G=train$G, E=train$E)
# Elastic Net
fit = elastic_net_var_select(train$data, lv, y ~ G*E)
summary(fit)

## ------------------------------------------------------------------------
best_model(fit, criterion="BIC")

## ----}{r fig1, fig.height = 5, fig.width = 5-----------------------------
plot(fit)

## ------------------------------------------------------------------------
fit = elastic_net_var_select(train$data, lv, y ~ G*E, cross_validation=TRUE, cv_iter=5, cv_folds=10)
summary(fit)
best_model(fit, criterion="cv_R2")

## ------------------------------------------------------------------------
fit_mychoice = fit$fit[[8]]

## ------------------------------------------------------------------------
# Elastic net only applied on G
fit = elastic_net_var_select(train$data, lv, y ~ G*E, c(1))
# Elastic net only applied on E
fit = elastic_net_var_select(train$data, lv, y ~ G*E, c(2))

## ------------------------------------------------------------------------
# Most E variables not removed, use lambda_mult > 1 to remove more
fit = elastic_net_var_select(train$data, lv, y ~ G*E, c(2), lambda_mult=5)

## ------------------------------------------------------------------------
# Want more lambdas (useful if # of variables is large)
fit = elastic_net_var_select(train$data, lv, y ~ G*E, n_lambda = 200)

