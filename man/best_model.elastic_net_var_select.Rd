% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LEGIT.R
\name{best_model.elastic_net_var_select}
\alias{best_model.elastic_net_var_select}
\title{Best model from elastic net variable selection}
\usage{
\method{best_model}{elastic_net_var_select}(object, criterion, ...)
}
\arguments{
\item{object}{An object of class "elastic_net_var_select", usually, a result of a call to elastic_net_var_select.}

\item{criterion}{Criteria used to determine which model is the best. If \code{search_criterion="AIC"}, uses the AIC, if \code{search_criterion="AICc"}, uses the AICc, if \code{search_criterion="BIC"}, uses the BIC, if \code{search_criterion="cv_R2"}, uses the cross-validation R-squared, if \cr \code{search_criterion="cv_AUC"}, uses the cross-validated AUC, if \code{search_criterion="cv_Huber"}, uses the Huber cross-validation error, if \code{search_criterion="cv_L1"}, uses the L1-norm cross-validation error (Default = "AIC"). The Huber and L1-norm cross-validation errors are alternatives to the usual cross-validation L2-norm error (which the \eqn{R^2} is based on) that are more resistant to outliers. For all criterion, lower is better, with the exception of \code{search_criterion="cv_R2"} and \code{search_criterion="cv_AUC"}.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
Returns the best IMLEGIT model resulting from the glmnet path with associated information.
}
\description{
Best model from elastic net variable selection (based on selected criteria)
}
\examples{
\dontrun{
N = 1000
train = example_3way(N, sigma=1, logit=FALSE, seed=7)
g1_bad = rbinom(N,1,.30)
g2_bad = rbinom(N,1,.30)
g3_bad = rbinom(N,1,.30)
g4_bad = rbinom(N,1,.30)
g5_bad = rbinom(N,1,.30)
train$G = cbind(train$G, g1_bad, g2_bad, g3_bad, g4_bad, g5_bad)
lv = list(G=train$G, E=train$E)
fit = elastic_net_var_select(train$data, lv, y ~ G*E)
summary(fit)
best_model(fit, criterion="BIC")
 # Instead of taking the best, if you want the model with "Model index"=17 from summary, do
plot(fit)
# With Cross-validation
fit = elastic_net_var_select(train$data, lv, y ~ G*E, cross_validation=TRUE, cv_iter=1, cv_folds=5)
best_model(fit, criterion="cv_R2")
# Elastic net only applied on G
fit = elastic_net_var_select(train$data, lv, y ~ G*E, c(1))
# Elastic net only applied on E
fit = elastic_net_var_select(train$data, lv, y ~ G*E, c(2))
# Most E variables not removed, use lambda_mult > 1 to remove more
fit = elastic_net_var_select(train$data, lv, y ~ G*E, c(2), lambda_mult=5)
# Lasso (only L1 regularization)
fit = elastic_net_var_select(train$data, lv, y ~ G*E, alpha=1)
# Want more lambdas (useful if # of variables is large)
fit = elastic_net_var_select(train$data, lv, y ~ G*E, n_lambda = 200)
}
}
\references{
Alexia Jolicoeur-Martineau, Ashley Wazana, Eszter Szekely, Meir Steiner, Alison S. Fleming, James L. Kennedy, Michael J. Meaney, Celia M.T. Greenwood and the MAVAN team. \emph{Alternating optimization for GxE modelling with weighted genetic and environmental scores: examples from the MAVAN study} (2017). arXiv:1703.08111.
}
