% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LEGIT.R
\name{LEGIT_cv}
\alias{LEGIT_cv}
\title{Cross-validation for the LEGIT model}
\usage{
LEGIT_cv(
  data,
  genes,
  env,
  formula,
  cv_iter = 5,
  cv_folds = 10,
  folds = NULL,
  Huber_p = 1.345,
  classification = FALSE,
  start_genes = NULL,
  start_env = NULL,
  eps = 0.001,
  maxiter = 100,
  family = gaussian,
  ylim = NULL,
  seed = NULL,
  id = NULL,
  crossover = NULL,
  crossover_fixed = FALSE,
  lme4 = FALSE,
  test_only = FALSE
)
}
\arguments{
\item{data}{data.frame of the dataset to be used.}

\item{genes}{data.frame of the variables inside the genetic score \emph{G} (can be any sort of variable, doesn't even have to be genetic).}

\item{env}{data.frame of the variables inside the environmental score \emph{E} (can be any sort of variable, doesn't even have to be environmental).}

\item{formula}{Model formula. Use \emph{E} for the environmental score and \emph{G} for the genetic score. Do not manually code interactions, write them in the formula instead (ex: G*E*z or G:E:z).}

\item{cv_iter}{Number of cross-validation iterations (Default = 5).}

\item{cv_folds}{Number of cross-validation folds (Default = 10). Using \code{cv_folds=NROW(data)} will lead to leave-one-out cross-validation.}

\item{folds}{Optional list of vectors containing the fold number for each observation. Bypass cv_iter and cv_folds. Setting your own folds could be important for certain data types like time series or longitudinal data.}

\item{Huber_p}{Parameter controlling the Huber cross-validation error (Default = 1.345).}

\item{classification}{Set to TRUE if you are doing classification (binary outcome).}

\item{start_genes}{Optional starting points for genetic score (must be the same length as the number of columns of \code{genes}).}

\item{start_env}{Optional starting points for environmental score (must be the same length as the number of columns of \code{env}).}

\item{eps}{Threshold for convergence (.01 for quick batch simulations, .0001 for accurate results).}

\item{maxiter}{Maximum number of iterations.}

\item{family}{Outcome distribution and link function (Default = gaussian).}

\item{ylim}{Optional vector containing the known min and max of the outcome variable. Even if your outcome is known to be in [a,b], if you assume a Gaussian distribution, predict() could return values outside this range. This parameter ensures that this never happens. This is not necessary with a distribution that already assumes the proper range (ex: [0,1] with binomial distribution).}

\item{seed}{Seed for cross-validation folds.}

\item{id}{Optional id of observations, can be a vector or data.frame (only used when returning list of possible outliers).}

\item{crossover}{If not NULL, estimates the crossover point of \emph{E} using the provided value as starting point (To test for diathesis-stress vs differential susceptibility).}

\item{crossover_fixed}{If TRUE, instead of estimating the crossover point of E, we force/fix it to the value of "crossover". (Used when creating a diathes-stress model) (Default = FALSE).}

\item{lme4}{If TRUE, uses lme4::lmer or lme4::glmer; Note that is an experimental feature, bugs may arise and certain functions may fail. Currently only summary(), plot(), GxE_interaction_test(), LEGIT(), LEGIT_cv() work. Also note that the AIC and certain elements ignore the existence of the genes and environment variables, thus the AIC may not be used for variable selection of the genes and the environment. However, the AIC can still be used to compare models with the same genes and environments. (Default=FALSE).}

\item{test_only}{If TRUE, only uses the first fold for training and predict the others folds; do not train on the other folds. So instead of cross-validation, this gives you train/test and you get the test R-squared as output.}
}
\value{
If \code{classification} = FALSE, returns a list containing, in the following order: a vector of the cross-validated \eqn{R^2} at each iteration, a vector of the Huber cross-validation error at each iteration, a vector of the L1-norm cross-validation error at each iteration, a matrix of the possible outliers (standardized residuals > 2.5 or < -2.5) and their corresponding standardized residuals and standardized pearson residuals. If \code{classification} = TRUE, returns a list containing, in the following order: a vector of the cross-validated \eqn{R^2} at each iteration, a vector of the Huber cross-validation error at each iteration, a vector of the L1-norm cross-validation error at each iteration, a vector of the AUC at each iteration, a matrix of the best choice of threshold (based on Youden index) and the corresponding specificity and sensitivity at each iteration, and a list of objects of class "roc" (to be able to make roc curve plots) at each iteration. The Huber and L1-norm cross-validation errors are alternatives to the usual cross-validation L2-norm error (which the \eqn{R^2} is based on) that are more resistant to outliers, the lower the values the better.
}
\description{
Uses cross-validation on the LEGIT model. Note that this is not a very fast implementation since it was written in R.
}
\examples{
\dontrun{
train = example_3way(250, 2.5, seed=777)
# Cross-validation 4 times with 5 Folds
cv_5folds = LEGIT_cv(train$data, train$G, train$E, y ~ G*E*z, cv_iter=4, cv_folds=5)
cv_5folds
# Leave-one-out cross-validation (Note: very slow)
cv_loo = LEGIT_cv(train$data, train$G, train$E, y ~ G*E*z, cv_iter=1, cv_folds=250)
cv_loo
# Test set only
cv_test = LEGIT_cv(train$data, train$G, train$E, y ~ G*E*z, cv_iter=1, cv_folds=5, test_only=TRUE)
cv_test
# Cross-validation 4 times with 5 Folds (binary outcome)
train_bin = example_2way(500, 2.5, logit=TRUE, seed=777)
cv_5folds_bin = LEGIT_cv(train_bin$data, train_bin$G, train_bin$E, y ~ G*E, 
cv_iter=4, cv_folds=5, classification=TRUE, family=binomial)
cv_5folds_bin
par(mfrow=c(2,2))
pROC::plot.roc(cv_5folds_bin$roc_curve[[1]])
pROC::plot.roc(cv_5folds_bin$roc_curve[[2]])
pROC::plot.roc(cv_5folds_bin$roc_curve[[3]])
pROC::plot.roc(cv_5folds_bin$roc_curve[[4]])
}
}
\references{
Denis Heng-Yan Leung. \emph{Cross-validation in nonparametric regression with outliers.} Annals of Statistics (2005): 2291-2310.
}
