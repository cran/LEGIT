% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LEGIT.R
\name{IMLEGIT}
\alias{IMLEGIT}
\title{Independent Multiple Latent Environmental & Genetic InTeraction (IMLEGIT) model}
\usage{
IMLEGIT(
  data,
  latent_var,
  formula,
  start_latent_var = NULL,
  eps = 0.001,
  maxiter = 100,
  family = gaussian,
  ylim = NULL,
  print = TRUE
)
}
\arguments{
\item{data}{data.frame of the dataset to be used.}

\item{latent_var}{list of data.frame. The elements of the list are the datasets used to construct each latent variable. For interpretability and proper convergence, not using the same variable in more than one latent variable is highly recommended. It is recommended to set names to the list elements to prevent confusion because otherwise, the latent variables will be named L1, L2, ... (See examples below for more details)}

\item{formula}{Model formula. The names of \code{latent_var} can be used in the formula to represent the latent variables. If names(\code{latent_var}) is NULL, then L1, L2, ... can be used in the formula to represent the latent variables. Do not manually code interactions, write them in the formula instead (ex: G*E1*E2 or G:E1:E2).}

\item{start_latent_var}{Optional list of starting points for each latent variable (The list must have the same length as the number of latent variables and each element of the list must have the same length as the number of variables of the corresponding latent variable).}

\item{eps}{Threshold for convergence (.01 for quick batch simulations, .0001 for accurate results).}

\item{maxiter}{Maximum number of iterations.}

\item{family}{Outcome distribution and link function (Default = gaussian).}

\item{ylim}{Optional vector containing the known min and max of the outcome variable. Even if your outcome is known to be in [a,b], if you assume a Gaussian distribution, predict() could return values outside this range. This parameter ensures that this never happens. This is not necessary with a distribution that already assumes the proper range (ex: [0,1] with binomial distribution).}

\item{print}{If FALSE, nothing except warnings will be printed. (Default = TRUE).}
}
\value{
Returns an object of the class "IMLEGIT" which is list containing, in the following order: a glm fit of the main model, a list of the glm fits of the latent variables and a list of the true model parameters (AIC, BIC, rank, df.residual, null.deviance) for which the individual model parts (main, genetic, environmental) don't estimate properly.
}
\description{
Constructs a generalized linear model (glm) with latent variables using alternating optimization. This is an extension of the LEGIT model to accommodate more than 2 latent variables.
}
\examples{
train = example_2way(500, 1, seed=777)
fit_best = IMLEGIT(train$data, list(G=train$G, E=train$E), y ~ G*E, 
list(train$coef_G, train$coef_E))
fit_default = IMLEGIT(train$data, list(G=train$G, E=train$E), y ~ G*E)
summary(fit_default)
summary(fit_best)
train = example_3way_3latent(500, 1, seed=777)
fit_best = IMLEGIT(train$data, train$latent_var, y ~ G*E*Z, 
list(train$coef_G, train$coef_E, train$coef_Z))
fit_default = IMLEGIT(train$data, train$latent_var, y ~ G*E*Z)
summary(fit_default)
summary(fit_best)
}
\references{
Alexia Jolicoeur-Martineau, Ashley Wazana, Eszter Szekely, Meir Steiner, Alison S. Fleming, James L. Kennedy, Michael J. Meaney, Celia M.T. Greenwood and the MAVAN team. \emph{Alternating optimization for GxE modelling with weighted genetic and environmental scores: examples from the MAVAN study} (2017). arXiv:1703.08111.
}
