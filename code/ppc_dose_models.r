setwd(paste("C:/Users/Ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/Disciplinas/7 P/Estatistica Bayesiana/A2/a2-bayesian-statistics", sep = ""))

library(readr)
library(ggplot2)
library(cmdstanr)
library(bayesplot)
library(tidyr)
library(posterior)
library(miscTools)

dose <- read_delim("data/dose.csv")
dose$prop <- dose$deaths / dose$animals
dose$z_dose <- (dose$dose - mean(dose$dose)) / (2*sd(dose$dose))

# Data for Stan model
X <- model.matrix(~ z_dose, dose)
data_11 <- list(X = X, 
                N = nrow(dose),
                P = ncol(X),
                prior = 1,
                link = 1)

data_12 <- list(X = X,
                N = nrow(dose),
                P = ncol(X),
                prior = 1,
                link = 2)

data_21 <- list(X = X,
                N = nrow(dose),
                P = ncol(X),
                prior = 2,
                link = 1)

data_22 <- list(X = X,
                N = nrow(dose),
                P = ncol(X),
                prior = 2,
                link = 2)

data_31 <- list(X = X,
                N = nrow(dose),
                P = ncol(X),
                prior = 3,
                link = 1)

data_32 <- list(X = X,
                N = nrow(dose),
                P = ncol(X),
                prior = 3,
                link = 2)

# Fit the models
model <- cmdstan_model("code/logistic_model.stan") 
fit_11 <- model$sample(data = data_11,
                        chains = 4,
                        parallel_chains = 4,
                        fixed_param = TRUE)

fit_12 <- model$sample(data = data_12,
                        chains = 4,
                        parallel_chains = 4,
                        fixed_param = TRUE)

fit_21 <- model$sample(data = data_21,
                        chains = 4,
                        parallel_chains = 4,
                        fixed_param = TRUE)

fit_22 <- model$sample(data = data_22,
                        chains = 4,
                        parallel_chains = 4,
                        fixed_param = TRUE)

fit_31 <- model$sample(data = data_31,
                        chains = 4,
                        parallel_chains = 4,
                        fixed_param = TRUE)

fit_32 <- model$sample(data = data_32,
                        chains = 4,
                        parallel_chains = 4,
                        fixed_param = TRUE)

# Compute draws
draws_11 <- fit_11$draws("prob", format = "matrix")
draws_12 <- fit_12$draws("prob", format = "matrix")
draws_21 <- fit_21$draws("prob", format = "matrix")
draws_22 <- fit_22$draws("prob", format = "matrix")
draws_31 <- fit_31$draws("prob", format = "matrix")
draws_32 <- fit_32$draws("prob", format = "matrix")

# Plot the results
y <- dose$prop

ppc_stat(y, draws_11, stat = "mean")
dev.copy(png,'figures/dose_ppd_mean_11.png')
dev.off()

ppc_stat(y, draws_12, stat = "mean")
dev.copy(png,'figures/dose_ppd_mean_12.png')
dev.off()

ppc_stat(y, draws_21, stat = "mean")
dev.copy(png,'figures/dose_ppd_mean_21.png')
dev.off()

ppc_stat(y, draws_22, stat = "mean")
dev.copy(png,'figures/dose_ppd_mean_22.png')
dev.off()

ppc_stat(y, draws_31, stat = "mean")
dev.copy(png,'figures/dose_ppd_mean_31.png')
dev.off()

ppc_stat(y, draws_32, stat = "mean")
dev.copy(png,'figures/dose_ppd_mean_32.png')
dev.off()
