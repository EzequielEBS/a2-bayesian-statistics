setwd(paste("C:/Users/Ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/Disciplinas/7 P/Estatistica Bayesiana/A2/a2-bayesian-statistics", sep = ""))

library(readr)
library(ggplot2)
library(cmdstanr)
library(bayesplot)
library(tidyr)
library(posterior)
library(miscTools)
library(rstanarm)

## Year: VCF0004
## Income: VCF0114
## Race: VCF0071a
## Gender: VCF0070a
## Party: VCF0303

data0 <- read_delim("data/anes_timeseries_cdf_csv_20220916/anes_timeseries_cdf_csv_20220916.csv")

# Select the columns of interest
data <- data0[, c("VCF0004", "VCF0114", "VCF0071a", "VCF0070a", "VCF0303")]

# Select the rows of interest
data <- data[data$VCF0303 == "1" | data$VCF0303 == "3", ]
data <- data[data$VCF0004 != " ",]
data <- data[data$VCF0114 != " ",]
data <- data[data$VCF0071a != " ",]
data <- data[data$VCF0070a != " ",]
data[data[,5] == "3", 5] <- "0"

# Convert the columns to factors
data$VCF0114 <- as.numeric(data$VCF0114)
data$VCF0071a <- as.numeric(data$VCF0071a)
data$VCF0070a <- as.numeric(data$VCF0070a)
data$VCF0303 <- as.numeric(data$VCF0303)

# Standardize the data
data$VCF0114 <- (data$VCF0114 - mean(data$VCF0114)) / (2*sd(data$VCF0114))
data$VCF0071a <- (data$VCF0071a - mean(data$VCF0071a)) / (2*sd(data$VCF0071a))
data$VCF0070a <- (data$VCF0070a - mean(data$VCF0070a))

data_1988 <- data[data$VCF0004 == 1988,-1]
data_1992 <- data[data$VCF0004 == 1992,-1]
data_1996 <- data[data$VCF0004 == 1996,-1]
data_2000 <- data[data$VCF0004 == 2000,-1]

# Data for stan
X_1988 <- model.matrix(~ VCF0114 + VCF0071a + VCF0070a, data_1988)
X_1992 <- model.matrix(~ VCF0114 + VCF0071a + VCF0070a, data_1992)
X_1996 <- model.matrix(~ VCF0114 + VCF0071a + VCF0070a, data_1996)
X_2000 <- model.matrix(~ VCF0114 + VCF0071a + VCF0070a, data_2000)

stan_data_1988_11 <- list(X = X_1988, 
                            N = nrow(data_1988),
                            P = ncol(X_1988),
                            prior = 1,
                            link = 1)
stan_data_1992_11 <- list(X = X_1992, 
                            N = nrow(data_1992),
                            P = ncol(X_1992),
                            prior = 1,
                            link = 1)
stan_data_1996_11 <- list(X = X_1996, 
                            N = nrow(data_1996),
                            P = ncol(X_1996),
                            prior = 1,
                            link = 1)
stan_data_2000_11 <- list(X = X_2000, 
                            N = nrow(data_2000),
                            P = ncol(X_2000),
                            prior = 1,
                            link = 1)


stan_data_1988_12 <- list(X = X_1988,
                            N = nrow(data_1988),
                            P = ncol(X_1988),
                            prior = 1,
                            link = 2)
stan_data_1992_12 <- list(X = X_1992,
                            N = nrow(data_1992),
                            P = ncol(X_1992),
                            prior = 1,
                            link = 2)
stan_data_1996_12 <- list(X = X_1996,
                            N = nrow(data_1996),
                            P = ncol(X_1996),
                            prior = 1,
                            link = 2)
stan_data_2000_12 <- list(X = X_2000,
                            N = nrow(data_2000),
                            P = ncol(X_2000),
                            prior = 1,
                            link = 2)


stan_data_1988_21 <- list(X = X_1988,
                            N = nrow(data_1988),
                            P = ncol(X_1988),
                            prior = 2,
                            link = 1)
stan_data_1992_21 <- list(X = X_1992,
                            N = nrow(data_1992),
                            P = ncol(X_1992),
                            prior = 2,
                            link = 1)
stan_data_1996_21 <- list(X = X_1996,
                            N = nrow(data_1996),
                            P = ncol(X_1996),
                            prior = 2,
                            link = 1)
stan_data_2000_21 <- list(X = X_2000,
                            N = nrow(data_2000),
                            P = ncol(X_2000),
                            prior = 2,
                            link = 1)


stan_data_1988_22 <- list(X = X_1988,
                            N = nrow(data_1988),
                            P = ncol(X_1988),
                            prior = 2,
                            link = 2)
stan_data_1992_22 <- list(X = X_1992,
                            N = nrow(data_1992),
                            P = ncol(X_1992),
                            prior = 2,
                            link = 2)
stan_data_1996_22 <- list(X = X_1996,
                            N = nrow(data_1996),
                            P = ncol(X_1996),
                            prior = 2,
                            link = 2)
stan_data_2000_22 <- list(X = X_2000,
                            N = nrow(data_2000),
                            P = ncol(X_2000),
                            prior = 2,
                            link = 2)


stan_data_1988_31 <- list(X = X_1988,
                            N = nrow(data_1988),
                            P = ncol(X_1988),
                            prior = 3,
                            link = 1)
stan_data_1992_31 <- list(X = X_1992,
                            N = nrow(data_1992),
                            P = ncol(X_1992),
                            prior = 3,
                            link = 1)
stan_data_1996_31 <- list(X = X_1996,
                            N = nrow(data_1996),
                            P = ncol(X_1996),
                            prior = 3,
                            link = 1)
stan_data_2000_31 <- list(X = X_2000,
                            N = nrow(data_2000),
                            P = ncol(X_2000),
                            prior = 3,
                            link = 1)


stan_data_1988_32 <- list(X = X_1988,
                            N = nrow(data_1988),
                            P = ncol(X_1988),
                            prior = 3,
                            link = 2)
stan_data_1992_32 <- list(X = X_1992,
                            N = nrow(data_1992),
                            P = ncol(X_1992),
                            prior = 3,
                            link = 2)
stan_data_1996_32 <- list(X = X_1996,
                            N = nrow(data_1996),
                            P = ncol(X_1996),
                            prior = 3,
                            link = 2)
stan_data_2000_32 <- list(X = X_2000,
                            N = nrow(data_2000),
                            P = ncol(X_2000),
                            prior = 3,
                            link = 2)

# Fit the models
model <- cmdstan_model("code/logistic_model.stan")
fit_1988_11 <- model$sample(data = stan_data_1988_11,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1988_12 <- model$sample(data = stan_data_1988_12,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1988_21 <- model$sample(data = stan_data_1988_21,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1988_22 <- model$sample(data = stan_data_1988_22,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1988_31 <- model$sample(data = stan_data_1988_31,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1988_32 <- model$sample(data = stan_data_1988_32,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1992_11 <- model$sample(data = stan_data_1992_11,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1992_12 <- model$sample(data = stan_data_1992_12,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1992_21 <- model$sample(data = stan_data_1992_21,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1992_22 <- model$sample(data = stan_data_1992_22,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1992_31 <- model$sample(data = stan_data_1992_31,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1992_32 <- model$sample(data = stan_data_1992_32,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1996_11 <- model$sample(data = stan_data_1996_11,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1996_12 <- model$sample(data = stan_data_1996_12,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1996_21 <- model$sample(data = stan_data_1996_21,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1996_22 <- model$sample(data = stan_data_1996_22,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1996_31 <- model$sample(data = stan_data_1996_31,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_1996_32 <- model$sample(data = stan_data_1996_32,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_2000_11 <- model$sample(data = stan_data_2000_11,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_2000_12 <- model$sample(data = stan_data_2000_12,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_2000_21 <- model$sample(data = stan_data_2000_21,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_2000_22 <- model$sample(data = stan_data_2000_22,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_2000_31 <- model$sample(data = stan_data_2000_31,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

fit_2000_32 <- model$sample(data = stan_data_2000_32,
                            chains = 4,
                            parallel_chains = 4,
                            fixed_param = TRUE)

# Compute draws
draws_1988_11 <- fit_1988_11$draws("prob", format = "matrix")
draws_1988_12 <- fit_1988_12$draws("prob", format = "matrix")
draws_1988_21 <- fit_1988_21$draws("prob", format = "matrix")
draws_1988_22 <- fit_1988_22$draws("prob", format = "matrix")
draws_1988_31 <- fit_1988_31$draws("prob", format = "matrix")
draws_1988_32 <- fit_1988_32$draws("prob", format = "matrix")


draws_1992_11 <- fit_1992_11$draws("prob", format = "matrix")
draws_1992_12 <- fit_1992_12$draws("prob", format = "matrix")
draws_1992_21 <- fit_1992_21$draws("prob", format = "matrix")
draws_1992_22 <- fit_1992_22$draws("prob", format = "matrix")
draws_1992_31 <- fit_1992_31$draws("prob", format = "matrix")
draws_1992_32 <- fit_1992_32$draws("prob", format = "matrix")


draws_1996_11 <- fit_1996_11$draws("prob", format = "matrix")
draws_1996_12 <- fit_1996_12$draws("prob", format = "matrix")
draws_1996_21 <- fit_1996_21$draws("prob", format = "matrix")
draws_1996_22 <- fit_1996_22$draws("prob", format = "matrix")
draws_1996_31 <- fit_1996_31$draws("prob", format = "matrix")
draws_1996_32 <- fit_1996_32$draws("prob", format = "matrix")


draws_2000_11 <- fit_2000_11$draws("prob", format = "matrix")
draws_2000_12 <- fit_2000_12$draws("prob", format = "matrix")
draws_2000_21 <- fit_2000_21$draws("prob", format = "matrix")
draws_2000_22 <- fit_2000_22$draws("prob", format = "matrix")
draws_2000_31 <- fit_2000_31$draws("prob", format = "matrix")
draws_2000_32 <- fit_2000_32$draws("prob", format = "matrix")


# Plot the results
y_1988 <- data_1988$VCF0303

ppc_stat(y_1988, draws_1988_11, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1988_11.png')
dev.off()

ppc_stat(y_1988, draws_1988_12, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1988_12.png')
dev.off()

ppc_stat(y_1988, draws_1988_21, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1988_21.png')
dev.off()

ppc_stat(y_1988, draws_1988_22, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1988_22.png')
dev.off()

ppc_stat(y_1988, draws_1988_31, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1988_31.png')
dev.off()

ppc_stat(y_1988, draws_1988_32, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1988_32.png')
dev.off()


y_1992 <- data_1992$VCF0303

ppc_stat(y_1992, draws_1992_11, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1992_11.png')
dev.off()

ppc_stat(y_1992, draws_1992_12, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1992_12.png')
dev.off()

ppc_stat(y_1992, draws_1992_21, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1992_21.png')
dev.off()

ppc_stat(y_1992, draws_1992_22, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1992_22.png')
dev.off()

ppc_stat(y_1992, draws_1992_31, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1992_31.png')
dev.off()

ppc_stat(y_1992, draws_1992_32, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1992_32.png')
dev.off()


y_1996 <- data_1996$VCF0303

ppc_stat(y_1996, draws_1996_11, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1996_11.png')
dev.off()

ppc_stat(y_1996, draws_1996_12, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1996_12.png')
dev.off()

ppc_stat(y_1996, draws_1996_21, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1996_21.png')
dev.off()

ppc_stat(y_1996, draws_1996_22, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1996_22.png')
dev.off()

ppc_stat(y_1996, draws_1996_31, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1996_31.png')
dev.off()

ppc_stat(y_1996, draws_1996_32, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_1996_32.png')
dev.off()


y_2000 <- data_2000$VCF0303

ppc_stat(y_2000, draws_2000_11, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_2000_11.png')
dev.off()

ppc_stat(y_2000, draws_2000_12, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_2000_12.png')
dev.off()

ppc_stat(y_2000, draws_2000_21, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_2000_21.png')
dev.off()

ppc_stat(y_2000, draws_2000_22, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_2000_22.png')
dev.off()

ppc_stat(y_2000, draws_2000_31, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_2000_31.png')
dev.off()

ppc_stat(y_2000, draws_2000_32, stat = "mean")
dev.copy(png,'figures/anes_ppd_mean_2000_32.png')
dev.off()
