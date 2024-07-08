setwd(paste("C:/Users/Ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/Disciplinas/7 P/Estatistica Bayesiana/A2/a2-bayesian-statistics", sep = ""))

library(readr)
library(rstanarm)
library(loo)
library(bayesplot)

hayes <- read.table("data/hayes+roth/hayes-roth.data", header = TRUE, sep = ",")
hayes <- hayes[, -1]
hayes[hayes[,5] == 2 | hayes[,5] == 3, 5] <- 0

head(hayes)

# Standardize the data
hayes[,1] <- hayes[,1] - mean(hayes[,1]) / (2*sd(hayes[,1]))
hayes[,2] <- hayes[,2] - mean(hayes[,2]) / (2*sd(hayes[,2]))
hayes[,3] <- hayes[,3] - mean(hayes[,3]) / (2*sd(hayes[,3]))
hayes[,4] <- hayes[,4] - mean(hayes[,4]) / (2*sd(hayes[,4]))

# Fit the models
hayes_fit_11 <- stan_glm(X1.2 ~ ., 
                            data = hayes, 
                            family = binomial(link = "logit"),
                            prior = normal(0, 2.5),
                            prior_intercept = normal(0, 2.5))

hayes_fit_12 <- stan_glm(X1.2 ~ .,
                            data = hayes,
                            family = binomial(link = "probit"),
                            prior = normal(0, 2.5),
                            prior_intercept = normal(0, 2.5))

hayes_fit_21 <- stan_glm(X1.2 ~ .,
                            data = hayes,
                            family = binomial(link = "logit"),
                            prior = student_t(7, 0, 2.5),
                            prior_intercept = student_t(7, 0, 2.5))

hayes_fit_22 <- stan_glm(X1.2 ~ .,
                            data = hayes,
                            family = binomial(link = "probit"),
                            prior = student_t(7, 0, 2.5),
                            prior_intercept = student_t(7, 0, 2.5))

hayes_fit_31 <- stan_glm(X1.2 ~ .,
                            data = hayes,
                            family = binomial(link = "logit"),
                            prior = cauchy(0, 2.5),
                            prior_intercept = cauchy(0, 2.5))

hayes_fit_32 <- stan_glm(X1.2 ~ .,
                            data = hayes,
                            family = binomial(link = "probit"),
                            prior = cauchy(0, 2.5),
                            prior_intercept = cauchy(0, 2.5))

# Print results
summary(hayes_fit_11)
summary(hayes_fit_12)
summary(hayes_fit_21)
summary(hayes_fit_22)
summary(hayes_fit_31)
summary(hayes_fit_32)

# Plot the results
color_scheme_set("mix-blue-red")

mcmc_trace(hayes_fit_11, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/hayes_trace_plot_11.png')
dev.off()

mcmc_trace(hayes_fit_12, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/hayes_trace_plot_12.png')
dev.off()

mcmc_trace(hayes_fit_21, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/hayes_trace_plot_21.png')
dev.off()

mcmc_trace(hayes_fit_22, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/hayes_trace_plot_22.png')
dev.off()

mcmc_trace(hayes_fit_31, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/hayes_trace_plot_31.png')
dev.off()

mcmc_trace(hayes_fit_32, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/hayes_trace_plot_32.png')
dev.off()

color_scheme_get("red")

mcmc_intervals(hayes_fit_11) + scale_x_continuous(limits = c(-2, 3))
dev.copy(png,'figures/hayes_intervals_plot_11.png')
dev.off()

mcmc_intervals(hayes_fit_12) + scale_x_continuous(limits = c(-2, 3))
dev.copy(png,'figures/hayes_intervals_plot_12.png')
dev.off()

mcmc_intervals(hayes_fit_21) + scale_x_continuous(limits = c(-2, 3))
dev.copy(png,'figures/hayes_intervals_plot_21.png')
dev.off()

mcmc_intervals(hayes_fit_22) + scale_x_continuous(limits = c(-2, 3))
dev.copy(png,'figures/hayes_intervals_plot_22.png')
dev.off()

mcmc_intervals(hayes_fit_31) + scale_x_continuous(limits = c(-2, 3))
dev.copy(png,'figures/hayes_intervals_plot_31.png')
dev.off()

mcmc_intervals(hayes_fit_32) + scale_x_continuous(limits = c(-2, 3))
dev.copy(png,'figures/hayes_intervals_plot_32.png')
dev.off()


# LOO-CV
loo_hayes_fit_11 <- loo(hayes_fit_11)
loo_hayes_fit_12 <- loo(hayes_fit_12)
loo_hayes_fit_21 <- loo(hayes_fit_21)
loo_hayes_fit_22 <- loo(hayes_fit_22)
loo_hayes_fit_31 <- loo(hayes_fit_31)
loo_hayes_fit_32 <- loo(hayes_fit_32)

# Compare the models
loo_compare(loo_hayes_fit_11, loo_hayes_fit_12, loo_hayes_fit_21, loo_hayes_fit_22, loo_hayes_fit_31, loo_hayes_fit_32)
