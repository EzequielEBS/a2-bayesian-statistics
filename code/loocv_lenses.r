setwd(paste("C:/Users/Ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/Disciplinas/7 P/Estatistica Bayesiana/A2/a2-bayesian-statistics", sep = ""))

library(readr)
library(rstanarm)
library(loo)
library(bayesplot)

lenses <- read.table("data/lenses/lenses.data")
lenses <- lenses[, -1]
lenses[lenses[,5] == 1 | lenses[,5] == 2, 5] <- 0
lenses[lenses[,5] == 3, 5] <- 1

# Standardize the data
lenses[,1] <- (lenses[,1] - mean(lenses[,1]))
lenses[,2] <- (lenses[,2] - mean(lenses[,2]))
lenses[,3] <- (lenses[,3] - mean(lenses[,3]))
lenses[,4] <- (lenses[,4] - mean(lenses[,4]))

# Fit the models
lenses_fit_11 <- stan_glm(V6 ~ ., 
                            data = lenses, 
                            family = binomial(link = "logit"),
                            prior = normal(0, 2.5),
                            prior_intercept = normal(0, 2.5))

lenses_fit_12 <- stan_glm(V6 ~ .,
                            data = lenses,
                            family = binomial(link = "probit"),
                            prior = normal(0, 2.5),
                            prior_intercept = normal(0, 2.5))

lenses_fit_21 <- stan_glm(V6 ~ .,
                            data = lenses,
                            family = binomial(link = "logit"),
                            prior = student_t(7, 0, 2.5),
                            prior_intercept = student_t(7, 0, 2.5))

lenses_fit_22 <- stan_glm(V6 ~ .,
                            data = lenses,
                            family = binomial(link = "probit"),
                            prior = student_t(7, 0, 2.5),
                            prior_intercept = student_t(7, 0, 2.5))

lenses_fit_31 <- stan_glm(V6 ~ .,
                            data = lenses,
                            family = binomial(link = "logit"),
                            prior = cauchy(0, 2.5),
                            prior_intercept = cauchy(0, 2.5))

lenses_fit_32 <- stan_glm(V6 ~ .,
                            data = lenses,
                            family = binomial(link = "probit"),
                            prior = cauchy(0, 0.5),
                            prior_intercept = cauchy(0, 0.5))

# Print results
summary(lenses_fit_11)
summary(lenses_fit_12)
summary(lenses_fit_21)
summary(lenses_fit_22)
summary(lenses_fit_31)
summary(lenses_fit_32)

# Plot the results
color_scheme_set("mix-blue-red")
mcmc_trace(lenses_fit_11, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/lenses_trace_plot_11.png')
dev.off()

mcmc_trace(lenses_fit_12, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/lenses_trace_plot_12.png')
dev.off()

mcmc_trace(lenses_fit_21, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/lenses_trace_plot_21.png')
dev.off()

mcmc_trace(lenses_fit_22, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/lenses_trace_plot_22.png')
dev.off()

mcmc_trace(lenses_fit_31, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/lenses_trace_plot_31.png')
dev.off()

mcmc_trace(lenses_fit_32, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/lenses_trace_plot_32.png')
dev.off()

color_scheme_get("red")

mcmc_intervals(lenses_fit_11) + scale_x_continuous(limits = c(-20, 5))
dev.copy(png,'figures/lenses_intervals_plot_11.png')
dev.off()

mcmc_intervals(lenses_fit_12) + scale_x_continuous(limits = c(-20, 5))
dev.copy(png,'figures/lenses_intervals_plot_12.png')
dev.off()

mcmc_intervals(lenses_fit_21) + scale_x_continuous(limits = c(-20, 5))
dev.copy(png,'figures/lenses_intervals_plot_21.png')
dev.off()

mcmc_intervals(lenses_fit_22) + scale_x_continuous(limits = c(-20, 5))
dev.copy(png,'figures/lenses_intervals_plot_22.png')
dev.off()

mcmc_intervals(lenses_fit_31) + scale_x_continuous(limits = c(-20, 5))
dev.copy(png,'figures/lenses_intervals_plot_31.png')
dev.off()

mcmc_intervals(lenses_fit_32) + scale_x_continuous(limits = c(-20, 5))
dev.copy(png,'figures/lenses_intervals_plot_32.png')
dev.off()

# LOO-CV
loo_lenses_fit_11 <- loo(lenses_fit_11, k_threshold = 0.7)
loo_lenses_fit_12 <- loo(lenses_fit_12, k_threshold = 0.7)
loo_lenses_fit_21 <- loo(lenses_fit_21)
loo_lenses_fit_22 <- loo(lenses_fit_22, k_threshold = 0.7)
loo_lenses_fit_31 <- loo(lenses_fit_31, k_threshold = 0.7)
loo_lenses_fit_32 <- loo(lenses_fit_32, k_threshold = 0.7)

# Compare the models
loo_compare(loo_lenses_fit_11, loo_lenses_fit_12, loo_lenses_fit_21, loo_lenses_fit_22, loo_lenses_fit_31, loo_lenses_fit_32)

