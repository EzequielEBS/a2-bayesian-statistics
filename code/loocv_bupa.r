setwd(paste("C:/Users/Ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/Disciplinas/7 P/Estatistica Bayesiana/A2/a2-bayesian-statistics", sep = ""))

library(readr)
library(rstanarm)
library(loo)
library(bayesplot)

bupa <- read.table("data/liver+disorders/bupa.data", header = FALSE, sep = ",")
bupa[bupa[,7] == 2, 7] <- 0

# Standardize the data
bupa[,1] <- (bupa[,1] - mean(bupa[,1])) / (2*sd(bupa[,1]))
bupa[,2] <- (bupa[,2] - mean(bupa[,2])) / (2*sd(bupa[,2]))
bupa[,3] <- (bupa[,3] - mean(bupa[,3])) / (2*sd(bupa[,3]))
bupa[,4] <- (bupa[,4] - mean(bupa[,4])) / (2*sd(bupa[,4]))
bupa[,5] <- (bupa[,5] - mean(bupa[,5])) / (2*sd(bupa[,5]))
bupa[,6] <- (bupa[,6] - mean(bupa[,6])) / (2*sd(bupa[,6]))

# Fit the models
bupa_fit_11 <- stan_glm(V7 ~ ., 
                        data = bupa, 
                        family = binomial(link = "logit"),
                        prior = normal(0, 2.5),
                        prior_intercept = normal(0, 2.5))

bupa_fit_12 <- stan_glm(V7 ~ .,
                        data = bupa,
                        family = binomial(link = "probit"),
                        prior = normal(0, 2.5),
                        prior_intercept = normal(0, 2.5))

bupa_fit_21 <- stan_glm(V7 ~ .,
                        data = bupa,
                        family = binomial(link = "logit"),
                        prior = student_t(7, 0, 2.5),
                        prior_intercept = student_t(7, 0, 2.5))

bupa_fit_22 <- stan_glm(V7 ~ .,
                        data = bupa,
                        family = binomial(link = "probit"),
                        prior = student_t(7, 0, 2.5),
                        prior_intercept = student_t(7, 0, 2.5))

bupa_fit_31 <- stan_glm(V7 ~ .,
                        data = bupa,
                        family = binomial(link = "logit"),
                        prior = cauchy(0, 2.5),
                        prior_intercept = cauchy(0, 2.5))

bupa_fit_32 <- stan_glm(V7 ~ .,
                        data = bupa,
                        family = binomial(link = "probit"),
                        prior = cauchy(0, 0.5),
                        prior_intercept = cauchy(0, 0.5))

# Print results
summary(bupa_fit_11)
summary(bupa_fit_12)
summary(bupa_fit_21)
summary(bupa_fit_22)
summary(bupa_fit_31)
summary(bupa_fit_32)

# Plot the results
color_scheme_set("mix-blue-red")

mcmc_trace(bupa_fit_11, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/bupa_trace_plot_11.png')
dev.off()

mcmc_trace(bupa_fit_12, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/bupa_trace_plot_12.png')
dev.off()

mcmc_trace(bupa_fit_21, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/bupa_trace_plot_21.png')
dev.off()

mcmc_trace(bupa_fit_22, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/bupa_trace_plot_22.png')
dev.off()

mcmc_trace(bupa_fit_31, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/bupa_trace_plot_31.png')
dev.off()

mcmc_trace(bupa_fit_32, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/bupa_trace_plot_32.png')
dev.off()

color_scheme_get("red")

mcmc_intervals(bupa_fit_11) + scale_x_continuous(limits = c(-4, 4))
dev.copy(png,'figures/bupa_intervals_plot_11.png')
dev.off()

mcmc_intervals(bupa_fit_12) + scale_x_continuous(limits = c(-4, 4))
dev.copy(png,'figures/bupa_intervals_plot_12.png')
dev.off()

mcmc_intervals(bupa_fit_21) + scale_x_continuous(limits = c(-4, 4))
dev.copy(png,'figures/bupa_intervals_plot_21.png')
dev.off()

mcmc_intervals(bupa_fit_22) + scale_x_continuous(limits = c(-4, 4))
dev.copy(png,'figures/bupa_intervals_plot_22.png')
dev.off()

mcmc_intervals(bupa_fit_31) + scale_x_continuous(limits = c(-4, 4))
dev.copy(png,'figures/bupa_intervals_plot_31.png')
dev.off()

mcmc_intervals(bupa_fit_32) + scale_x_continuous(limits = c(-4, 4))
dev.copy(png,'figures/bupa_intervals_plot_32.png')
dev.off()

# LOO-CV
bupa_loo_11 <- loo(bupa_fit_11)
bupa_loo_12 <- loo(bupa_fit_12, k_threshold = 0.7)
bupa_loo_21 <- loo(bupa_fit_21)
bupa_loo_22 <- loo(bupa_fit_22, k_threshold = 0.7)
bupa_loo_31 <- loo(bupa_fit_31)
bupa_loo_32 <- loo(bupa_fit_32)

# Compare the models
loo_compare(bupa_loo_11, bupa_loo_12, bupa_loo_21, bupa_loo_22, bupa_loo_31, bupa_loo_32)

