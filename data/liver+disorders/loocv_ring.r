setwd(paste("C:/Users/Ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/Disciplinas/7 P/Estatistica Bayesiana/A2", sep = ""))

library(readr)
library(rstanarm)
library(loo)
library(bayesplot)

ring <- read.table("data/challenger+usa+space+shuttle+o+ring/o-ring-erosion-only.data")
ring <- ring[, -1]
ring <- ring[,-4]
ring[ring[,1] != 1,1] <- 0

head(ring)
# Standardize the data
ring[,2] <- (ring[,2] - mean(ring[,2])) / (2*sd(ring[,2]))
ring[,3] <- (ring[,3] - mean(ring[,3])) / (2*sd(ring[,3]))

# Fit the models
ring_fit_11 <- stan_glm(V2 ~ ., 
                        data = ring, 
                        family = binomial(link = "logit"),
                        prior = normal(0, 2.5),
                        prior_intercept = normal(0, 2.5))

ring_fit_12 <- stan_glm(V2 ~ .,
                        data = ring,
                        family = binomial(link = "probit"),
                        prior = normal(0, 2.5),
                        prior_intercept = normal(0, 2.5))

ring_fit_21 <- stan_glm(V2 ~ .,
                        data = ring,
                        family = binomial(link = "logit"),
                        prior = student_t(7, 0, 2.5),
                        prior_intercept = student_t(7, 0, 2.5))

ring_fit_22 <- stan_glm(V2 ~ .,
                        data = ring,
                        family = binomial(link = "probit"),
                        prior = student_t(7, 0, 2.5),
                        prior_intercept = student_t(7, 0, 2.5))

ring_fit_31 <- stan_glm(V2 ~ .,
                        data = ring,
                        family = binomial(link = "logit"),
                        prior = cauchy(0, 2.5),
                        prior_intercept = cauchy(0, 2.5))

ring_fit_32 <- stan_glm(V2 ~ .,
                        data = ring,
                        family = binomial(link = "probit"),
                        prior = cauchy(0, 2.5),
                        prior_intercept = cauchy(0, 2.5))

# Print results
summary(ring_fit_11)
summary(ring_fit_12)
summary(ring_fit_21)
summary(ring_fit_22)
summary(ring_fit_31)
summary(ring_fit_32)

# Plot the results
color_scheme_set("mix-blue-red")

mcmc_trace(ring_fit_11, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/ring_trace_plot_11.png')
dev.off()

mcmc_trace(ring_fit_12, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/ring_trace_plot_12.png')
dev.off()

mcmc_trace(ring_fit_21, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/ring_trace_plot_21.png')
dev.off()

mcmc_trace(ring_fit_22, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/ring_trace_plot_22.png')
dev.off()

mcmc_trace(ring_fit_31, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/ring_trace_plot_31.png')
dev.off()

mcmc_trace(ring_fit_32, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/ring_trace_plot_32.png')
dev.off()

color_scheme_get("red")

mcmc_intervals(ring_fit_11) + scale_x_continuous(limits = c(-5, 3))
dev.copy(png,'figures/ring_intervals_plot_11.png')
dev.off()

mcmc_intervals(ring_fit_12) + scale_x_continuous(limits = c(-5, 3))
dev.copy(png,'figures/ring_intervals_plot_12.png')
dev.off()

mcmc_intervals(ring_fit_21) + scale_x_continuous(limits = c(-5, 3))
dev.copy(png,'figures/ring_intervals_plot_21.png')
dev.off()

mcmc_intervals(ring_fit_22) + scale_x_continuous(limits = c(-5, 3))
dev.copy(png,'figures/ring_intervals_plot_22.png')
dev.off()

mcmc_intervals(ring_fit_31) + scale_x_continuous(limits = c(-5, 3))
dev.copy(png,'figures/ring_intervals_plot_31.png')
dev.off()

mcmc_intervals(ring_fit_32) + scale_x_continuous(limits = c(-5, 3))
dev.copy(png,'figures/ring_intervals_plot_32.png')
dev.off()

# LOO-CV
loo_ring_11 <- loo(ring_fit_11)
loo_ring_12 <- loo(ring_fit_12, k_threshold = 0.7)
loo_ring_21 <- loo(ring_fit_21)
loo_ring_22 <- loo(ring_fit_22, k_threshold = 0.7)
loo_ring_31 <- loo(ring_fit_31, k_threshold = 0.7)
loo_ring_32 <- loo(ring_fit_32, k_threshold = 0.7)

# Compare the models
loo_compare(loo_ring_11, loo_ring_12, loo_ring_21, loo_ring_22, loo_ring_31, loo_ring_32)

