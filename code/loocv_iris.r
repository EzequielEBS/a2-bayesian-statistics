setwd(paste("C:/Users/Ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/Disciplinas/7 P/Estatistica Bayesiana/A2/a2-bayesian-statistics", sep = ""))

library(readr)
library(rstanarm)
library(loo)
library(bayesplot)

iris <- read.table("data/iris/iris.data", sep = ",", header = TRUE)
iris[iris[,5] == "Iris-versicolor", 5] <- 1
iris[iris[,5] != 1, 5] <- 0
iris[,5] <- as.numeric(iris[,5])

# Standardize the data
iris[,1] <- (iris[,1] - mean(iris[,1])) / (2*sd(iris[,1]))
iris[,2] <- (iris[,2] - mean(iris[,2])) / (2*sd(iris[,2]))
iris[,3] <- (iris[,3] - mean(iris[,3])) / (2*sd(iris[,3]))
iris[,4] <- (iris[,4] - mean(iris[,4])) / (2*sd(iris[,4]))

# Fit the models
iris_fit_11 <- stan_glm(Iris.setosa ~ ., 
                        data = iris, 
                        family = binomial(link = "logit"),
                        prior = normal(0, 2.5),
                        prior_intercept = normal(0, 2.5))

iris_fit_12 <- stan_glm(Iris.setosa ~ ., 
                        data = iris, 
                        family = binomial(link = "probit"),
                        prior = normal(0, 2.5),
                        prior_intercept = normal(0, 2.5))

iris_fit_21 <- stan_glm(Iris.setosa ~ .,
                        data = iris,
                        family = binomial(link = "logit"),
                        prior = student_t(7, 0, 2.5),
                        prior_intercept = student_t(7, 0, 2.5))

iris_fit_22 <- stan_glm(Iris.setosa ~ .,
                        data = iris,
                        family = binomial(link = "probit"),
                        prior = student_t(7, 0, 2.5),
                        prior_intercept = student_t(7, 0, 2.5))

iris_fit_31 <- stan_glm(Iris.setosa ~ .,
                        data = iris,
                        family = binomial(link = "logit"),
                        prior = cauchy(0, 2.5),
                        prior_intercept = cauchy(0, 2.5))

iris_fit_32 <- stan_glm(Iris.setosa ~ .,
                        data = iris,
                        family = binomial(link = "probit"),
                        prior = cauchy(0, 2.5),
                        prior_intercept = cauchy(0, 2.5))

# Print results
summary(iris_fit_11)
summary(iris_fit_12)
summary(iris_fit_21)
summary(iris_fit_22)
summary(iris_fit_31)
summary(iris_fit_32)

# Plot the results
color_scheme_set("mix-blue-red")

mcmc_trace(iris_fit_11, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/iris_trace_plot_11.png')
dev.off()

mcmc_trace(iris_fit_12, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/iris_trace_plot_12.png')
dev.off()

mcmc_trace(iris_fit_21, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/iris_trace_plot_21.png')
dev.off()

mcmc_trace(iris_fit_22, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/iris_trace_plot_22.png')
dev.off()

mcmc_trace(iris_fit_31, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/iris_trace_plot_31.png')
dev.off()

mcmc_trace(iris_fit_32, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
dev.copy(png,'figures/iris_trace_plot_32.png')
dev.off()

color_scheme_get("red")

mcmc_intervals(iris_fit_11) + scale_x_continuous(limits = c(-5, 5))
dev.copy(png,'figures/iris_intervals_plot_11.png')
dev.off()

mcmc_intervals(iris_fit_12) + scale_x_continuous(limits = c(-5, 5))
dev.copy(png,'figures/iris_intervals_plot_12.png')
dev.off()

mcmc_intervals(iris_fit_21) + scale_x_continuous(limits = c(-5, 5))
dev.copy(png,'figures/iris_intervals_plot_21.png')
dev.off()

mcmc_intervals(iris_fit_22) + scale_x_continuous(limits = c(-5, 5))
dev.copy(png,'figures/iris_intervals_plot_22.png')
dev.off()

mcmc_intervals(iris_fit_31) + scale_x_continuous(limits = c(-5, 5))
dev.copy(png,'figures/iris_intervals_plot_31.png')
dev.off()

mcmc_intervals(iris_fit_32) + scale_x_continuous(limits = c(-5, 5))
dev.copy(png,'figures/iris_intervals_plot_32.png')
dev.off()

# LOO-CV
iris_loo_11 <- loo(iris_fit_11)
iris_loo_12 <- loo(iris_fit_12)
iris_loo_21 <- loo(iris_fit_21)
iris_loo_22 <- loo(iris_fit_22)
iris_loo_31 <- loo(iris_fit_31)
iris_loo_32 <- loo(iris_fit_32)

# Compare the models
loo_compare(iris_loo_11, iris_loo_12, iris_loo_21, iris_loo_22, iris_loo_31, iris_loo_32)