data {
    int<lower=0> N;                   // number of data points
    int<lower=0> P;                   // number of predictors (including intercept)
    matrix[N,P] X;                    // predictors (including 1s for intercept)
    int prior;                       // prior specification
    int link;                        // link function
}
model {
   
}
generated quantities {
    vector[P] beta;
    if (prior == 1) {
        for (p in 1:P) {
            beta[p] = normal_rng(0, 2.5);
        }
    } else if (prior == 2) {
        for (p in 1:P) {
            beta[p] = student_t_rng(7, 0, 2.5);
        }
    } else if (prior == 3) {
        for (p in 1:P) {
            beta[p] = cauchy_rng(0, 2.5);
        }
    }
    vector[N] eta = X * beta;
    vector[N] prob;
    if (link == 1) {
        for (n in 1:N) {
            prob[n] = inv_logit(eta[n]);
        }
    } else if (link == 2) {
        for (n in 1:N) {
            prob[n] = Phi(eta[n]);
        }
    }
}