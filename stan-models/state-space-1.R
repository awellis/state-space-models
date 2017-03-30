library(ggplot2)
library(dplyr)
library(tidyr)
library(vestcog)
library(Rbiips)
library(rstan)
library(bayesplot)

library(viridis)
color_palette <- viridis::plasma(n = 9)

sd2precision <- function(sd) {
    prec <- 1/(sd^2)
    prec
}

neff <- function(weights) {
    1/sum(weights^2)
}

Mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

## generate data ----
trajectory <- vestcog::generate_data(T = 2, amplitude = 20, sensor_sd = 1.7,
                                 as_df = TRUE, seed = TRUE)
trajectory

## model 1 ----
stan_data = list(
    n_steps = length(trajectory$observations),
    dt = 0.1,
    y = trajectory$observations
)

model = stan_model(file='state-space-1.stan')

# run inference
vb = TRUE # if false use MCMC
if (vb) {
    out = vb(
        object = model,
        data = stan_data,
        algorithm = 'meanfield',
        init = 'random',
        iter = 5e3, # default = 10000
        tol_rel_obj = 0.001, # default = 0.01
        grad_samples = 10, # default = 1
        elbo_samples = 100, # default = 100
        output_samples = 5000, # default = 1000
        adapt_engaged = FALSE, # default = TRUE
        eta = 0.2, # default = ADAPTIVE
        seed = 1
    )
} else {
    sampling_iterations = 5e3
    out = sampling(
        object = model,
        data = stan_data,
        chains = 4,
        algorithm = 'NUTS',
        iter = sampling_iterations,
        warmup = sampling_iterations/2,
        refresh = sampling_iterations/10, # show an update every n iterations
        seed = 1
    )
}

# extract results
print(out)
x_hat = apply(rstan::extract(out, pars="x_vel")$x_vel, 2, mean)
x_var = apply(rstan::extract(out, pars="x_vel")$x_vel, 2, var)

# plot estimated latent variables (smoothed time-series)
# ts.plot(stan_data$y, col="red")
# with(trajectory, {plot(time, observations)})
# lines(source,col="black")
# lines(x_hat,col="blue")
# lines(x_hat + sqrt(x_var),col="blue",lty=2)
# lines(x_hat - sqrt(x_var),col="blue",lty=2)

samples <- rstan::extract(out)
