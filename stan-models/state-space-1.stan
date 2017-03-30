data {
  int<lower=0> n_steps;
  vector[n_steps] y;
  real dt;
}
parameters {
  vector[n_steps] x_vel;
  vector[n_steps] x_pos;
  real<lower=0> noise_proc;
  real<lower=0> noise_obs;
}
model {
  // noise_proc ~ cauchy(0, 2.5);
  noise_proc ~ inv_gamma(3, 1);
  // noise_obs ~ cauchy(0, 2.5);
  noise_obs ~ inv_gamma(3,1);

  x_vel[1] ~ normal(0, 2);
  x_pos[1] ~ normal(0, 2);

  for (t in 2:n_steps) {
      x_vel[t] ~ normal(x_vel[t-1], noise_proc);
      x_pos[t] ~ normal(x_pos[t-1] + dt * x_vel[t-1], noise_proc);
      }

  y ~ normal(x_vel, noise_obs);
}
