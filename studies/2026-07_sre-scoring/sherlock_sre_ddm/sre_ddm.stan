// generated with brms 2.23.0
functions {
  /* Wiener diffusion log-PDF for a single response
   * Args:
   *   y: reaction time data
   *   dec: decision data (0 or 1)
   *   alpha: boundary separation parameter > 0
   *   tau: non-decision time parameter > 0
   *   beta: initial bias parameter in [0, 1]
   *   delta: drift rate parameter
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real wiener_diffusion_lpdf(real y, int dec, real alpha,
                              real tau, real beta, real delta) {
     if (dec == 1) {
       return wiener_lpdf(y | alpha, tau, beta, delta);
     } else {
       return wiener_lpdf(y | alpha, tau, 1 - beta, - delta);
     }
   }
  /* integer sequence of values
   * Args:
   *   start: starting integer
   *   end: ending integer
   * Returns:
   *   an integer sequence from start to end
   */
  array[] int sequence(int start, int end) {
    array[end - start + 1] int seq;
    for (n in 1:num_elements(seq)) {
      seq[n] = n + start - 1;
    }
    return seq;
  }
  // compute partial sums of the log-likelihood
  real partial_log_lik_lpmf(array[] int seq, int start, int end, data vector Y, data array[] int dec, data matrix X, vector b, data matrix X_bs, vector b_bs, data matrix X_ndt, vector b_ndt, real bias, data array[] int J_1, data vector Z_1_1, vector r_1_1, data array[] int J_2, data vector Z_2_1, vector r_2_1, data array[] int J_3, data vector Z_3_bs_1, vector r_3_bs_1, data array[] int J_4, data vector Z_4_ndt_1, vector r_4_ndt_1) {
    real ptarget = 0;
    int N = end - start + 1;
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] bs = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] ndt = rep_vector(0.0, N);
    mu += X[start:end] * b;
    bs += X_bs[start:end] * b_bs;
    ndt += X_ndt[start:end] * b_ndt;
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      mu[n] += r_1_1[J_1[nn]] * Z_1_1[nn] + r_2_1[J_2[nn]] * Z_2_1[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      bs[n] += r_3_bs_1[J_3[nn]] * Z_3_bs_1[nn];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      int nn = n + start - 1;
      ndt[n] += r_4_ndt_1[J_4[nn]] * Z_4_ndt_1[nn];
    }
    bs = exp(bs);
    ndt = exp(ndt);
    for (n in 1:N) {
      int nn = n + start - 1;
      ptarget += wiener_diffusion_lpdf(Y[nn] | dec[nn], bs[n], ndt[n], bias, mu[n]);
    }
    return ptarget;
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  array[N] int<lower=0,upper=1> dec;  // decisions
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> K_bs;  // number of population-level effects
  matrix[N, K_bs] X_bs;  // population-level design matrix
  int<lower=1> K_ndt;  // number of population-level effects
  matrix[N, K_ndt] X_ndt;  // population-level design matrix
  int grainsize;  // grainsize for threading
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  array[N] int<lower=1> J_2;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  array[N] int<lower=1> J_3;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_bs_1;
  // data for group-level effects of ID 4
  int<lower=1> N_4;  // number of grouping levels
  int<lower=1> M_4;  // number of coefficients per level
  array[N] int<lower=1> J_4;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_4_ndt_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  real min_Y = min(Y);
  array[N] int seq = sequence(1, N);
}
parameters {
  vector[K] b;  // regression coefficients
  vector[K_bs] b_bs;  // regression coefficients
  vector[K_ndt] b_ndt;  // regression coefficients
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  array[M_2] vector[N_2] z_2;  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  array[M_3] vector[N_3] z_3;  // standardized group-level effects
  vector<lower=0>[M_4] sd_4;  // group-level standard deviations
  array[M_4] vector[N_4] z_4;  // standardized group-level effects
}
transformed parameters {
  real bias = 0.5;  // initial bias parameter
  vector[N_1] r_1_1;  // actual group-level effects
  vector[N_2] r_2_1;  // actual group-level effects
  vector[N_3] r_3_bs_1;  // actual group-level effects
  vector[N_4] r_4_ndt_1;  // actual group-level effects
  // prior contributions to the log posterior
  real lprior = 0;
  r_1_1 = (sd_1[1] * (z_1[1]));
  r_2_1 = (sd_2[1] * (z_2[1]));
  r_3_bs_1 = (sd_3[1] * (z_3[1]));
  r_4_ndt_1 = (sd_4[1] * (z_4[1]));
  lprior += normal_lpdf(b | 1, 1.5);
  lprior += normal_lpdf(b_bs | 0.4, 0.5);
  lprior += normal_lpdf(b_ndt | -1.9, 0.3);
  lprior += exponential_lpdf(sd_1 | 2);
  lprior += exponential_lpdf(sd_2 | 2);
  lprior += exponential_lpdf(sd_3 | 4);
  lprior += exponential_lpdf(sd_4 | 6);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, dec, X, b, X_bs, b_bs, X_ndt, b_ndt, bias, J_1, Z_1_1, r_1_1, J_2, Z_2_1, r_2_1, J_3, Z_3_bs_1, r_3_bs_1, J_4, Z_4_ndt_1, r_4_ndt_1);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
  target += std_normal_lpdf(z_4[1]);
}
generated quantities {
}

