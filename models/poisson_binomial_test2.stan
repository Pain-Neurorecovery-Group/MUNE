functions{
  real poisson_binomial_lpmf(
    int y,
    vector log_theta,
    vector log1m_theta
) {
    int N = rows(log_theta);
    matrix[N + 1, N + 1] alpha;

    if (y < 0 || y > N)
      reject("Poisson-binomial variate out of range");

    if (N == 0)
      return y == 0 ? 0 : negative_infinity();

    alpha[1,1] = 0;

    for (n in 1:N) {

      alpha[n+1,1] =
        alpha[n,1] + log1m_theta[n];

      for (tot in 1:min(y,n-1)) {
        alpha[n+1,tot+1] =
          log_sum_exp(
            alpha[n,tot] + log_theta[n],
            alpha[n,tot+1] + log1m_theta[n]
          );
      }

      if (n <= y)
        alpha[n+1,n+1] =
          alpha[n,n] + log_theta[n];
    }

    return alpha[N+1,y+1];
}}


data {
  int<lower=0> N;
  array[N] int Y;
  vector[N] S;
  int<lower=0> U;
}


parameters {
  real<lower = 0> t_mu0;
  vector[U-1] delta_raw;
  real t_sigma_raw;
}

transformed parameters{
  vector[U] t_mu;
  vector[U-1] delta;
  matrix[N,U] log_pp;
  matrix[N,U] log_1m_pp;
  real t_sigma;
  
  t_mu[1] = t_mu0;
  for (k in 2:U){
    delta[k-1] = exp(delta_raw[k-1]);
    t_mu[k] = t_mu[k-1] + delta[k-1];}
  
  t_sigma = exp(0.6+0.3*t_sigma_raw);
  
  for (k in 1:U){
  for(s in 1:N){
      real z = (S[s] - t_mu[k]) / t_sigma;
      
      log_pp[s,k] = std_normal_lcdf(z);
      log_1m_pp[s,k] = std_normal_lccdf(z);
    }}
}
model {
  for(k in 2:U){
    delta_raw ~ normal(-0.4,1);
  }
  t_sigma_raw ~ std_normal();
  t_mu0 ~ normal(5,3);
  
  for (i in 1:N){
  target += poisson_binomial_lpmf(Y[i]| to_vector(log_pp[i, ]), to_vector(log_1m_pp[i, ]));}
}

