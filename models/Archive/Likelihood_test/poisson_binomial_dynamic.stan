functions{
  
  /**
   * Return the Poisson-binomial log probability mass for the specified
   * count y and vector of probabilities theta.  The result is the log
   * probability of y successes in N = size(theta) independent
   * trials with probabilities of success theta[1], ..., theta[N].
   *
   * See:  https://en.wikipedia.org/wiki/Poisson_binomial_distribution
   *
   * @param y number of successes
   * @param theta vector of probabilities
   * @return Poisson-binomial log probability mass
   */
   
  real poisson_binomial_lpmf(int y, vector theta) {
    int N = rows(theta);                              //Determine the number of attempts
    matrix[N + 1, N + 1] alpha;                       //initiate the matrix
    vector[N] log_theta = log(theta);                 //take the log of success probabilities
    vector[N] log1m_theta = log1m(theta);              //Take the log of failure probabilities

    if (y < 0 || y > N)
      reject("poisson binomial variate y out of range, y = ", y,   // Number of successes has to be between 0-N
             " N = ", N);
    for (n in 1:N)                                                // Check if success probabilities are between 0-1
      if (theta[n] < 0 || theta[n] > 1)
        reject("poisson binomial parameter out of range,",
               " theta[", n, "] =", theta[n]);

    if (N == 0)
      return y == 0 ? 0 : negative_infinity();      // the prob of observing a failure if N = 0 is 1 so check if N = 0
      
    //first consider the special cases if y=0 or y=N
    if (y == 0) {
      real lp = 0;
      for (i in 1:N) {
        lp += log1m(theta[i]);
      }
      return lp;
    }
    
    if (y == N) {
      real lp = 0;
      for (i in 1:N) {
        lp += log(theta[i]);
      }
      return lp;
    }

    // dynamic programming with cells
    // alpha[n + 1, tot + 1] = log prob of tot successes in first n trials
    alpha = rep_matrix(negative_infinity(), N + 1, N + 1);
    
    alpha[1, 1] = 0;      // before considering any attempts, the prob of observing a failure = 1 -> log() = 0 
    for (n in 1:N) {      // Loop through units
      // tot = 0
      alpha[n + 1, 1] = alpha[n, 1] + log1m_theta[n]; // prob of 0 successes after considering the nth unit

      // 0 < tot < n
      for (tot in 1:min(y, n - 1)) // You loop over the possible successes, if a successrate lower then n-1 is observed there is no need to go further, n-1 because we cant have more as we havent considered unit n yet
        alpha[n + 1, tot + 1]
            = log_sum_exp(alpha[n, tot] + log_theta[n], 
                          alpha[n, tot  + 1] + log1m_theta[n]);  // calculates the prob of obs tot successes 

      // tot = n
      if (n > y) continue;   // check if the current MU number is greater than the successes continues if the case
      alpha[n + 1, n + 1] = alpha[n, n] + log_theta[n]; // calculate the prob that all units fire
    }
    return alpha[N + 1, y + 1];
  }}
  

data {
  int<lower=1> N;
  vector[N] theta;
}

generated quantities {

  vector[N + 1] log_pmf;

  for (y in 0:N)
    log_pmf[y + 1] = poisson_binomial_lpmf(y | theta);
}
