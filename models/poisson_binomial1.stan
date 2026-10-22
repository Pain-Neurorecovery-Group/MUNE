// Functions 
functions{
  
  real lppoibin( int k, int n, array[] int elements , vector pp ) {
    // Calculates the log probability of one specific combination of MU firing and not firing
    // k is number of successes, 
    // n is total number of elements, 
    // elements contains which units fired 
    // pp = firing probabilities
        real lp; 
        lp = 0;      // Set the log probability to 0 to start

        // loop over each unit (1,2,...,n)
        for ( i in 1:n ) {
            int bang;
            bang = 0;   // create a Boolean indicator = 0
            for ( j in 1:k ) if ( elements[j]==i ) bang = 1;  // Check if the j'd item in elements is the unit that we are looping over
            if ( bang==1 ) {
                // If it fired the log probability of firing is log(P(fire of n))
                lp = lp + log(pp[i]);
            } else {
                // If it did not fire the log probability contribution is 1 - log(P(fire of n))
                lp = lp + log1m(pp[i]);
            }
        }

        return lp;
    }
    
     real poissonbinomial_lpmf( int k, int n, vector pp ) {
       // Finds the log probability that k of n units fire, regardless of which Units they are.
       // log(P(U active = k))
       
       // First consider the special case where k = 0
       if (k == 0){
         real lp = 0;
         for (i in 1:n){
           lp += log1m(pp[i]);
         }
         
         return lp;
       }
       
       // Now consider all other cases
        array[n] int x; // Stores all possible unit labels
        array[k] int a; // indexes active units with labels
        int e = 0;
        int h = 0;
        array[k] int r; // Stores the current combination
        vector[ choose( n , k ) ] terms; // is a vector with length of possible combinations of firing units given n and k, 
                                         //stores log-prob for each

        for ( i in 1:n ) x[i] = i; // x = c(1,2,...,n)
        for ( i in 1:k ) a[i] = i; // a = c(1,...,k) 

        
        r = a; // first sequence is just a copy of unit indexes (so if k = 2, r = c(1,2))
        terms[1] = lppoibin( k , n , r , pp ); // Find the log-prob of the first combination of units given k

        // loop to lexicographically generate remaining sets of units
        if ( k > 0 ) {
            int i;  //indexes the position of the calculated log-prob in the terms vector
            int j;
            int nmmp1;
            i = 2; // 1 is already used because terms[1] is already calculated
            nmmp1 = n - k + 1;
            // Algorithm for calculating all possible pairs of units given n and k
            while ( a[1] != nmmp1 ) {
                if ( e < n - h ) {
                    h = 1;
                    e = a[k];
                    j = 1;
                    a[ k - h + j ] = e + j;
                } else {
                    e = a[k-h];
                    h = h + 1;
                    j = h;
                    for ( ii in 1:j ) a[ k - h + ii ] = e + ii;
                }
                for ( ii in 1:k ) r[ii] = x[a[ii]];
                terms[i] = lppoibin( k , n , r , pp );
                i = i + 1;
            }
        }
        return log_sum_exp(terms);
    }
}


// Data block.
data {
  int<lower=0> U;    // Number of motor units
  int<lower=0> N;    // Number of obs
  vector[N] CMAP;
  vector[N] S;
}

// The parameters accepted by the model.
parameters {
  real t_mu_mu;                     // Parameters for the between unit variation in threshold mean
  real t_mu_sigma_log;
  
  real t_sigma_mu_log;              // Parameters for the between unit variation in threshold precision
  real t_sigma_sigma_log_log;
  
  real<lower=0> amp_mu;             // Parameters for the between unit variation in amplitude contribution of CMAP
  real<lower=0> amp_sigma;
  
  real noise_mean;                   // Parameters for the added background noise to the CMAP
  real<lower=0> noise_sigma;
  
  vector[U] t_mu;
  vector<lower=0>[U] t_sigma_log;

  
}

transformed parameters{
  matrix[N,U] p;
  real<lower=0> t_sigma_sigma_log;
  vector<lower=0>[U] t_sigma;
  
  t_sigma_sigma_log = exp(t_sigma_sigma_log_log);
  t_sigma = exp(t_sigma_log);
  
   for (s in 1:N) {
    for (u in 1:U) {
      p[s, u] = Phi(
        (S[s] - t_mu[u]) / t_sigma[u]);}}
        
}

// The model
model {
  // priors
  t_mu_mu ~ normal(10,1);
  t_mu_sigma_log ~ normal(0,0.2);
  
  t_sigma_mu_log ~ normal(-1,0.2);
  t_sigma_sigma_log_log ~ normal(-1,0.2);
  
  amp_mu ~ normal(0.05,0.1);
  amp_sigma ~ normal(0.15,0.1);
  
  noise_mean ~ normal(0.05,0.1);
  noise_sigma ~ normal(0.05,0.1);
  
  t_mu ~ normal(t_mu_mu, exp(t_mu_sigma_log));
  t_sigma_log ~ normal(t_sigma_mu_log, t_sigma_sigma_log);

  
  
for (s in 1:N) {
    
    // Stores:
    //
    // log_lik[1] = log P(CMAP | K=0)
    // log_lik[2] = log P(CMAP | K=1)
    // ...
    // log_lik[U+1] = log P(CMAP | K=U)
    
    vector[U+1] log_lik;
    // Consider every possible number of recruited units
    
    for (k in 0:U) {
      // P(K = k | S)
      real log_p_k;
      log_p_k =poissonbinomial_lpmf(k|U, to_vector(p[s,]));
      
      // P(CMAP | K = k)
      real log_cmap_k;
      log_cmap_k = normal_lpdf(CMAP[s] |noise_mean + k * amp_mu,sqrt(square(noise_sigma)+ k * square(amp_sigma)));
      
      
      // P(K=k) * P(CMAP | K=k)
      log_lik[k+1] =log_p_k + log_cmap_k;}   // Add them because log space
      
      

    // Sum over all possible K
    target += log_sum_exp(log_lik);
  }}

