data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int tech[N];        // techique chosen
    real pay_i[N,K];    // observed personal yields of techs (1/0)
    int bout[N];        // processing bout per individual
    int id[N];          // individual id
    int N_effects;      // number of learning parameters to estimate
}

parameters {
    vector[N_effects] mu;                     //population means
    vector<lower=0>[N_effects] sigma_i;       // standard deviations of varying effects
    matrix[N_effects,J] zed_i;                // individual z-scores for cholesky decomp
    cholesky_factor_corr[N_effects] L_Rho_i;  // correlation matrix
}

transformed parameters{
    matrix[J,N_effects] I;              //define varying effects for individuals
    I = (diag_pre_multiply(sigma_i,L_Rho_i) * zed_i)'; //cholesky decomp majick
}

model {
    vector[K] AC;       // attraction scores
    real logPrA;           // asocial learning Pr
    real phi;           // stickiness parameter to recent experience
    real lambda;        // sensitivity to attraction scores


   //priors
    mu[1] ~ normal(0,1.2);
    mu[2] ~ normal(0,1.2);
    sigma_i ~ exponential(1);
    to_vector(zed_i) ~ normal(0,1);
    L_Rho_i ~ lkj_corr_cholesky(4);
    
    //liklihood loop
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi)*AC[j] + phi*(pay_i[i-1,j]) ;
            } else {
                AC[j] = 1;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            lambda = exp( mu[1] + I[id[i],1] ) ;
            phi= inv_logit(  mu[2] + I[id[i],2] );
        }

        logPrA = lambda*AC[tech[i]] - log_sum_exp( lambda*AC );
        target += logPrA;
     }//i  

}//end of model

generated quantities{
    //vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    vector[J] lambda_i;           // stickiness parameter
    vector[J] phi_i;           // stickiness parameter
    real lambda;
    real phi;
    matrix[N_effects,N_effects] Rho_i;
    matrix[N,K] PrPreds;     

    Rho_i = L_Rho_i * L_Rho_i';
    lambda = exp(mu[1]);
    phi = inv_logit(mu[2]);
    
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-phi_i[id[i]])*AC[j] + phi_i[id[i]]*pay_i[i-1,j];
            } else {
                AC[j] = 1;
            }
        }//j

        if ( bout[i]==1 ) {
            lambda_i[id[i]] = exp( mu[1] + I[id[i],1] ) ;
            phi_i[id[i]] = inv_logit(  mu[2] + I[id[i],2] );
        }

        logPrA = lambda_i[id[i]]*AC[tech[i]] - log_sum_exp( lambda_i[id[i]]*AC );

         //log_lik[i] = (logPrA);
         for(j in 1:K){
            PrPreds[i,j] = exp( lambda_i[id[i]]*AC[j] - log_sum_exp( lambda_i[id[i]]*AC) );
        }
     }//i  
}//end of model
