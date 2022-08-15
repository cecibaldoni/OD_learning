data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int tech[N];        // techique chosen
    int bout[N];        // processing bout per individual
    int id[N];          // individual id
    int N_effects;      // number of learning parameters to estimate
}

model {
    vector[K] AC;       // attraction scores
    real logPrA;           // asocial learning Pr

    
    //liklihood loop
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
                AC[j] = 1 ;
        }//j
        logPrA = AC[tech[i]] - log_sum_exp( AC );
        target += logPrA;
     }//i  
}//end of model

generated quantities{
    //vector[N] log_lik;
    vector[K] AC;       // attraction scores
    real logPrA;        // individual learning temp
    matrix[N,K] PrPreds;     
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
                AC[j] = 1;
        }//j
        logPrA = AC[tech[i]] - log_sum_exp( AC );
         for(j in 1:K){
            PrPreds[i,j] = exp( AC[j] - log_sum_exp( AC) );
        }
     }//i  
}//end of model
