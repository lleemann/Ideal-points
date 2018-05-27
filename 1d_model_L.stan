data {
  int<lower=1> J; //Legislators
  int<lower=1> K; //Proposals/bills
  int<lower=1> N; //no. of observations
  int<lower=1> hSP; //head of SP
  int<lower=1> hSVP; //head of SVP
  int<lower=1, upper=J> j[N]; //Legislator for observation n
  int<lower=1, upper=K> k[N]; //proposal for observation n
  int<lower=0, upper=1> y[N]; //vote of observation n
  //int<lower=-1, upper=1> party[J];
}
parameters {
  vector[K] alpha;
  vector[K] beta;
  vector[J] theta;
  //real<lower=0> pbeta;
}
model {
  alpha ~ normal(0, 1); 
  beta ~ normal(0, 1);
  theta ~ normal(0, 1);  
// pbeta ~ normal(1, 1);
//  for (p in 1:J){
//    theta[p] ~ normal(0 + pbeta * party[p], 1); // theta[p]!!!
//  }
  theta[hSP] ~  normal(-2, .1);  //constraints
  theta[hSVP] ~ normal(2, .1);  
  for (n in 1:N){
    y[n] ~ bernoulli_logit(theta[j[n]] * beta[k[n]] - alpha[k[n]]);
  }
}
