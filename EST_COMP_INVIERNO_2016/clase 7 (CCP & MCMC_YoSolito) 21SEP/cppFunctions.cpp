#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector timesTwo(Rcpp::NumericVector x) {
  return x * 2;
}

// [[Rcpp::export]]
int sampleC(Rcpp::NumericVector prob) {
  // por si el usuario flojo me da sumas que no suman 1
  prob = prob / Rcpp::sum(prob);
  Rcpp::NumericVector cumprob = Rcpp::cumsum(prob);
  // runif regresa un NumericVector que es un 
  // arreglo de doubles......
  double u = Rcpp::runif(1)[0];
  int i = 0;
  while(cumprob[i] < u) {
    i++;
  }
  // vamos a trabajar con indice que empiezan en uno tipo R
  return i + 1;
}

// [[Rcpp::export]]
int mc_transition(int current_state, Rcpp::NumericMatrix trans_mat) {
  // Rcpp:: se vuelve _ si incluyen en el namespace
  Rcpp::NumericVector prob = trans_mat(current_state-1,Rcpp::_ );
  // Creo esto sirve
  int new_state = sampleC(prob);
  return new_state;
}

// [[Rcpp::export]]
Rcpp::NumericVector mc_trajectory(int init_state, int nobs, Rcpp::NumericMatrix trans_mat) {
  // llama al constructor de clase NumericVector
  Rcpp::NumericVector trajectory(nobs + 1);
  trajectory[0] = init_state;
  for(int i=0; i < nobs; i++) {
    trajectory[i + 1] = mc_transition(trajectory[i], trans_mat);
  }
  return trajectory;
}
  
