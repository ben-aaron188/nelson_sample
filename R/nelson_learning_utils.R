###############################################################################
###############################################################################
### NELSON sample size estimator for supervised machine learning problems
### Helper functions for supervised learning bootstrap
### nelson_learning_utils.R
###
###############################################################################

# dependencies
library(caret)


# Sample from population
sample_from_population = function(population
                                  , n
                                  , seed){
  
  set.seed(seed)
  pop_sample_idx = sample(1:nrow(population), n, replace = F)
  pop_sample = population[pop_sample_idx, ]
  return(pop_sample)
  
}

##USECASE sample_from_population(p1, 10, 12)

# retrieve values from confusion-matrix
get_metric_from_confmat = function(cmat
                                   , metric = 'acc'){
  
  if(metric == 'acc'){
    m_out = cmat$overall[1]
  } else if(metric){
    #...
  }
  
  return(unname(m_out))
  
}

##USECASE get_metric_from_confmat(cmat_1)

