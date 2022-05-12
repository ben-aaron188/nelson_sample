###############################################################################
###############################################################################
### NELSON sample size estimator for supervised machine learning problems
### Helper functions for population simulation
### nelson_sim_utils.R
###
###############################################################################

# dependencies
library(data.table)

# Define base variables
create_features = function(feat_n, population_size){
  feature_list = list()
  
  for(i in 1:feat_n){
    var_sim = rnorm(population_size)
    feature_list[[i]] = var_sim
  }
  
  return(feature_list)
  
}

## USECASE: returned_feature_list = create_features(feat_n, population_size)

# Convert prop change in var to logodds
change_to_logodds = function(change){
  odds = change/(1-change)
  logodds = log(odds)
  return(logodds)
}



# Define beta coefficients for simulated pop
create_betas = function(feat_n
                        , feat_dist # uniform, normal
                        , mu
                        , sigma
                        , min
                        , max
                        , feat_strengt_unit #logodds, prop
                        , multiplicator = 10
                        ){
  
  
  beta_list = list()
  
  if(feat_dist == 'uniform'){
    if(feat_strengt_unit == 'logodds'){
      
      coef_distr = runif(n = (feat_n*multiplicator)
                         , min = min
                         , max = max)
      
    } else if(feat_strengt_unit == 'prop'){
      
      min_converted = change_to_logodds(min)
      max_converted = change_to_logodds(max)
      
      coef_distr = runif(n = (feat_n*multiplicator)
                         , min = min_converted
                         , max = max_converted)
      
    }
    
  } else if(feat_dist == 'normal'){
    
    if(feat_strengt_unit == 'logodds'){
      
      coef_distr = abs(rnorm(n = (feat_n*multiplicator)
                             , mean = mu
                             , sd = sigma))
      
    } else if(feat_strengt_unit == 'prop'){
      
      mu_converted = change_to_logodds(mu)
      sigma_converted = abs(change_to_logodds(sigma))
      
      # coef_distr = abs(rnorm(n = (feat_n*multiplicator)
      #                    , mean = mu_converted
      #                    , sd = sigma_converted))
      
      coef_distr_ = abs(rnorm(n = (feat_n*multiplicator)
                              , mean = mu
                              , sd = sigma))
      
      coef_distr = change_to_logodds(coef_distr_)
      
    }
    
  }
  
  
  sampled_betas = sample(x = coef_distr
                         , size = feat_n
                         , replace = F)
  
  for(i in 1:feat_n){
    beta_list[[i]] = sampled_betas[i]
  }
  
  return(beta_list)
  
}

## USECASE
# returned_beta_list = create_betas(feat_n = 4
#                                   , feat_dist = 'normal'
#                                   , mu = 0.05
#                                   , sigma = 0.2
#                                   , feat_strengt_unit = 'prop')


# Build base model
simulate_pseudo_population = function(betas_list = returned_beta_list
                                      , features_list = returned_feature_list
                                      , intercept = 0
                                      #, seed = seed
                                      , pop_size = population_size
){
  
  feature_matrix = matrix(data = unlist(features_list)
                          , nrow = length(features_list[[1]])
                          , byrow = F)
  
  beta_matrix = matrix(data = unlist(betas_list)
                       , nrow = length(unlist(betas_list))
                       , ncol = 1)
  
  model_matrix = feature_matrix %*% beta_matrix
  
  z = intercept + model_matrix
  
  pr = 1/(1+exp(-z))
  
  #set.seed(seed*5)
  y = rbinom(n = pop_size
             , size = 1
             , prob = pr)
  
  df_outcome = as.data.frame(y)
  df_features = as.data.frame(feature_matrix)
  
  base_dt = setDT(cbind(df_outcome, df_features))
  
  feature_name_vec = paste0('var_', 1:length(betas_list))
  
  names(base_dt) = c('y', feature_name_vec)
  
  return(base_dt)   
}

## USECASE 
# pseudo_population = simulate_pseudo_population(returned_beta_list
#                                                , returned_feature_list
#                                                , intercept = 0
#                                                , pop_size = population_size)


# WRAPPER
make_pseudo_population = function(feat_n
                                  , population_size
                                  , feat_dist = 'uniform'
                                  , feat_strength_unit = 'prop'
                                  , min = 0.01
                                  , max = 0.20
                                  , mu = 0.05
                                  , sigma = 0.10
                                  , seed = 123
                                  ){
  
  set.seed(seed)
  
  returned_feature_list = create_features(feat_n, population_size)
  returned_beta_list = create_betas(feat_n = feat_n
                                    , feat_dist = feat_dist
                                    , mu = mu
                                    , sigma = sigma
                                    , min = min
                                    , max = max
                                    , feat_strengt_unit = feat_strength_unit)
  pseudo_population = simulate_pseudo_population(returned_beta_list
                                                 , returned_feature_list
                                                 , intercept = 0
                                                 , pop_size = population_size
  )
  
  return(pseudo_population)
  
  #add slots with info from betas
  
}

##USECASE
# p1 = make_pseudo_population(feat_n = 4
#                             , feat_dist = 'normal'
#                             , mu = 0.5
#                             , sigma = 0.1
#                             , population_size = 1e6)


# Check for simulated and given params
nelson_check = function(population){
  
  data = population
  test_glm = (glm( y~.
                   , data = data
                   , family="binomial"))
  
  bal_class_1 = unname(prop.table(table(glm1$data$y))[1])
  bal_class_2 = unname(prop.table(table(glm1$data$y))[2])
  
  print(summary(test_glm))
  
  
}

##USECASE
# nelson_check(p1)
