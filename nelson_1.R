###############################################################################
###############################################################################
### NELSON sample size estimator for supervised machine learning problems
###
###
###############################################################################
rm(list = ls())

# dependencies
library(data.table)

# simulate pseudo population
## meta
# seed = 123
# set.seed(seed)

## parameters
# feat_n = 2
# #feat_strength
# feat_dist = 'uniform'
# #balance
# population_size = 1e6

### Define base variables
create_features = function(feat_n, population_size){
  feature_list = list()
  
  for(i in 1:feat_n){
    var_sim = rnorm(population_size)
    feature_list[[i]] = var_sim
  }

  return(feature_list)
  
}

# returned_feature_list = create_features(feat_n)

### Define beta coefficients
create_betas = function(feat_n
                        , feat_dist # uniform, normal
                        , mu
                        , sigma
                        , feat_strengt_unit #logodds, perc_change
                        , multiplicator = 10
                        ){

  beta_list = list()
  
  if(feat_dist == 'uniform'){
    if(feat_strengt_unit == 'logodds'){
      
    } else if(feat_strengt_unit == 'percentage'){
      
      
    }
    
  } else if(feat_dist == 'normal'){
    
    if(feat_strengt_unit == 'logodds'){
      
    } else if(feat_strengt_unit == 'percentage'){
      
      
    }
    
  }
  
  coef_distr = runif((feat_n*multiplicator), -5, 5)
  
  sampled_betas = sample(x = coef_distr
                         , size = feat_n
                         , replace = F)
  
  for(i in 1:feat_n){
    beta_list[[i]] = sampled_betas[i]
  }
  
  return(beta_list)

}

# returned_beta_list = create_betas(feat_n)

### Fix balance

### Build base model
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

# pseudo_population = simulate_pseudo_population()


# WRAPPER

make_pseudo_population = function(feat_n
                                  , feat_dist = 'uniform'
                                  , population_size
                                  , seed = 123
                                  ){
  
  set.seed(seed)
  
  returned_feature_list = create_features(feat_n, population_size)
  returned_beta_list = create_betas(feat_n)
  pseudo_population = simulate_pseudo_population(returned_beta_list
                                                 , returned_feature_list
                                                 , intercept = 0
                                                 , pop_size = population_size
                                                 )
  
  return(pseudo_population)
  
  #add slots with info from betas
  
}

p1 = make_pseudo_population(feat_n = 4
                            , population_size = 1e6)

#### check 
population = p1
nelson_check = function(population){
  
  data = population
  test_glm = (glm( y~.
               , data = data
               , family="binomial"))
  
  bal_class_1 = unname(prop.table(table(glm1$data$y))[1])
  bal_class_2 = unname(prop.table(table(glm1$data$y))[2])
  
  
}

glm1 = (glm( y~.
             , data = p1
             , family="binomial"))

summary(glm1)
returned_beta_list

#TODO:
# feature strength via log-odds reversion

# 
# change_to_logodds(0)

# feature strength prior information
# print output statement
# evaluate simulation-to-empirical match DONE
# add slots to class with meta var

###############################################################################
###############################################################################
###############################################################################




# ML model 1
run_ml_model_1 = function(n, metric){
  
}

# 


# Sample from pseudo population, run ML model





# META FUNCTION
## params
### - nfeatures
### - strengths: weak, moderate, strong
### - distr.: uniform, normal
### model
### balance
### split
### metric

##### STEPS within
# 1. simulate pop
# 2. obtain baseline performance (metric!)
# 3. downsample
# 4. assessment with corridor


# VERSIONS
## user: simply user-based input
## dev: full control