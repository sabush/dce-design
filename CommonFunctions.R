library(tidyverse)

construct_lambda <- function(choice_sets, level_vec, n_opts){
  allopts <- matrix(
    data.matrix(
      expand.grid(
        lapply(level_vec, function(x) 0:(x-1))) %>% 
        arrange_all()),
    ncol = length(level_vec))
  #Initialise a matrix of the right dimension
  lambda <- matrix(0, ncol = nrow(allopts), nrow = nrow(allopts))
  
  # Iterate over choice sets to get a co-incidence matrix
  for(i in 1:nrow(choice_sets)){
    chset <- matrix(choice_sets[i,], nrow = n_opts, byrow = T)
    
    # Iterate over pairs of alternatives
    for(j in 1:(n_opts - 1)){
      for(k in (j + 1):n_opts){
        # Find the matching treatment for the first option
        j_opt <- 0
        for(s in 1:nrow(allopts)){
          if(isTRUE(all.equal(chset[j,], allopts[s,]))) j_opt <- s
        }
        # Find the matching treatment for the second option
        k_opt <- 0
        for(s in 1:nrow(allopts)){
          if(isTRUE(all.equal(chset[k,], allopts[s,]))) k_opt <- s
        }
        lambda[j_opt, k_opt] <- lambda[j_opt, k_opt] - 1
        lambda[k_opt, j_opt] <- lambda[k_opt, j_opt] - 1
        lambda[j_opt, j_opt] <- lambda[j_opt, j_opt] + 1
        lambda[k_opt, k_opt] <- lambda[k_opt, k_opt] + 1
      }
    }
  }
  
  lambda <- (1/((n_opts**2)*nrow(choice_sets))) * lambda
  return(lambda)
}

# Construct B matrix

construct_contrast <- function(level_vec){
  allopts <- matrix(
    data.matrix(
      expand.grid(
        lapply(level_vec, function(x) 0:(x-1))) %>% 
        arrange_all()),
    ncol = length(level_vec))
  
  bmat <- NULL
  
  for(i in 1:length(level_vec)){
    li <- level_vec[[i]]
    bset <- matrix(unlist(lapply(allopts[,i], function(x) diag(li)[,x+1] %*% contr.poly(li))),
                   nrow = nrow(allopts), byrow = T)
    bmat <- cbind(bmat, bset)
  }
  # Normalise columns and transpose
  bmat <- t(bmat)/sqrt(diag(t(bmat) %*% bmat))
  # Check whether the rows are orthonormal
  if(all(bmat %*% t(bmat) - diag(rep(1, nrow(bmat))) < 1e-10)){
    # If rows are orthonormal then return contrast matrix
    return(bmat)
  } else {
    # If rows are not orthonormal then return error
    stop("Contrast Matrix does not have orthonormal rows")
  }
}

construct_contrast_interactions <- function(level_vec, interactions = 'all'){
  if(is.data.frame(interactions)){
    working_interactions <- interactions
  } else if(interactions == 'all') {
    working_interactions <- t(combn(length(level_vec), 2))
  } else {
    stop("Set of interactions must either be a matrix or 'all'")
  }
  
  # print(working_interactions)
  
  
  allopts <- matrix(
    data.matrix(
      expand.grid(
        lapply(level_vec, function(x) 0:(x-1))) %>% 
        arrange_all()),
    ncol = length(level_vec))
  
  bmat <- NULL
  
  for(i in 1:length(level_vec)){
    li <- level_vec[[i]]
    bset <- matrix(unlist(lapply(allopts[,i], function(x) diag(li)[,x+1] %*% contr.poly(li))),
                   nrow = nrow(allopts), byrow = T)
    bmat <- cbind(bmat, bset)
  }
  
  # Add in interactions
  indices <- cbind(rep(1:length(level_vec), level_vec-1), 1:sum(level_vec-1))
  
  for(i in 1:nrow(working_interactions)){
    int_set <- working_interactions[i,]
    
    # Extract columns of the contrast matrix corresponding to each factor
    int1 <- bmat[,indices[indices[,1] == (int_set[1] %>% unlist()),2]]
    int2 <- bmat[,indices[indices[,1] == (int_set[2] %>% unlist()),2]]
    
    # If there is only one main effect contrast, convert to matrix
    if(is.null(dim(int1))) int1 <- matrix(int1, ncol = 1)
    if(is.null(dim(int2))) int2 <- matrix(int2, ncol = 1)

    # Create interaction effects by cross multiplying main effects contrasts
    r_combs <- expand.grid(1:ncol(int1), 1:ncol(int2))
    int_contrasts <- int1[, r_combs[,1]] * int2[, r_combs[,2]]
    
    # If there is only one interaction contrast, convert to matrix
    if(is.null(dim(int_contrasts))) int_contrasts <- matrix(int_contrasts, ncol = 1)
    
    # All new contrasts
    bmat <- cbind(bmat, int_contrasts)
  }
  
  # Normalise columns and transpose
  bmat <- t(bmat)/sqrt(diag(t(bmat) %*% bmat))
  # Check whether the rows are orthonormal
  if(all(bmat %*% t(bmat) - diag(rep(1, nrow(bmat))) < 1e-10)){
    # If rows are orthonormal then return contrast matrix
    return(bmat)
  } else {
    # If rows are not orthonormal then return error
    stop("Contrast Matrix does not have orthonormal rows")
  }
}

# Calculate C
construct_C <- function(bmat, lambda){
  return(bmat %*% lambda %*% t(bmat))
}

# Calculate optimal determinant of the information matrix

compute_optimal_sum_diff <- function(n_level, n_opts){
  if(n_level == 2) {
    if(n_opts %% 2 == 0) {
      return(n_opts ** 2 /4)
    } else {
      return((n_opts ** 2 - 1)/4)
    }
  } else {
    if(n_level >= n_opts) {
      return(n_opts * (n_opts - 1) / 2)
    } else {
      x = floor(n_opts / n_level)
      y = n_opts %% n_level
      return((n_opts ** 2 - (n_level * x**2 + 2 * x * y + y)) / 2)
    }
  }
}

compute_optimal_det_maineff <- function(level_vec, n_opts){
  # Construct a vector containing the optimal sum of differences
  sumdiffs <- sapply(level_vec, 
                     function(x) compute_optimal_sum_diff(x, n_opts))
  # Calculate the product of the number of levels for each factor
  prodlevels <- prod(level_vec)
  # Compute the optimal determinant
  optdetvec <- ((2 * level_vec * sumdiffs) / 
                  (n_opts ** 2 * (level_vec - 1) * prodlevels)) ** (level_vec - 1)
  return(prod(optdetvec))
}

assess_design_main_effect <- function(choice_sets, level_vec, print_detail = T){
  
  # Error checking
  if(ncol(choice_sets) %% length(level_vec) != 0){
    stop("The number of columns in the choice sets matrix is not divisible by the number of attributes")
  }
  
  n_opts <- ncol(choice_sets) / length(level_vec)
  lambda <- construct_lambda(choice_sets = choice_sets, 
                             level_vec = level_vec, 
                             n_opts = n_opts)
  
  bmat <- construct_contrast(level_vec)
  cmat <- construct_C(bmat, lambda)
  
  # Calculate Det C
  detmatc <- det(cmat)
  detmatc
  
  
  optdet <- compute_optimal_det_maineff(level_vec, n_opts)
  
  numeffects <- nrow(bmat)
  efficiency <- (detmatc / optdet) ** (1 / numeffects) * 100
  
  
  if(print_detail){
    print(choice_sets)
    
    print("The lambda matrix is: ")
    print(lambda)
    
    print("The contrast matrix is: ")
    print(bmat)
    
    print("The C matrix is: ")
    print(cmat)
    
    print(paste0("The determinant of the C matrix is: ", detmatc))
    print(optdet)
    
    print(level_vec)
    print(n_opts)
    print(compute_optimal_det_maineff(level_vec, n_opts))
    print(paste0("Efficiency compared with complete factorial (optimal): ", 
                 efficiency, "%"))
  }
  
  # Return all lambda, B, C matrices, determinant of C and efficiency
  return(list(lambda = lambda, bmat = bmat, cmat = cmat, detmatc = detmatc,
              optdet = optdet, efficiency = efficiency))
}

assess_design_interactions <- function(choice_sets, level_vec, interactions, print_detail = T){
  
  # Error checking
  if(ncol(choice_sets) %% length(level_vec) != 0){
    stop("The number of columns in the choice sets matrix is not divisible by the number of attributes")
  }
  
  n_opts <- ncol(choice_sets) / length(level_vec)
  lambda <- construct_lambda(choice_sets = choice_sets, 
                             level_vec = level_vec, 
                             n_opts = n_opts)
  
  bmat <- construct_contrast_interactions(level_vec, interactions)
  cmat <- construct_C(bmat, lambda)
  
  # Calculate Det C
  detmatc <- det(cmat)
  detmatc
  
  
  # optdet <- compute_optimal_det_maineff(level_vec, n_opts)
  
  # numeffects <- nrow(bmat)
  # efficiency <- (detmatc / optdet) ** (1 / numeffects) * 100
  
  
  if(print_detail){
    print("The lambda matrix is: ")
    print(lambda)
    
    print("The contrast matrix is: ")
    print(bmat)
    
    print("The C matrix is: ")
    print(cmat)
    
    print(paste0("The determinant of the C matrix is: ", detmatc))
    
    # print(paste0("Efficiency compared with complete factorial (optimal): ", 
                 # efficiency, "%"))
  }
  
  # Return all lambda, B, C matrices, determinant of C and efficiency
  return(list(lambda = lambda, bmat = bmat, cmat = cmat, detmatc = detmatc))
}



# Generate choice sets for a given treatment 
generate_options <- function(treatment, generators, level_vec){
  options <- treatment
  for(i in 1:nrow(generators)){
    options <- rbind(options, (treatment + generators[i,]) %% level_vec)
  }
  return(options)
}

# Construct a set of unique choice sets using generators
generate_choiceset <- function(generators, level_vec, treatments = NULL, print_detail = T){
  
  # If a list of treatments has not been specified, use all treatments
  if(is.null(treatments)){
    treatments <- matrix(
      data.matrix(
        expand.grid(
          lapply(level_vec, function(x) 0:(x-1))) %>% 
          arrange_all()),
      ncol = length(level_vec))
  }
  
  # Create a list of treatments
  tmtsplit <- split(t(treatments), rep(1:nrow(treatments), each = ncol(treatments)))
  
  # Generate each treatment into a choice set and order alternatives lexicograpgically
  generated_raw <- NULL
  for(i in 1:nrow(generators)){
    work_gens <- matrix(generators[i,], nrow = 1)
    generated_raw <- append(generated_raw, 
                            lapply(tmtsplit, 
                                   function(x) generate_options(x, work_gens, level_vec) %>% 
                                     as.data.frame() %>% 
                                     arrange_all()))
  }
  
  # Discard non-unique choice sets (where order doesn't matter)
  generated_unique <- unique(generated_raw)
  
  # Print Choice Sets
  choicesets <- t(sapply(generated_unique, function(x) as.numeric(unlist(t(x)))))
  if(print_detail){
    print("The choice sets are:")
    print(choicesets)
  }
  
  # Return choice sets
  return(choicesets)
}

# Wrapper function for design construction
assess_design <- function(level_vec, choicesets = NULL, generators = NULL, 
                          treatments = NULL, interactions = NULL, print_detail = T,
                          contrasts = "OP", addition = "modulo"){
  # Check that one of choicesets and generators are specified
  if(is.null(choicesets) & is.null(generators)){
    stop("At least one of choicesets and generators need to be defined.")
  }
  # If both choicesets and generators are specified then use choicesets
  if(!is.null(choicesets) & !is.null(generators)){
    message("Both choicesets and generators have been defined, the generators will be ignored.")
    generators <- NULL
  }
  
  if(is.null(choicesets)) {
    choicesets <- generate_choiceset(generators, level_vec, treatments, print_detail)
  }
  
  if(is.null(interactions)){
    assessment <- assess_design_main_effect(choicesets, level_vec, print_detail)
  } else {
    assessment <- assess_design_interactions(choicesets, level_vec, interactions, print_detail)
  }
  
  # assessment <- assess_design_main_effect(choicesets, level_vec, print_detail)
  return(append(list(choicesets = choicesets), assessment))
}
