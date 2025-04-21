genetic_algorithm <- function(f, x_min, x_max, cel, pop_size, p_mut, max_iter=100){
  n <- length(x_min)
  results <- list(x_opt = NA, f_opt = NA, 
                  x_hist = matrix(NA, nrow=max_iter, ncol=n),
                  f_hist = rep(NA, max_iter),
                  f_mean = rep(NA, max_iter))
  
  # Init P0
  population <- matrix(sample(c(0,1), pop_size * n * cel, replace = TRUE),
                       nrow = pop_size, ncol = n * cel)
  coordinates <- get_coordinates(population, x_min, x_max, pop_size, cel, n)
  obj_fun <- apply(coordinates, 1, f)
  results$x_opt <- coordinates[which.min(obj_fun), ]
  results$f_opt <- min(obj_fun)
  results$x_hist[1,] <- coordinates[which.min(obj_fun), ]
  results$f_hist[1]  <- min(obj_fun)
  
  for(k in 2:max_iter){
    fitness <- min(obj_fun) / obj_fun 
    fitness <- fitness / sum(fitness)
    
    # Selection
    parents <- rep(NA, pop_size)
    for(i in 1 : pop_size){
      parents[i] <- which.min(cumsum(fitness) < runif(1))  
    }
    
    # Cross-over
    new_population <- matrix(NA, nrow = pop_size, ncol = n*cel)
    for(i in 1 : pop_size){
      p1 <- parents[i]
      p2 <- sample(parents, 1)
      cut_id <- sample(seq(1, n*cel-1), 1)
      new_population[i, seq(1, cut_id)] <- population[p1, seq(1, cut_id)]
      new_population[i, seq(cut_id+1, n*cel)] <- population[p2, seq(cut_id+1, n*cel)]
    }
    
    # Mutation
    for(i in 1 : pop_size){
      g_mut <- which(runif(n*cel) <= p_mut)
      new_population[i, g_mut] <- !new_population[i, g_mut]
    }
    
    population <- new_population
    coordinates <- get_coordinates(population, x_min, x_max, pop_size, cel, n)
    obj_fun <- apply(coordinates, 1, f)
    if(min(obj_fun) < results$f_opt){
      results$x_opt <- coordinates[which.min(obj_fun), ]
      results$f_opt <- min(obj_fun)
    }
    results$x_hist[k,] <- coordinates[which.min(obj_fun), ]
    results$f_hist[k]  <- min(obj_fun)
    results$f_mean[k]  <- mean(obj_fun)
  }

  return(results)
}


bin2int <- function(x) {
  x <- as.logical(x)            # wymuszamy wartości logiczne (0/1 → FALSE/TRUE)
  idx <- which(rev(x))          # znajdź pozycje "1" w odwróconym wektorze
  if (length(idx) == 0) return(0)
  sum(2^(idx - 1))              # przelicz binarkę na liczbę całkowitą
}

get_coordinates <- function(population, x_min, x_max, pop_size, cel, n){
  coordinates <- matrix(NA, nrow=pop_size, ncol=n)
  for(i in 1: pop_size){
    for(j in 1 : n){
      t <- bin2int(population[i, seq((j-1)*cel+1, j*cel)])
      coordinates[i,j] <- (x_max[j]-x_min[j])/(2^cel-1)*t + x_min[j]
    }
  }
  return(coordinates)
}



