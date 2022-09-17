calc_nsc <- function(vec_anet, tau = 1.5){
  ## vec_anet is the Anet annual time series
  
  ## initialize
  vec_nsc <- vec_anet * NA
  
  ## Equilibrate NSC based on first 10 years
  nsc_init <- mean(vec_anet[1:10], na.rm = TRUE) * tau
  vec_nsc[1] <- nsc_init
  
  ## first-order decay
  for (idx in 2:length(vec_anet)){
    vec_nsc[idx] <- vec_nsc[idx-1] + vec_anet[idx] - (1/tau) * vec_nsc[idx-1]
  }
  
  # tibble(x = 1:length(vec_anet),
  #        anet = vec_anet/vec_anet[1],
  #        nsc = vec_nsc/vec_nsc[1]) %>% 
  #   ggplot() +
  #   geom_line(aes(x, anet)) +
  #   geom_line(aes(x, nsc), color = "red")
  
  return(vec_nsc)
}