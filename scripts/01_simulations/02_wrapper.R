################################################################
#                         WRAPPER                              #
################################################################
##### Description
# This function calls the model over parameters specified and
# extracts the value of a given varialbe (want) for the last
# timestep. It defaults to extracting equilibrium biomass.# 
# 
################################################################

wrapper <- function(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, chi, years, want = "X_vec"){
  
  if(!want == "All"){
    value <- 
      model(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, chi, years) %>%     # run the model
      filter(time == max(time)) %>%                                          # keep the last timestep only
      pull({{want}})                                                         # return desired variable 
  } else {
    value <- 
      model(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, chi, years) %>%     # run the model
      filter(time == max(time))                                              # keep the last timestep only
  }
  
  return(value)                                                            # Return the value
}
