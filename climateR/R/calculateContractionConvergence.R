#' Calculate budgets for the Contraction / Convergence approach
#' @details Calculates emission budgets for every country and every year
#' @param data A dataframe column or list which is going to contain the emission budgets. Should already be established and contain the country's base year emissions.
#' @param Ct Convergence factor for every year: Ct = (Year - BaseYear) / (ConvergenceYear -  BaseYear)
#' @param global_emissions Path for global emissions for every year
#' @param population_perc Percentage of every country's population on global population for every year

calculateContractionConvergence = function(data, Ct, global_emissions, population_perc){

  for (i in seq_along(data)[-1]) data[i] <- ((1 - Ct[i]) * data[i-1] / global_emissions[i-1] + Ct[i] * population_perc[i]) * global_emissions[i]
  return(data)

}
