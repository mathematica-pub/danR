#' Wrapper to compile model and save as RDS. Only recompiles if stan file has changed
#'
#' @param model Name of model to compile. Exclude the ".stan" ending
#' @param model_folder Path to model, defaults to "models"
#'
#' @export
#'
compile_model <- function(model, model_folder='models') {
    #Recompile if needed
    if (!file.exists(sprintf('%s/%s.RDS', model_folder, model))) {
        comp_model <- rstan::stan_model(sprintf('%s/%s.stan', model_folder, model))
        saveRDS(comp_model, sprintf('%s/%s.RDS', model_folder, model))
    } else {
        comp_model <- readRDS(sprintf('%s/%s.RDS', model_folder, model))
        if (!identical(comp_model@model_code[1],  sprintf('%s/%s.stan', model_folder, model) %>% readLines %>% paste(collapse='\n'))) {
            comp_model <- rstan::stan_model(sprintf('%s/%s.stan', model_folder, model))
            saveRDS(comp_model, sprintf('%s/%s.RDS', model_folder, model))
        }
    }
    return(comp_model)
}
