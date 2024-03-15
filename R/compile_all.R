#' Wrapper to compile_model which runs for all models in a folder
#'
#' @param models optional regex to determine which models to compile. Defaults to NULL, compiling all
#' @param model_folder Path to models, defaults to "models"
#'
#' @export
#'
compile_all <- function(models=NULL, model_folder='models') {
    if (is.null(models)) {
        models <- dir(model_folder, pattern='.stan') %>% stringr::str_remove('\\.stan')
    }
    for (m in models) {
        print(glue::glue('compiling {m}'))
        compile_model(m, model_folder)
    }
    return(1)
}
