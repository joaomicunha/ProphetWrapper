

#' DefaultArguments
#'
#' Function used to default parameters and generate the grid of parameters
#'
#' @param list_params list of parameters
#' @param debug debbuger
#'
#' @import magrittr
#'
#'



DefaultArguments = function(list_params, debug){

  if(debug){browser()}

  #Defaulting arguments from list_params:

  if(is.null(list_params$changepoint.prior.scale)){list_params$changepoint.prior.scale = 0.05; cat("Defaulting changepoint.prior.scale to 0.05 ...\n")}

  if(is.null(list_params$regressor.prior.scale)){list_params$regressor.prior.scale = 0.05; cat("Defaulting regressor.prior.scale to 0.05 ... \n")}

  if(is.null(list_params$holidays.prior.scale)){list_params$holidays.prior.scale = 10; cat("Defaulting holidays.prior.scale to 10 ... \n")}

  if(is.null(list_params$seasonality.prior.scale)){list_params$seasonality.prior.scale = 10; cat("Defaulting seasonality.prior.scale to 10 ... \n")}

  if(is.null(list_params$weekly.seasonality)){list_params$weekly.seasonality = 'auto'; cat("Defaulting weekly.seasonality to 'auto' ... \n")}

  if(is.null(list_params$yearly.seasonality)){list_params$yearly.seasonality = 'auto'; cat("Defaulting yearly.seasonality to 'auto' ... \n")}

  if(is.null(list_params$daily.seasonality)){list_params$daily.seasonality = 'auto'; cat("Defaulting daily.seasonality to 'auto' ... \n")}

  if(is.null(list_params$log_transformation)){list_params$log_transformation = FALSE; cat("Defaulting log_transformation to FALSE ... \n")}

  if(is.null(list_params$standardize_regressor)){list_params$standardize_regressor = FALSE; cat("Defaulting standardize_regressor to FALSE ... \n")}

  if(is.null(list_params$regressor1)){list_params$regressor1 = "no_regressor"}

  if(is.null(list_params$regressor2)){list_params$regressor2 = "no_regressor"}


  #cleaning the parameters to avoid duplication:

  list_params$changepoint.prior.scale = unique(list_params$changepoint.prior.scale)
  list_params$seasonality.prior.scale = unique(list_params$seasonality.prior.scale)
  list_params$holidays.prior.scale = unique(list_params$holidays.prior.scale)
  list_params$regressor.prior.scale = unique(list_params$regressor.prior.scale)
  list_params$regressor1 = unique(list_params$regressor1) %>% as.character()
  list_params$regressor2 = unique(list_params$regressor2) %>% as.character()

  #Simplify the regressor vectors to avoid duplication when a vector is parsed for variable selection:

  regs = unique( c(list_params$regressor1,  list_params$regressor2))

  #If there is more than 1 unique regressor calculate the combinations without repetition to build the grid.
  #For example if the user parses regressor1 = c("A", "B") and regressor2 = c("B","C") we dont want to train a model that uses B and B as regressor 1 and 2 respectively.
  #Also if the user parses c("A", "B") and c("A", "B") as regressors 1 and 2 respectively, we dont want to train models that use A and B (reg1 and reg2) and B and A (reg1 and reg2) since it is the same thing:
  if(length(regs) > 1){

    regs_final = gtools::combinations(n = length(regs), r = 2, v = regs, repeats.allowed = FALSE) %>%
      data.frame() %>%
      set_names(c("regressor1", "regressor2")) %>%
      mutate(regs = paste0(regressor1,"|",regressor2)) %>%
      select(regs) %>%
      unlist()

    grid = expand.grid(changepoint.prior.scale = list_params$changepoint.prior.scale,
                       seasonality.prior.scale = list_params$seasonality.prior.scale,
                       regressor.prior.scale = list_params$regressor.prior.scale,
                       holidays.prior.scale = list_params$holidays.prior.scale,
                       regressors = regs_final,
                       stringsAsFactors = F) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(regressor1 = stringr::str_split(regressors, pattern = "\\|")[[1]][1],
                    regressor2 = stringr::str_split(regressors, pattern = "\\|")[[1]][2]) %>%
      dplyr::select(-regressors)

  }else{

    grid = expand.grid(changepoint.prior.scale = list_params$changepoint.prior.scale,
                       seasonality.prior.scale = list_params$seasonality.prior.scale,
                       regressor.prior.scale = list_params$regressor.prior.scale,
                       holidays.prior.scale = list_params$holidays.prior.scale,
                       regressor1 = list_params$regressor1,
                       regressor2 = list_params$regressor2,
                       stringsAsFactors = F)

    regs_final = c(paste0(list_params$regressor1, "|", list_params$regressor2))

  }


  #Return the defaulted list_params and the grid_final:
  list(list_params_final = list_params,
       grid_final = grid,
       final_unique_regressors = regs_final)


}





