


#' Prophet Wrapper
#'
#' This is a function that wraps up (\href{https://facebook.github.io/prophet/docs/installation.html}{Prophet}) package functionality, expanding the possibilities currently offered by the Facebook developed R package.
#' The main rationale behind the package was to build a reproducible function to test several models simultaneously. The model doesn't perform Cross-Validation (yet!) but allows the user to define a train/test split and evaluate the models produced under different scenarios.
#'
#'
#' @param df A data-frame with a numeric column, a date type column, a regressor and a column train (for train/test split). The 'train' column has to have 1 and 0 only where 1 refer to training set.
#' @param list_params A list with the following parameters:
#'  \itemize{
#'  \item{\strong{target_variable}}  {The name of the target variable to predict. Has to be included in df.}
#'  \item{\strong{changepoint.prior.scale}}  {A regularization parameter (vector or single value) for the automatic changepoint definitions of Prophet to define piecewise trend blocks. If the trend changes are being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default Prophet sets this parameter to 0.05. Increasing it will make the trend more flexible.}
#'  \item{\strong{regressor.prior.scale}}  {A regularization parameter (vector or single value) for the external regressor. If the regressor is being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default Prophet sets this parameter to 0.05. Increasing it will make the effect of regressor more flexible. This parameter is applied to both regressor1 and regressor2 (if parsed).}
#'  \item{\strong{holidays.prior.scale}}   {A regularization parameter (vector or single value) for the holidays effects. If the regressor is being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default this parameter is 10, which provides very little regularization. Reducing this parameter dampens holiday effects. Increasing it will make the holidays effect more flexible.}
#'  \item{\strong{weekly.seasonality}}  {Fit weekly seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{yearly.seasonality}}  {Fit yearly seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{regressor1}}  {The name of the first external regressor (or regressors) to include in the model. It has to exist on df as a column. If a vector is parsed, 1 regressor at a time is tested (as a model parameter essentially). If "no_regressor" is parsed, a univariate time-series model is estimated. "no_regressor" can be parsed as an element of the vector as well. }
#'  \item{\strong{regressor2}}  {The name of the second external regressor (or regressors) to include in the model. It has to exist on df as a column. If a vector is parsed, 1 regressor at a time is tested (in combination with regressor1 if parsed). If "no_regressor" is parsed, a univariate time-series model is estimated. "no_regressor" can be parsed as an element of the vector as well. }
#'  \item{\strong{standardize_regressor}}  {Bool, specify whether this regressor will be standardized prior to fitting. Can be 'auto' (standardize if not binary), True, or False.}
#'  \item{\strong{log_transformation}}  {Bool, specify whether the Target Variable will be log transformed pre-fitting the models or not.}
#' }
#' @param best_model_in A character value either: 'train', 'test', 'mix_train_test' or 'cv'. Defaults to 'test'. This parameter defines the criteria to pick the best model - either based on accuracy on training set or in test set. The user might have different business requirements and wants to understand the general performance of the model cross-validated over a set of periods ('cv').
#' @param train_set_imp_perc A numeric input between 0 and 1 representing the weight to give to the training set results relatively to the test set. Defaults to 0.5 (mean results). This parameter only affects the results when 'best_model_in' is set to 'mix_train_test'. When set to 1 the results are the same as setting 'best_model_in' to 'train'.
#' @param main_accuracy_metric A character value either: 'MAPE', 'MSE', 'MAE', 'RMSE' or MPE (it defaults to MAPE). This defines the criteria for selecting the best model (together with the 'best_model_in' parameter).
#' @param holidays A data-frame with columns holiday (character) and ds (date type)and optionally columns lower_window and upper_window which specify a range of days around the date to be included as holidays. lower_window=-2 will include 2 days prior to the date as holidays. Also optionally can have a column prior_scale specifying the prior scale for each holiday. It defaults to NULL in which case no holidays are used.
#' @param judgmental_forecasts A names vector with the date as name and the value of the judmental forecast. For example if we know that allways on the xmas day the value we are trying to predict is zero we can parse judgmental_forecasts = c('2016-12-25' = 1,  '2017-12-25' = 1, '2018-12-25' = 1). If the judgemental forecast is zero don't parse the value zero and parse 1 instead. This will facilitate with log transformations.
#' @param k_impute Integer width of the moving average window. Expands to both sides of the center element e.g. k=2 means 4 observations (2 left, 2 right) are taken into account. If all observations in the current window are NA, the window size is automatically increased until there are at least 2 non-NA values present (from ImputeTS package). Defaults to 4.
#' @param method_impute Weighting method to be used for imputing missing values or padded time-series. Accepts the following input: "simple" - Simple Moving Average (SMA), "linear" - Linear Weighted Moving Average (LWMA) or "exponential" - Exponential Weighted Moving Average (EWMA). Defaults to 'exponential'.
#' @param plotFrom A character value ('yyyy-mm-dd') representing a date to filter the data from to plot the best model based on the 'best_model_in' parameter (actuals vs forecast).
#' @param seed A seed.
#' @param parallel A Bool specify wheter to use parallelization whilst training the different models or not (defaults to FALSE). If TRUE, the number of cores is set to the total number of available cores minus 1 (parallel::detectCores()-1).
#' @param period_cv Used if best_model_in == 'cv'. Integer amount of time between cutoff dates. Same units as horizon. If not provided, 0.5 * horizon is used (for more details see documentation of prophet::cross_validation function and/or check Details section below).
#' @param initial_cv Used if best_model_in == 'cv'. Integer size of the first training period. If not provided, 3 * horizon is used. Same units as horizon (for more details see documentation of prophet::cross_validation function and/or check Details section below).
#' @param horizon_cv Used if best_model_in == 'cv'. Integer size of the horizon (for more details see documentation of prophet::cross_validation function and/or check Details section below).
#' @param debug TRUE for browsing the function. Defaults to FALSE.
#'
#'
#' @return This function returns a list with 3 elements:
#'  \itemize{
#'  \item{\strong{Accuracy_Overview}}  {A data-frame with all the trained models and accuracy metrics divided by train, test and cross-validated results (if available) The metrics are: MAPE', 'MSE', 'MAE', 'RMSE', 'MPE'}
#'  \item{\strong{Actuals_vs_Predictions_All}} {A data-frame with the actuals vs predictions and with upper and lower bound confidence intervals. This output also contains a column 'diff' with the Absolute Percentage Error for each prediction allowing the user to easily identify areas of miss-prediction.}
#'  \item{\strong{Actuals_vs_Predictions_Best}} {A data-frame with the predictions of the best model (based on 'main_accuracy_metric' and 'best_model_in' parameters). Actuals, predictions, upper and lower bound predictions are included as well as a 'prediction_and_predictor' field wich contains actuals from the train set and predictions from the test set (useful when stacked models are used).}
#'  \item{\strong{Best_Parameters}} {A data-frame with the best parameters selected by performance.}
#'  \item{\strong{Plot_Actual_Predictions}}  {A ggplot with actuals vs predictions and a separator for train/test split for the best model (based on 'best_model_in' and 'main_accuracy_metric' parameters).}
#' }
#'
#' @details Since this is a wrapper for Prophet, you can find extra parameters information on Prophet documentation \code{?prophet}.
#' @details When the parameter 'best_model_in' is set to 'cv' cross-validation (cv) will be performed. More precisely, due to the chronological nature of the data (time-series) the cv method used is 'evaluation on a rolling forecasting origin'. ProphetWrapper uses the prophet::cross_validation() function to compute this error metrics.
#' Three arguments can be selected for this section (horizon_cv, period_cv and initial_cv). horizon_cv controls for the length of each of the forecasts in the cross-validation process (defaults to the size of the testing set parsed to prophet_wrapper). The initial_cv controls for the length of the minimum data used for training on each fold (defaults to 3 * horizon_cv). period_cv controls for the time between cut-offs controlling if they overlap or not.
#' As an example assuming daily data, if we have 1673 observations finishing on the 2018-07-31 and we select horizon_cv equal to 91, period equal to 89 and initial as 1673 - 270, we end up with 3 folds of 90 days each sequentially.
#'
#' @importFrom("stats", "predict")
#' @importFrom("utils", "head")
#'
#' @examples
#' \dontrun{
#'
#' parameters = list(changepoint.prior.scale = c(0.2, 0.3),
#' regressor.prior.scale = c(0.01, 0.02),
#' weekly.seasonality = TRUE,
#' yearly.seasonality = TRUE,
#' standardize_regressor = TRUE,
#' log_transformation = TRUE,
#' target_variable = "y",
#' regressor1 = "x",
#' regressor2 = "z" )
#'
#'
#'test = Prophet_Wrapper(df = df_example,
#'                     list_params = parameters,
#'                     holidays = holidays,
#'                     best_model_in = "train",
#'                     main_accuracy_metric = "MAPE",
#'                     train_set_imp_perc = 0.5)
#'
#'}
#'
#' @import magrittr
#'
#' @export
#'



Prophet_Wrapper = function(df, list_params, holidays = NULL, best_model_in = "test", plotFrom = NULL, main_accuracy_metric = "MAPE", train_set_imp_perc = 0.5, judgmental_forecasts = NULL, k_impute = 4, method_impute = "exponential", parallel = FALSE, seed = 12345, period_cv = NULL, initial_cv = NULL, horizon_cv = NULL, debug = FALSE){


  #~~~ DEBUG =================================================================

  if(debug){browser()}


  is.date <- function(x) inherits(x, 'Date')

  #~~~ ERROR CONTROLS =================================================================

  if(is.null(df)){
    stop("No df provided ('df').")
  }

  if( !("data.frame" %in% class(df))){
    stop("The object parsed as 'df' is not a data.frame.")
  }

  if(ncol(df) < 2){
    stop('The df object has to include at least 3 columns (a target_var and a date variable).')
  }

  if(sum(sapply(df, is.numeric)) == 0){
    stop("No numeric/integer type column was identified in 'df'. Please include a numeric/integer and a date column to continue.")
  }

  if(sum(sapply(df, is.date)) != 1){
    stop("No Date type column was identified in 'df'. Please include a numeric and a date column to continue.")
  }

  tryCatch({df[,"train"]}, error = function(e){stop(paste0("The 'train' column is not in df."))})

  if(sum(unique(df$train) %in% c(1,0)) != 2){
    stop("The 'train' column has to include 1 or 0 values.")
  }

  #~~~ Clean the main data-frame (df) =================================================================

  vars = c(target_var = list_params$target_variable)

  original = df %>%
    dplyr::rename_if(is.date ,
                     dplyr::funs(sub(., 'Date', .))) %>%
    dplyr::rename(target_var = !!vars)


  if(!is.null(plotFrom)){
    if(as.Date(plotFrom) > max(original$Date) | as.Date(plotFrom) < min(original$Date)){
      stop("The argument 'plotFrom' date is out-of-bounds.")
    }
  }

  if(best_model_in != "train" & best_model_in != "test" & best_model_in != "mix_train_test" & best_model_in != "cv"){
    stop("The 'best_model_in' argument has to be either 'train', 'test', 'mix_train_test' or 'cv'.")
  }

  if(train_set_imp_perc > 1  & train_set_imp_perc <0 ){
    stop("The 'train_set_imp_perc' argument has to have a value between 0 and 1.")
  }

  if(main_accuracy_metric != "MAPE" & main_accuracy_metric != "MSE" & main_accuracy_metric != "MAE" & main_accuracy_metric != "RMSE" & main_accuracy_metric != "MPE"){
    stop("The 'main_accuracy_metric' argument has to be either MAPE, MSE, MAE, RMSE or MPE (it defaults to MAPE).")
  }

  if(sum(names(list_params) %in% c("weekly.seasonality", "yearly.seasonality", "standardize_regressor", "log_transformation", "target_variable", "regressor1", "regressor2" )) != 7){
    stop(paste0("The list_params argument has to include the following elements:, ", paste0(c("weekly.seasonality", "yearly.seasonality", "standardize_regressor", "log_transformation", "target_variable", "regressor1", "regressor2" ), collapse = ", "), ","))
  }

  length_reg1 = length(list_params$regressor1)
  tryCatch({df[,list_params$target_variable]}, error = function(e){stop(paste0("The Target Variable ", list_params$target_variable, " is not in df."))})


  for(i in 1:length_reg1){

    if(list_params$regressor1[i] != "no_regressor"){
      tryCatch({df[,list_params$regressor1[i]]}, error = function(e){stop(paste0("The Regressor ", list_params$regressor1[i], " is not in df."))})

    }else{next()}

  }

  length_reg2 = length(list_params$regressor2)

  for(i in 1:length_reg2){

    if(list_params$regressor2[i] != "no_regressor"){
      tryCatch({df[,list_params$regressor2[i]]}, error = function(e){stop(paste0("The Regressor ", list_params$regressor2[i], " is not in df."))})

    }else{next()}

  }


  if(list_params$weekly.seasonality != FALSE & list_params$weekly.seasonality != TRUE & !is.numeric(list_params$weekly.seasonality)){
    stop("The 'list_params$weekly.seasonality' argument has to be a bolean (TRUE or FALSE) or a number representing the Fourier terms to generate.")
  }

  if(list_params$yearly.seasonality != FALSE & list_params$yearly.seasonality != TRUE & !is.numeric(list_params$yearly.seasonality)){
    stop("The 'list_params$yearly.seasonality' argument has to be a bolean (TRUE or FALSE) or a number representing the Fourier terms to generate.")
  }

  if(list_params$standardize_regressor != FALSE & list_params$standardize_regressor != TRUE){
    stop("The 'list_params$standardize_regressor' argument has to be a bolean (TRUE or FALSE).")
  }

  if(list_params$log_transformation != FALSE & list_params$log_transformation != TRUE){
    stop("The 'list_params$log_transformation' argument has to be a bolean (TRUE or FALSE).")
  }

  if(class(list_params$changepoint.prior.scale) != "numeric" & !is.null(list_params$changepoint.prior.scale)){
    stop("The 'list_params$changepoint.prior.scale' argument has to be a numeric vector.")
  }

  if(class(list_params$regressor.prior.scale) != "numeric" & !is.null(list_params$regressor.prior.scale)){
    stop("The 'list_params$regressor.prior.scale' argument has to be a numeric vector.")
  }

  if(class(list_params$holidays.prior.scale) != "numeric" & !is.null(list_params$holidays.prior.scale)){
    stop("The 'list_params$holidays.prior.scale' argument has to be a numeric vector.")
  }

  if(sum(!is.null(judgmental_forecasts))> 0){

    cond = tryCatch(as.Date(names(judgmental_forecasts)),
             error = function(e){
               stop("The judgmental_forecasts parameter has to be a named character vector where the names can be tranformed to Dates. If necessary please check documentation for an example.")
             })
  }

  if(sum(!is.null(judgmental_forecasts))> 0 & !is.numeric(judgmental_forecasts)){
    stop("The judgmental_forecasts parameter has to have numeric values.  If necessary please check documentation for an example.")
  }

  #~~~ Defaulting Parameters =========================================================================

  if(is.null(list_params$changepoint.prior.scale)){list_params$changepoint.prior.scale = 0.05; cat("Defaulting changepoint.prior.scale to 0.05 ...\n\n")}

  if(is.null(list_params$regressor.prior.scale) & is.null(list_params$holidays.prior.scale)){list_params$regressor.prior.scale = 10; cat("Defaulting regressor.prior.scale to 10 ...\n\n")}

  if(is.null(list_params$regressor.prior.scale) & !is.null(list_params$holidays.prior.scale) & (sum(list_params$regressor2 != "no_regressor") <=1 | sum(list_params$regressor1 != "no_regressor") <=1)){list_params$regressor.prior.scale = list_params$holidays.prior.scale; cat("Defaulting regressor.prior.scale to holidays.prior.scale ...\n\n")}

  if(is.null(list_params$regressor.prior.scale) & is.null(list_params$holidays.prior.scale) & (sum(list_params$regressor2 != "no_regressor") <=1 | sum(list_params$regressor1 != "no_regressor") <=1)){list_params$regressor.prior.scale = 10; cat("Defaulting regressor.prior.scale to 10 ...\n\n")}

  if(is.null(list_params$holidays.prior.scale)){list_params$holidays.prior.scale = 10; cat("Defaulting holidays.prior.scale to 10 ... \n\n")}

  #cleaning the parameters to avoid duplication:

  list_params$changepoint.prior.scale = unique(list_params$changepoint.prior.scale)
  list_params$holidays.prior.scale = unique(list_params$holidays.prior.scale)
  list_params$regressor.prior.scale = unique(list_params$regressor.prior.scale)
  list_params$regressor1 = unique(list_params$regressor1) %>% as.character()
  list_params$regressor2 = unique(list_params$regressor2) %>% as.character()


  #~~~ Printing Informative Messeges =================================================================

  cat(paste0("We are testing Prophet models for ", length(list_params$changepoint.prior.scale), " values of changepoint.prior.scale and ", length(list_params$regressor.prior.scale), " of regressor.prior.scale, ", length(list_params$regressor1),   " regressors1 and ", length(list_params$regressor2), " regressors2,", length(list_params$holidays.prior.scale), " values of holidays.prior.scale. This is a total of ", length(list_params$regressor.prior.scale) * length(list_params$changepoint.prior.scale) * length(list_params$regressor1) * length(list_params$regressor2) * length(list_params$holidays.prior.scale), " models.\n\n"))
  #cat(paste0("If there are no surprises, it should take maximum of ", round(((length(list_params$regressor.prior.scale) * length(list_params$changepoint.prior.scale) * length(list_params$regressor1) * length(list_params$regressor2) * length(list_params$holidays.prior.scale)) * 10)/60, 2), " minutes to run ...\n\n"))


  #~~~ Create Train and Testing Set =================================================================

  if(list_params$log_transformation){
    log_transf = paste0("log(target_var)")
  }else{
    log_transf = paste0("target_var")
  }

  #training set:
  df_all = original %>%
    padr::pad() %>%
    tidyr::fill(train) %>%
    dplyr::mutate(
      target_var = ifelse(target_var == 0 & list_params$log_transformation, NA, target_var),
      ds = Date) %>%
    dplyr::mutate_all(dplyr::funs(imputeTS::na.ma(x = ., k = k_impute, weighting = method_impute))) %>%
    dplyr::mutate(y = eval(parse(text = log_transf)))

  #test set:
  df_test = df_all %>%
    dplyr::filter(train == 0)

  #test set:
  df_train = df_all %>%
    dplyr::filter(train == 1)

  #~~~ Create a list with predictions and accuracy data.frames for each combination of  changepoint.prior.scale and prior.scale. =================================================================

  grid = expand.grid(changepoint.prior.scale = list_params$changepoint.prior.scale,
                     regressor.prior.scale = list_params$regressor.prior.scale,
                     regressor1 = list_params$regressor1,
                     holidays.prior.scale = list_params$holidays.prior.scale,
                     regressor2 = list_params$regressor2)

  #Create the function we will use to apply to each combination of parameters/arguments of grid:
  create_predictions_and_outputs_fun = function(changepoint.prior.scale.param, regressor.prior.scale.param, regressor1.param, holidays.prior.scale.param, regressor2.param){

    if(debug){browser()}

    #####Models

    set.seed(seed = seed)

    models_output = modelling_prophet_function(df_all_modelling = df_all,
                                        df_test_modelling = df_test,
                                        df_train_modelling = df_train,
                                        modelling_type = "test",
                                        list_params_modelling = list_params,
                                        changepoint.prior.scale.modelling = changepoint.prior.scale.param,
                                        holidays.prior.scale.modelling = holidays.prior.scale.param,
                                        regressor.prior.scale.modelling = regressor.prior.scale.param,
                                        regressor1.modelling = regressor1.param,
                                        regressor2.modelling = regressor2.param,
                                        judgmental_forecasts.modelling = judgmental_forecasts,
                                        holidays_modelling = holidays,
                                        debug_modelling = debug)



    #Run Cross-Validated calculations if best_model_in == 'cv' is selected):
    if(best_model_in == "cv"){

      #Note: We are passing the variables to this function explicitly since apply functions enforce a strange environment structure:
      accuracies_cv_df = cv_wrapper(df = df_all,
                                    period.param = period_cv,
                                    horizon.param = horizon_cv,
                                    initial.param = initial_cv,
                                    df.test = df_test,
                                    debug = debug,
                                    model.param = models_output$model,
                                    list.params = list_params,
                                    judgmental.forecasts = judgmental_forecasts,
                                    regressor1_cv = regressor1.param,
                                    regressor2_cv = regressor2.param,
                                    changepoint.prior.scale_cv = changepoint.prior.scale.param,
                                    regressor.prior.scale_cv = regressor.prior.scale.param,
                                    holidays.prior.scale_cv = holidays.prior.scale.param,
                                    main.accuracy.metric = main_accuracy_metric)

    }


    #Creating the Accuracy Overview tables:

      test = data.frame( Error_Type = "Test Set",
                         regressor1 = regressor1.param,
                         regressor2 = regressor2.param,
                         changepoint.prior.scale = changepoint.prior.scale.param,
                         regressor.prior.scale = regressor.prior.scale.param,
                         holidays.prior.scale = holidays.prior.scale.param,
                         MAPE = MLmetrics::MAPE(y_pred = models_output$train_test_actuals_preds$y_pred_test, y_true = models_output$train_test_actuals_preds$y_true_test),
                         MSE =  MLmetrics::MSE(y_pred = models_output$train_test_actuals_preds$y_pred_test, y_true = models_output$train_test_actuals_preds$y_true_test),
                         MAE =  MLmetrics::MAE(y_pred = models_output$train_test_actuals_preds$y_pred_test, y_true = models_output$train_test_actuals_preds$y_true_test),
                         RMSE = MLmetrics::RMSE(y_pred = models_output$train_test_actuals_preds$y_pred_test, y_true = models_output$train_test_actuals_preds$y_true_test),
                         MPE = mean((models_output$train_test_actuals_preds$y_true_test - models_output$train_test_actuals_preds$y_pred_test)/models_output$train_test_actuals_preds$y_true_test),
                         stringsAsFactors = FALSE)

      train = data.frame( Error_Type = "Train Set",
                          regressor1 = regressor1.param,
                          regressor2 = regressor2.param,
                          changepoint.prior.scale = changepoint.prior.scale.param,
                          regressor.prior.scale = regressor.prior.scale.param,
                          holidays.prior.scale = holidays.prior.scale.param,
                          MAPE = MLmetrics::MAPE(y_pred = models_output$train_test_actuals_preds$y_pred_train, y_true = models_output$train_test_actuals_preds$y_true_train),
                          MSE = MLmetrics::MSE(y_pred = models_output$train_test_actuals_preds$y_pred_train, y_true = models_output$train_test_actuals_preds$y_true_train),
                          MAE = MLmetrics::MAE(y_pred = models_output$train_test_actuals_preds$y_pred_train, y_true = models_output$train_test_actuals_preds$y_true_train),
                          RMSE = MLmetrics::RMSE(y_pred = models_output$train_test_actuals_preds$y_pred_train, y_true = models_output$train_test_actuals_preds$y_true_train),
                          MPE = mean((models_output$train_test_actuals_preds$y_true_train - models_output$train_test_actuals_preds$y_pred_train)/models_output$train_test_actuals_preds$y_true_train),
                          stringsAsFactors = FALSE)

    #Add to the accuracy results the CV results:
    df_accuracy = if(best_model_in == 'cv'){
      dplyr::bind_rows(test, train, accuracies_cv_df$accuracies_cv)
    }else{
      dplyr::bind_rows(test, train)
    }


    #Return final results:
    final_list = list(accuracy_overview = df_accuracy,
                      actuals_vs_forecast = models_output$forecasts_all,
                      accuracy_cv = if(best_model_in == 'cv'){accuracies_cv_df$accuracies_cv}else{list()},
                      overview_cv = if(best_model_in == 'cv'){accuracies_cv_df$overview_cv_results}else{list()},
                      graph_cv = if(best_model_in == 'cv'){accuracies_cv_df[[3]]}else{list()}) %>%
      purrr::set_names("accuracy_overview", "actuals_vs_forecast", "accuracy_cv", "overview_cv", paste0("graph_cv", regressor1.param, regressor2.param, changepoint.prior.scale.param, regressor.prior.scale.param, holidays.prior.scale.param))



    return(final_list)

  }

  if(!parallel){

    final = mapply(create_predictions_and_outputs_fun,
                   SIMPLIFY = F,
                   changepoint.prior.scale.param = grid$changepoint.prior.scale,
                   regressor.prior.scale.param = grid$regressor.prior.scale,
                   regressor1.param = grid$regressor1,
                   holidays.prior.scale.param = grid$holidays.prior.scale,
                   regressor2.param = grid$regressor2)

  }else{

    cat("Running the models in parallel (no progress bar) ... \n")

    final = parallel::mcmapply(create_predictions_and_outputs_fun,
                               SIMPLIFY = F,
                               changepoint.prior.scale.param = grid$changepoint.prior.scale,
                               regressor.prior.scale.param = grid$regressor.prior.scale,
                               regressor1.param = grid$regressor1,
                               holidays.prior.scale.param = grid$holidays.prior.scale,
                               regressor2.param = grid$regressor2,
                               mc.cores = parallel::detectCores()-1)

  }


  #~~~ Prepare the Output by reducing the lists and creating a graph of actuals vs forecasts for the best model in terms of 'best_model_in' =================================================================


  final = unlist(final, recursive = FALSE)

  final_accuracy = final[names(final) == "accuracy_overview"]
  final_predictions_actuals = final[names(final) == "actuals_vs_forecast"]
  final_accuracy_cv = final[names(final) == "accuracy_cv"]
  final_overview_cv = final[names(final) == "overview_cv"]

  if(is.null(plotFrom)){
    plotFrom = min(df_train$ds)
  }else{
    plotFrom = as.Date(plotFrom)
  }

  #identify the best model based on best_model_in parameter:

  if(best_model_in == "train" | best_model_in == "test"){
    accuracies = invisible(final_accuracy %>%
                             dplyr::bind_rows() %>%
                             dplyr::filter(grepl(pattern = best_model_in, x = Error_Type, ignore.case = TRUE)) %>%
                             dplyr::arrange(!!rlang::sym(main_accuracy_metric)) %>%
                             dplyr::select(changepoint.prior.scale,
                                           regressor.prior.scale,
                                           regressor1,
                                           regressor2,
                                           holidays.prior.scale) %>%
                             head(1))

  }else if(best_model_in == "cv"){

    accuracies = invisible(final_accuracy_cv %>%
                             dplyr::bind_rows() %>%
                             dplyr::arrange(!!rlang::sym(main_accuracy_metric)) %>%
                             dplyr::select(changepoint.prior.scale,
                                           regressor.prior.scale,
                                           regressor1,
                                           regressor2,
                                           holidays.prior.scale) %>%
                             head(1))


  }else{

    accuracies = final_accuracy %>%
      dplyr::bind_rows() %>%
      dplyr::filter(grepl(pattern = "test|train", x = Error_Type, ignore.case = TRUE)) %>%
      dplyr::select(Error_Type, changepoint.prior.scale, regressor.prior.scale, regressor1, regressor2, holidays.prior.scale, !!main_accuracy_metric) %>%
      tidyr::spread(key = Error_Type, !!main_accuracy_metric) %>%
      dplyr::mutate(avg_accuracy_metric = (`Train Set` * train_set_imp_perc  + `Test Set` * (1- train_set_imp_perc)  )) %>%
      dplyr::arrange(avg_accuracy_metric) %>%
      dplyr::select(changepoint.prior.scale, regressor.prior.scale, regressor1, regressor2, holidays.prior.scale) %>%
      head(1)

  }

  accuracies_graph = final_accuracy %>%
    dplyr::bind_rows() %>%
    dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale, regressor.prior.scale == accuracies$regressor.prior.scale, regressor1 == accuracies$regressor1, regressor2 == accuracies$regressor2, holidays.prior.scale == accuracies$holidays.prior.scale)

  #Best model predictions:
  df_best_model = final_predictions_actuals %>%
                   dplyr::bind_rows() %>%
                   dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale, regressor.prior.scale == accuracies$regressor.prior.scale, regressor1 == accuracies$regressor1, regressor2 == accuracies$regressor2, holidays.prior.scale == accuracies$holidays.prior.scale) %>%
                   dplyr::mutate(prediction_and_predictor = ifelse(train == 1, actuals, yhat)) %>%
                   dplyr::select(Date, actuals, yhat, yhat_lower, yhat_upper, prediction_and_predictor, train)

  #Generate a ggplot graph from the best model:
  df_graph = df_best_model %>%
    dplyr::filter(as.Date(Date) >= as.Date(plotFrom)) %>%
    tidyr::gather(`Actuals vs Forecast`, Volumes, actuals:yhat)

  graph1 = ggplot2::ggplot(data = df_graph, ggplot2::aes(x = as.Date(Date), y = Volumes, color = `Actuals vs Forecast`, linetype = `Actuals vs Forecast`)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = as.numeric(max(df_train$ds))), linetype = 4, colour = "#40dfad", alpha = 0.7) +
    ggthemes::theme_tufte() +
    ggplot2::scale_y_continuous("\nActuals/Forecasts\n", labels = scales::comma_format()) +
    ggplot2::scale_x_date(name = "\nDate", breaks = scales::date_breaks("2 months")) +
    ggthemes::scale_color_tableau(palette = 'tableau10medium') +
    ggtitle(label = "Actuals vs Forecasts", subtitle = paste0("From: ", min(df_graph$Date), " To: ", max(df_graph$Date), " (test set from ", max(df_train$ds), " onwards)")) +
    theme(legend.position = "bottom")


  graph2 = invisible(gridExtra::tableGrob(accuracies_graph, rows = NULL))
  graph_final = invisible(gridExtra::arrangeGrob(graph2, graph1, nrow = 2, heights = c(0.3, 2)))



  #~~ FINAL OUTPUT: In this section we prepare the final outputs to be exported on the final list object:

  final_overview_cv = final_overview_cv %>%
    dplyr::bind_rows() %>%
    dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale & regressor.prior.scale == accuracies$regressor.prior.scale & regressor1 == accuracies$regressor1 & regressor2 == accuracies$regressor2 & holidays.prior.scale == accuracies$holidays.prior.scale)

  final_accuracy = final_accuracy %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(best_model = ifelse(changepoint.prior.scale == accuracies$changepoint.prior.scale & regressor.prior.scale == accuracies$regressor.prior.scale & regressor1 == accuracies$regressor1 & regressor2 == accuracies$regressor2 & holidays.prior.scale == accuracies$holidays.prior.scale, 1, 0))


  final_graph_cv = final[names(final) == paste0("graph_cv", accuracies$regressor1, accuracies$regressor2, accuracies$changepoint.prior.scale, accuracies$regressor.prior.scale, accuracies$holidays.prior.scale)]

  #Retraining a final model with the best parameters on full data:
  models_output = modelling_prophet_function(df_all_modelling = df_all,
                                             df_test_modelling = df_test,
                                             df_train_modelling = df_train,
                                             modelling_type = "final",
                                             list_params_modelling = list_params,
                                             changepoint.prior.scale.modelling = accuracies$changepoint.prior.scale,
                                             holidays.prior.scale.modelling = accuracies$holidays.prior.scale,
                                             regressor.prior.scale.modelling = accuracies$regressor.prior.scale,
                                             regressor1.modelling = accuracies$regressor1,
                                             regressor2.modelling = accuracies$regressor2,
                                             judgmental_forecasts.modelling = judgmental_forecasts,
                                             holidays_modelling = holidays,
                                             debug_modelling = debug)

  final_results = list(
                       Final_Forecasts = models_output$forecasts_all,
                       Accuracy_Overview = final_accuracy,
                       Actuals_vs_Predictions_All = final_predictions_actuals %>% dplyr::bind_rows(),
                       Actual_vs_Predictions_Best = df_best_model,
                       Best_Parameters = accuracies,
                       CV_Overview = if(best_model_in == 'cv'){final_overview_cv}else{NULL},
                       Plot_CV_Accuracy =  if(best_model_in == 'cv'){final_graph_cv}else{NULL},
                       Plot_Actual_Predictions = graph1
                       )

  #Return all non-null elements:

  cat(paste0("\n\nThe ", nrow(grid), " models were trained and the accuracy (", main_accuracy_metric,") was estimated for the test set between ", min(df_test$ds), " to ", max(df_test$ds), ".\n"))
  cat(paste0("To analyse the accuracy distributions use access 'Accuracy_Overview'. For a view of actuals vs forecasts, confidence intervals and point forecast error metrics access 'Actuals_vs_Predictions (All or Best)'. For a plot of Actual vs Forecasts of the best model use 'Plot_Actual_Predictions'."))

  return(final_results[!sapply(final_results, is.null)] )


  }
