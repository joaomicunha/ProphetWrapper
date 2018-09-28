


#' Prophet Wrapper
#'
#' This is a function that wraps up (\href{https://facebook.github.io/prophet/docs/installation.html}{Prophet}) package functionality, expanding the possibilities currently offered by the Facebook developed R package.
#' The main rationale behind the package was to build a reproducible function to model and test several models simultaneously. The package currently offers grid.search functionality for parameter tuning, results/accuracy visualisations and cross-validation.
#'
#'
#' @param df A data-frame with a numeric column, a date type column, a regressor and a column train (for train/test split). The 'train' column has to have 1 and 0 only where 1 refer to training set.
#' @param list_params A list with the following parameters:
#'  \itemize{
#'  \item{\strong{target_variable}}  {The name of the target variable to predict. Has to be included in df.}
#'  \item{\strong{changepoint.prior.scale}}  {A regularization parameter (vector or single value) for the automatic changepoint definitions of Prophet to define piecewise trend blocks. If the trend changes are being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default Prophet sets this parameter to 0.05. Increasing it will make the trend more flexible.}
#'  \item{\strong{regressor.prior.scale}}  {A regularization parameter (vector or single value) for the external regressor. If the regressor is being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default Prophet sets this parameter to 0.05. Increasing it will make the effect of regressor more flexible. This parameter is applied to both regressor1 and regressor2 (if parsed).}
#'  \item{\strong{holidays.prior.scale}}   {A regularization parameter (vector or single value) for the holidays effects. If the regressor is being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default this parameter is 10, which provides very little regularization. Reducing this parameter dampens holiday effects. Increasing it will make the holidays effect more flexible.}
#'  \item{\strong{seasonality.prior.scale}}   {A regularization parameter (vector or single value) for the seasonality effects. If the regressor is being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default this parameter is 10, which provides very little regularization. Reducing this parameter dampens seasonality effects. Increasing it will make the holidays effect more flexible.}
#'  \item{\strong{weekly.seasonality}}  {Fit weekly seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{yearly.seasonality}}  {Fit yearly seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{daily.seasonality}}  {Fit daily seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{regressor1}}  {The name of the first external regressor (or regressors) to include in the model. It has to exist on df as a column. If a vector is parsed, 1 regressor at a time is tested (as a model parameter essentially). If "no_regressor" is parsed, a univariate time-series model is estimated. "no_regressor" can be parsed as an element of the vector as well. }
#'  \item{\strong{regressor2}}  {The name of the second external regressor (or regressors) to include in the model. It has to exist on df as a column. If a vector is parsed, 1 regressor at a time is tested (in combination with regressor1 if parsed). If "no_regressor" is parsed, a univariate time-series model is estimated. "no_regressor" can be parsed as an element of the vector as well. }
#'  \item{\strong{standardize_regressor}}  {Specify whether this regressor will be standardized prior to fitting. Can be 'auto' (standardize if not binary), True, or False.}
#'  \item{\strong{log_transformation}}  {Bool, specify whether the Target Variable will be log transformed pre-fitting the models or not.}
#' }
#' @param best_model_in A character value either: 'train', 'test', 'mix_train_test' or 'cv'. Defaults to 'test'. This parameter defines the criteria to pick the best model - either based on accuracy on training set or in test set. The user might have different business requirements and wants to understand the general performance of the model cross-validated over a set of periods ('cv').
#' @param train_set_imp_perc A numeric input between 0 and 1 representing the weight to give to the training set results relatively to the test set. Defaults to 0.5 (mean results). This parameter only affects the results when 'best_model_in' is set to 'mix_train_test'. When set to 1 the results are the same as setting 'best_model_in' to 'train'.
#' @param main_accuracy_metric A character value either: 'MAPE', 'MSE', 'MAE', 'RMSE' or MPE (it defaults to MAPE). This defines the criteria for selecting the best model (together with the 'best_model_in' parameter).
#' @param holidays A data-frame with columns holiday (character) and ds (date type)and optionally columns lower_window and upper_window which specify a range of days around the date to be included as holidays. lower_window=-2 will include 2 days prior to the date as holidays. Also optionally can have a column prior_scale specifying the prior scale for each holiday. It defaults to NULL in which case no holidays are used.
#' @param judgmental_forecasts A names vector with the date as name and the value of the judmental forecast. For example if we know that allways on the xmas day the value we are trying to predict is zero we can parse judgmental_forecasts = c('2016-12-25' = 1,  '2017-12-25' = 1, '2018-12-25' = 1). If the judgemental forecast is zero don't parse the value zero and parse 1 instead. This will facilitate with log transformations.
#' @param k_impute Integer width of the moving average window. Expands to both sides of the center element e.g. k=2 means 4 observations (2 left, 2 right) are taken into account. If all observations in the current window are NA, the window size is automatically increased until there are at least 2 non-NA values present (from ImputeTS package). Defaults to 2.
#' @param method_impute Weighting method to be used for imputing missing values or padded time-series. Accepts the following input: "simple" - Simple Moving Average (SMA), "linear" - Linear Weighted Moving Average (LWMA) or "exponential" - Exponential Weighted Moving Average (EWMA). Defaults to 'exponential'.
#' @param plotFrom A character value ('yyyy-mm-dd') representing a date to filter the data from to plot the best model based on the 'best_model_in' parameter (actuals vs forecast).
#' @param seed A seed.
#' @param parallel A Bool specify wheter to use parallelization whilst training the different models or not (defaults to FALSE). If TRUE, the number of cores is set to the total number of available cores minus 1 (parallel::detectCores()-1).
#' @param period_cv Used if best_model_in == 'cv'. Integer amount of time between cutoff dates. Same units as horizon. If not provided, 0.5 * horizon is used (for more details see documentation of prophet::cross_validation function and/or check Details section below).
#' @param initial_cv Used if best_model_in == 'cv'. Integer size of the first training period. If not provided, 3 * horizon is used. Same units as horizon (for more details see documentation of prophet::cross_validation function and/or check Details section below).
#' @param horizon_cv Used if best_model_in == 'cv'. Integer size of the horizon (for more details see documentation of prophet::cross_validation function and/or check Details section below).
#' @param final_predictions final_predictions is the argument that will control the length/horizon of the final forecasts (the forecast generated from the optimised model trained on all available data). The values of final_predictions argument can be:
#'  \itemize{
#'  \item{\strong{integer}}  {If final_predictions is set to an integer, the final forecast horizon will have length final_predictions. Please note that if the final forecast uses regressors, these have to be parsed here as a data.frame (see below).}
#'  \item{\strong{data.frame}} {A data.frame with columns 'Date', 'regressor1' and 'regressor2' with the future values of the regressors. If regressors are not used then an integer value can be used.}
#'  \item{\strong{ProphetWrapper Object}} {The user can parse directly a ProphetWrapper object (output of Prophet_Wrapper function). This reveals to be useful when using 'stacked' modelling. The regressors used are the Final_Forecasts from the ProphetWrapper object parsed and the length/horizon of the final forecasts is the number of future rows in Final_Forecasts.}
#'  \item{\strong{NULL (default)}} {If set to NULL, final_predictions is set to the length of the test set (assumes the test set is representative of the future horizon.)}
#' }
#' @param debug TRUE for browsing the function. Defaults to FALSE.
#'
#'
#' @return This function returns a list of class 'ProphetWrapper' with the following elements:
#'  \itemize{
#' \item{\strong{Final_Forecasts:}} {A data-frame with final forecasts on unseen data produced by training the optimised model (based on 'main_accuracy_metric' and 'best_model_in' parameters) on the entire data. The user can use the 'final_predictions' parameter to control the horizon of the final forecasts (more detail on the function documentation). ProphetWrapper class object can be parsed directly to 'final_predictions' which reveals to be useful when using 'stacked' modelling. The regressors used are the Final_Forecasts from the ProphetWrapper object parsed and the length/horizon of the final forecasts is the number of future rows in Final_Forecasts. The forecast length (horizon) can also be set to be equal to the length of the test set (when 'final_predictions' is NULL) or to a specified integer.}
#' \item{\strong{Accuracy_Overview:}} {A data-frame with the performance of all the models estimated with a flag for the best model (based on 'main_accuracy_metric' and 'best_model_in' parameters). The error metrics available are: MAPE, MSE, MAE, RMSE and MPE.}
#' \item{\strong{Actuals_vs_Predictions_All:}} {A data-frame with point predictions vs actuals, upper and lower bound predictions as per Prophet and other details about the series and the models (for train and test set). There is a row per date and model trained pair (i.e. if n models are trained this data-frame returns n rows for each of the date points).}
#' \item{\strong{Actual_vs_Predictions_Best:}} {A data-frame with the predictions of the best model (selected based on 'main_accuracy_metric' and 'best_model_in' parameters). Actuals, predictions, upper and lower bound predictions are included as well as a 'prediction_and_predictor' field wich contains actuals from the train set and predictions from the test set (useful when stacked models are used).}
#' \item{\strong{Best_Parameters:}} {A data-frame with the best parameters used to estimate the best model (selected based on 'main_accuracy_metric' and 'best_model_in' parameters).}
#' \item{\strong{CV_Overview:}} {A data-frame with an overview of the in-fold accuracy performance for the best model (selected based on 'main_accuracy_metric' and 'best_model_in' parameters). This output is only made available if best_model_in parameter is set to 'cv'.}
#' \item{\strong{Plot_CV_Accuracy:}} {A ggplot graph illustrating the performance on cross-validation for different horizon (look-ahead) periods. This output is only made available if best_model_in parameter is set to 'cv'.}
#' \item{\strong{Plot_Actual_Predictions:}} {A ggplot graph of Actuals vs Predictions. The 'plotFrom' parameter controls from when to plot from.}
#' }
#'
#' @details Since this is a wrapper for Prophet, you can find extra parameters information on Prophet documentation \code{?prophet}.
#' @details When the parameter 'best_model_in' is set to 'cv' cross-validation (cv) will be performed. More precisely, due to the chronological nature of the data (time-series) the cv method used is 'evaluation on a rolling forecasting origin'. ProphetWrapper uses the prophet::cross_validation() function to compute this error metrics.
#' Three arguments can be selected for this section (horizon_cv, period_cv and initial_cv). horizon_cv controls for the length of each of the forecasts in the cross-validation process (defaults to the size of the testing set parsed to prophet_wrapper). The initial_cv controls for the length of the minimum data used for training on each fold (defaults to 3 * horizon_cv). period_cv controls for the time between cut-offs controlling if they overlap or not.
#' As an example assuming daily data, if we have 1673 observations finishing on the 2018-07-31 and we select horizon_cv equal to 91, period equal to 89 and initial as 1673 - 270, we end up with 3 folds of 90 days each sequentially.
#'
#' @importFrom stats predict
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#'
#' parameters = list(changepoint.prior.scale = c(0.2, 0.3),
#' regressor.prior.scale = c(0.01, 0.02),
#' weekly.seasonality = TRUE,
#' yearly.seasonality = TRUE,
#' daily.seasonality = TRUE,
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
#'                     train_set_imp_perc = 0.5,
#'                     final_predictions = list(forecasts_inbound_all, forecasts_offered_all)
#'
#'}
#'
#' @import magrittr
#'
#' @export
#'



Prophet_Wrapper = function(df, list_params, holidays = NULL, best_model_in = "test", plotFrom = NULL, main_accuracy_metric = "MAPE", train_set_imp_perc = 0.5, judgmental_forecasts = NULL, k_impute = 2, method_impute = "exponential", parallel = FALSE, seed = 12345, period_cv = NULL, initial_cv = NULL, horizon_cv = NULL, final_predictions = NULL, debug = FALSE){


  #~~~ DEBUG ================================================================

  if(debug){browser()}

  initial_time = Sys.time()


  is.date <- function(x) inherits(x, 'Date')

  #~~~ ERROR CONTROLS =======================================================

  original = ValidateArguments(df = df,
                               list_params = list_params,
                               best_model_in = best_model_in,
                               plotFrom = plotFrom,
                               main_accuracy_metric = main_accuracy_metric,
                               train_set_imp_perc = train_set_imp_perc,
                               judgmental_forecasts = judgmental_forecasts,
                               final_predictions = final_predictions,
                               debug = debug)

  #~~~ Defaulting Parameters ================================================

  cat(paste0("*** Forecasting the target variable: ", list_params$target_variable, " ***\n"))
  cat(paste0("* Test Period from ", min(original$Date[original$train == 0]), " to ",  max(original$Date[original$train == 0]), " *\n\n"))

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

  #train set:
  df_train = df_all %>%
    dplyr::filter(train == 1)

  #~~~ Create a list with predictions and accuracy data.frames for each combination of  changepoint.prior.scale and prior.scale. =================================================================

  grid = expand.grid(changepoint.prior.scale = list_params$changepoint.prior.scale,
                     seasonality.prior.scale = list_params$seasonality.prior.scale,
                     regressor.prior.scale = list_params$regressor.prior.scale,
                     regressor1 = list_params$regressor1,
                     holidays.prior.scale = list_params$holidays.prior.scale,
                     regressor2 = list_params$regressor2,
                     stringsAsFactors = F)

  #~~~ Printing Informative Messeges =================================================================
  cat(paste0("We are testing Prophet models for ", length(list_params$changepoint.prior.scale), " values of changepoint.prior.scale, ",length(list_params$seasonality.prior.scale), " values of seasonality.prior.scale, ", length(list_params$regressor.prior.scale), " of regressor.prior.scale, ", length(list_params$regressor1),   " of regressors1, ", length(list_params$regressor2), " of regressors2,", length(list_params$holidays.prior.scale), " values of holidays.prior.scale. This is a total of ", nrow(grid), " models.\n\n"))

  #Create the function we will use to apply to each combination of parameters/arguments of grid:
  create_predictions_and_outputs_fun = function(changepoint.prior.scale.param, regressor.prior.scale.param, regressor1.param, holidays.prior.scale.param, regressor2.param, seasonality.prior.scale.param){

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
                                        seasonality.prior.scale.modelling = seasonality.prior.scale.param,
                                        regressor.prior.scale.modelling = regressor.prior.scale.param,
                                        regressor1.modelling = regressor1.param,
                                        regressor2.modelling = regressor2.param,
                                        judgmental_forecasts.modelling = judgmental_forecasts,
                                        holidays_modelling = holidays,
                                        final_predictions_modelling = final_predictions,
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
                                    seasonality.prior.scale_cv = seasonality.prior.scale.param,
                                    regressor.prior.scale_cv = regressor.prior.scale.param,
                                    holidays.prior.scale_cv = holidays.prior.scale.param,
                                    main.accuracy.metric = main_accuracy_metric)

    }


    #Creating the Accuracy Overview tables:

      test = data.frame( Error_Type = "Test Set",
                         regressor1 = regressor1.param,
                         regressor2 = regressor2.param,
                         changepoint.prior.scale = changepoint.prior.scale.param,
                         seasonality.prior.scale = seasonality.prior.scale.param,
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
                          seasonality.prior.scale = seasonality.prior.scale.param,
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
      purrr::set_names("accuracy_overview", "actuals_vs_forecast", "accuracy_cv", "overview_cv", paste0("graph_cv", regressor1.param, regressor2.param, changepoint.prior.scale.param, seasonality.prior.scale.param, regressor.prior.scale.param, holidays.prior.scale.param))



    return(final_list)

  }

  #~~~ Running the mapply operation over the grid (parallel or not):

  if(!parallel){

    final = mapply(create_predictions_and_outputs_fun,
                   SIMPLIFY = F,
                   changepoint.prior.scale.param = grid$changepoint.prior.scale,
                   seasonality.prior.scale.param = grid$seasonality.prior.scale,
                   regressor.prior.scale.param = grid$regressor.prior.scale,
                   regressor1.param = grid$regressor1,
                   holidays.prior.scale.param = grid$holidays.prior.scale,
                   regressor2.param = grid$regressor2)

  }else{

    cat("Running the models in parallel (no progress bar) ... \n")

    final = parallel::mcmapply(create_predictions_and_outputs_fun,
                               SIMPLIFY = F,
                               changepoint.prior.scale.param = grid$changepoint.prior.scale,
                               seasonality.prior.scale.param = grid$seasonality.prior.scale,
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
                                           seasonality.prior.scale,
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
                                           seasonality.prior.scale,
                                           regressor.prior.scale,
                                           regressor1,
                                           regressor2,
                                           holidays.prior.scale) %>%
                             head(1))


  }else{

    accuracies = final_accuracy %>%
      dplyr::bind_rows() %>%
      dplyr::filter(grepl(pattern = "test|train", x = Error_Type, ignore.case = TRUE)) %>%
      dplyr::select(Error_Type, changepoint.prior.scale, seasonality.prior.scale, regressor.prior.scale, regressor1, regressor2, holidays.prior.scale, !!main_accuracy_metric) %>%
      tidyr::spread(key = Error_Type, !!main_accuracy_metric) %>%
      dplyr::mutate(avg_accuracy_metric = (`Train Set` * train_set_imp_perc  + `Test Set` * (1- train_set_imp_perc)  )) %>%
      dplyr::arrange(avg_accuracy_metric) %>%
      dplyr::select(changepoint.prior.scale, seasonality.prior.scale, regressor.prior.scale, regressor1, regressor2, holidays.prior.scale) %>%
      head(1)

  }

  accuracies_graph = final_accuracy %>%
    dplyr::bind_rows() %>%
    dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale, seasonality.prior.scale == accuracies$seasonality.prior.scale, regressor.prior.scale == accuracies$regressor.prior.scale, regressor1 == accuracies$regressor1, regressor2 == accuracies$regressor2, holidays.prior.scale == accuracies$holidays.prior.scale)

  #Best model predictions:
  df_best_model = final_predictions_actuals %>%
                   dplyr::bind_rows() %>%
                   dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale, seasonality.prior.scale == accuracies$seasonality.prior.scale, regressor.prior.scale == accuracies$regressor.prior.scale, regressor1 == accuracies$regressor1, regressor2 == accuracies$regressor2, holidays.prior.scale == accuracies$holidays.prior.scale) %>%
                   dplyr::mutate(prediction_and_predictor = ifelse(train == 1, actuals, yhat)) %>%
                   dplyr::select(Date, actuals, yhat, yhat_lower, yhat_upper, prediction_and_predictor, train)


  #~~ FINAL OUTPUT: In this section we prepare the final outputs to be exported on the final list object:

  final_overview_cv = final_overview_cv %>%
    dplyr::bind_rows() %>%
    dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale & seasonality.prior.scale == accuracies$seasonality.prior.scale & regressor.prior.scale == accuracies$regressor.prior.scale & regressor1 == accuracies$regressor1 & regressor2 == accuracies$regressor2 & holidays.prior.scale == accuracies$holidays.prior.scale)

  final_accuracy = final_accuracy %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(best_model = ifelse(changepoint.prior.scale == accuracies$changepoint.prior.scale & seasonality.prior.scale == accuracies$seasonality.prior.scale & regressor.prior.scale == accuracies$regressor.prior.scale & regressor1 == accuracies$regressor1 & regressor2 == accuracies$regressor2 & holidays.prior.scale == accuracies$holidays.prior.scale, 1, 0))

  final_predictions_actuals = final_predictions_actuals %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(best_model = ifelse(changepoint.prior.scale == accuracies$changepoint.prior.scale & seasonality.prior.scale == accuracies$seasonality.prior.scale & regressor.prior.scale == accuracies$regressor.prior.scale & regressor1 == accuracies$regressor1 & regressor2 == accuracies$regressor2 & holidays.prior.scale == accuracies$holidays.prior.scale, 1, 0))


  final_graph_cv = final[names(final) == paste0("graph_cv", accuracies$regressor1, accuracies$regressor2, accuracies$changepoint.prior.scale, accuracies$seasonality.prior.scale, accuracies$regressor.prior.scale, accuracies$holidays.prior.scale)]

  #Retraining a final model with the best parameters on full data:
  #Currently the final forecasts are only produced for cases where there is no regressors picked in the last model. This is because there is not regressors values for the future:


  if((is.null(final_predictions) | is.integer(final_predictions) | is.numeric(final_predictions)) & (accuracies$regressor1 != "no_regressor" | accuracies$regressor2 != "no_regressor"  )){

    warning("\nNot possible to run the final optimised model on all available data since the future values of the regressors were not made available \n")
    models_output = list(forecasts_all = NULL)

  }else if(length(list_params$regressor1) >1 | length(list_params$regressor2) >1){

    warning("\nNot possible to run the final optimised model on all available data since the optimal regressors were estimated within ProphetWrapper and therefore can't be parsed in advance \n")
    models_output = list(forecasts_all = NULL,
                         plot_components = NULL,
                         plot_changepoints = NULL,
                         model = NULL)

  }else{

    models_output = modelling_prophet_function(df_all_modelling = df_all,
                                               df_test_modelling = df_test,
                                               df_train_modelling = df_train,
                                               modelling_type = "final",
                                               list_params_modelling = list_params,
                                               changepoint.prior.scale.modelling = accuracies$changepoint.prior.scale,
                                               seasonality.prior.scale.modelling = accuracies$seasonality.prior.scale,
                                               holidays.prior.scale.modelling = accuracies$holidays.prior.scale,
                                               regressor.prior.scale.modelling = accuracies$regressor.prior.scale,
                                               regressor1.modelling = accuracies$regressor1,
                                               regressor2.modelling = accuracies$regressor2,
                                               judgmental_forecasts.modelling = judgmental_forecasts,
                                               holidays_modelling = holidays,
                                               final_predictions_modelling = final_predictions,
                                               debug_modelling = debug)

  }


  #~~~~~ Generate visualisations/Graphs. Collect graphs and visualisations from several functions and store them in a list:

 plots_generate_viz =  GenerateViz( df_best_model_viz = df_best_model,
                                    Final_Forecasts_viz = models_output$forecasts_all,
                                    list_params_viz = list_params,
                                    plotFrom_viz = plotFrom,
                                    debug_viz = debug)

  #Plot Components has to be rendered in a different way because returns a list with graphical elements:
  plot_components_expr = c()

  for(i in 1:length(forecasts_inbound_all$Plots$Plot_Final_Model_Components)){


     plot_components_expr[i] = paste0("models_output$plot_components[[", i, "]]")

   }


  eval(parse(text = paste0("final_grid_components = gridExtra::arrangeGrob(", paste(expr, collapse = ", "), ", ncol = 1)")))

  final_plot_components = ggplotify::as.ggplot(final_grid_components)

 #All Graphs:
 plots_generate_viz[["Plot_Final_Model_Components"]] = final_plot_components
 plots_generate_viz[["Plot_Final_Model_Changepoints"]] = models_output$plot_changepoints
 plots_generate_viz[["Plot_CV_Accuracy"]] = if(best_model_in == 'cv'){final_graph_cv}else{NULL}

  #~~~~~ Define the final list to export:

  final_results = list(
                       Final_Forecasts = models_output$forecasts_all,
                       Accuracy_Overview = final_accuracy,
                       Actuals_vs_Predictions_All = final_predictions_actuals,
                       Actuals_vs_Predictions_Best = df_best_model,
                       Best_Parameters = accuracies,
                       CV_Overview = if(best_model_in == 'cv'){final_overview_cv}else{NULL},
                       Final_Prophet_Model = models_output$model,
                       Plots = plots_generate_viz[!sapply(plots_generate_viz, is.null)]
                       )

  #Return all non-null elements:

  cat(paste0("\n\nThe ", nrow(grid), " models were trained and the accuracy (", main_accuracy_metric,") was estimated for the test set between ", min(df_test$ds), " to ", max(df_test$ds), ". It took ", round(difftime(time1 = Sys.time(), time2 = initial_time, units = "mins" )), " minutes to run.\n"))
  cat(paste0("To analyse the accuracy distributions use access 'Accuracy_Overview'. For a view of actuals vs forecasts, confidence intervals and point forecast error metrics access for all models trained 'Actuals_vs_Predictions_All'. For a plot of Actual vs Forecasts of the best model use 'Plot_Actual_Predictions'. 'Final_Forecasts' will include a final forecast with the optimised model.\n"))

  return(structure(.Data = final_results[!sapply(final_results, is.null)], class = "ProphetWrapper") )

  }
