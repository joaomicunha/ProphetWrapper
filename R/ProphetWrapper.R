


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
#'  \item{\strong{regressor.prior.scale}}  {A regularization parameter (vector or single value) for the external regressor. If the regressor is being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default Prophet sets this parameter to 0.05. Increasing it will make the effect of regressor more flexible more flexible.}
#'  \item{\strong{holidays.prior.scale}}   {A regularization parameter (vector or single value) for the holidays effects. If the regressor is being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default this parameter is 10, which provides very little regularization. Reducing this parameter dampens holiday effects. Increasing it will make the trend more flexible.}
#'  \item{\strong{weekly.seasonality}}  {Fit weekly seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{yearly.seasonality}}  {Fit yearly seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{regressor}}  {The name of the external regressor (or regressors) to include in the model. It has to exist on df as a column. If a vector is parsed, 1 regressor at a time is tested (as a model parameter essentially). If "no_regressor" is parsed, a univariate time-series model is estimated. "no_regressor" can be parsed as an element of the vector as well. }
#'  \item{\strong{standardize_regressor}}  {Bool, specify whether this regressor will be standardized prior to fitting. Can be 'auto' (standardize if not binary), True, or False.}
#'  \item{\strong{log_transformation}}  {Bool, specify whether the Target Variable will be log transformed pre-fitting the models or not.}
#' }
#' @param best_model_in A character value either: 'train', 'test' or 'mix_train_test'. Defaults to 'train'. This parameter defines the criteria to pick the best model - either based on accuracy on training set or in test set. The user might have different business requirements.
#' @param train_set_imp_perc A numeric input between 0 and 1 representing the weight to give to the training set results relatively to the test set. Defaults to 0.5 (mean results). This parameter only affects the results when 'best_model_in' is set to 'mix_train_test'. When set to 1 the results are the same as setting 'best_model_in' to 'train'.
#' @param main_accuracy_metric A character value either: 'MAPE', 'MSE', 'MAE' or 'RMSE' (it defaults to MAPE). This defines the criteria for selecting the best model (together with the 'best_model_in' parameter).
#' @param holidays A data-frame with columns holiday (character) and ds (date type)and optionally columns lower_window and upper_window which specify a range of days around the date to be included as holidays. lower_window=-2 will include 2 days prior to the date as holidays. Also optionally can have a column prior_scale specifying the prior scale for each holiday. It defaults to NULL in which case no holidays are used.
#' @param judgmental_forecasts A names vector with the date as name and the value of the judmental forecast. For example if we know that allways on the xmas day the value we are trying to predict is zero we can parse judgmental_forecasts = c('2016-12-25' = 1,  '2017-12-25' = 1, '2018-12-25' = 1). If the judgemental forecast is zero don't parse the value zero and parse 1 instead. This will facilitate with log transformations.
#' @param plotFrom A character value ('yyyy-mm-dd') representing a date to filter the data from to plot the best model based on the 'best_model_in' parameter (actuals vs forecast).
#' @param seed A seed.
#'
#'
#' @return This function returns a list with 3 elements:
#'  \itemize{
#'  \item{\strong{Accuracy_Overview}}  {A data-frame with all the trained models and accuracy metrics divided by train and test set. The metrics are: MAPE', 'MSE', 'MAE' or 'RMSE'.}
#'  \item{\strong{Actuals_vs_Predictions}}  {A data-frame with the actuals vs predictions and with upper and lower bound confidence intervals. This output also contains a column 'diff' with the Absolute Percentage Error for each prediction allowing the user to easily identify areas of miss-prediction. If log_transformation is applied the output will also include log predictions and log actuals.}
#'  \item{\strong{Plot_Actual_Predictions}}  {A ggplot with actuals vs predictions and a separator for train/test split for the best model (based on 'best_model_in' and 'main_accuracy_metric' parameters).}
#' }
#'
#' @details Since this is a wrapper for Prophet, you can find extra parameters information on Prophet documentation \code{?prophet}.
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
#' regressor = "x" )
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




Prophet_Wrapper = function(df, list_params, holidays = NULL, best_model_in = "train", plotFrom = NULL, main_accuracy_metric = "MAPE", train_set_imp_perc = 0.5, judgmental_forecasts = NULL, seed = 12345){

  is.date <- function(x) inherits(x, 'Date')

  #~~~ ERROR CONTROLS =================================================================

  if(is.null(df)){
    stop("No df provided ('df').")
  }

  if( !("data.frame" %in% class(df))){
    stop("The object parsed as 'df' is not a data.frame.")
  }

  if(ncol(df) < 3){
    stop('The df object has to include at least 3 columns (a target_var, a date var and a regressor).')
  }

  if(sum(sapply(df, is.numeric)) == 0){
    stop("No numeric/integer type column was identified in 'df'. Please include a numeric/integer and a date column to continue.")
  }

  if(sum(sapply(df, is.date)) != 1){
    stop("No Date type column was identified in 'df'. Please include a numeric and a date column to continue.")
  }

  length_reg = length(list_params$regressor)
  tryCatch({df[,list_params$target_variable]}, error = function(e){stop(paste0("The Target Variable ", list_params$target_variable, " is not in df."))})


  for(i in 1:length_reg){

    if(list_params$regressor[i] != "no_regressor"){
    tryCatch({df[,list_params$regressor[i]]}, error = function(e){stop(paste0("The Regressor ", list_params$regressor[i], " is not in df."))})

    }else{next()}

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

  if(best_model_in != "train" & best_model_in != "test" & best_model_in != "mix_train_test" ){
    stop("The 'best_model_in' argument has to be either 'train', 'test' or 'mix_train_test'.")
  }

  if(train_set_imp_perc > 1  & train_set_imp_perc <0 ){
    stop("The 'train_set_imp_perc' argument has to have a value between 0 and 1.")
  }

  if(main_accuracy_metric != "MAPE" & main_accuracy_metric != "MSE" & main_accuracy_metric != "MAE" & main_accuracy_metric != "RMSE"){
    stop("The 'main_accuracy_metric' argument has to be either MAPE, MSE, MAE or RMSE (it defaults to MAPE).")
  }

  if(sum(names(list_params) %in% c("weekly.seasonality", "yearly.seasonality", "standardize_regressor", "log_transformation", "target_variable", "regressor" )) != 6){
    stop(paste0("The list_params argument has to include the following elements:, ", paste0(c("weekly.seasonality", "yearly.seasonality", "standardize_regressor", "log_transformation", "target_variable", "regressor" ), collapse = ", "), ","))
  }

  if(list_params$weekly.seasonality != FALSE & list_params$weekly.seasonality != TRUE){
    stop("The 'list_params$weekly.seasonality' argument has to be a bolean (TRUE or FALSE).")
  }

  if(list_params$yearly.seasonality != FALSE & list_params$yearly.seasonality != TRUE){
    stop("The 'list_params$yearly.seasonality' argument has to be a bolean (TRUE or FALSE).")
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

  if(is.null(list_params$regressor.prior.scale) & !is.null(list_params$holidays.prior.scale) & list_params$regressor != "no_regressor"){list_params$regressor.prior.scale = list_params$holidays.prior.scale; cat("Defaulting regressor.prior.scale to holidays.prior.scale ...\n\n")}

  if(is.null(list_params$regressor.prior.scale) & !is.null(list_params$holidays.prior.scale) & list_params$regressor == "no_regressor"){list_params$regressor.prior.scale = 10; cat("Defaulting regressor.prior.scale to 10 ...\n\n")}

  if(is.null(list_params$holidays.prior.scale)){list_params$holidays.prior.scale = 10; cat("Defaulting holidays.prior.scale to 10 ... \n\n")}


  #~~~ Printing Informative Messeges =================================================================

  cat(paste0("We are testing Prophet models for ", length(list_params$changepoint.prior.scale), " values of changepoint.prior.scale and ", length(list_params$regressor.prior.scale), " of regressor.prior.scale, ", length(list_params$regressor),   " regressors and ", length(list_params$holidays.prior.scale), " values of holidays.prior.scale. This is a total of ", length(list_params$regressor.prior.scale) * length(list_params$changepoint.prior.scale) * length(list_params$regressor) * length(list_params$holidays.prior.scale), " models.\n\n"))
  cat(paste0("If there are no surprises, it should take maximum of ", round(((length(list_params$regressor.prior.scale) * length(list_params$changepoint.prior.scale) * length(list_params$regressor) * length(list_params$holidays.prior.scale)) * 10)/60, 2), " minutes to run ...\n\n"))


  #~~~ Create Train and Testing Set =================================================================

  if(list_params$log_transformation){
    log_transf = paste0("log(target_var)")
  }else{
    log_transf = paste0("target_var")
  }

  df_train = original %>%
    padr::pad(interval = "day") %>%
    dplyr::mutate(
      target_var = ifelse(target_var == 0 & list_params$log_transformation, dplyr::lag(target_var), target_var),
      ds = Date,
      y = eval(parse(text = log_transf))) %>%
    dplyr::filter(train == 1)

  df_test = original %>%
    padr::pad(interval = "day") %>%
    dplyr::mutate(
      target_var = ifelse(target_var == 0 & list_params$log_transformation, dplyr::lag(target_var), target_var),
      ds = Date,
      y = eval(parse(text = log_transf))) %>%
    dplyr::filter(train == 0)

  #~~~ Create a list of with predictions and accuracy data.frames for each combination of  changepoint.prior.scale and prior.scale. =================================================================

  final = lapply(list_params$changepoint.prior.scale, function(x){

    lapply(list_params$regressor.prior.scale, function(y){

      lapply(list_params$regressor, function(z){

        lapply(list_params$holidays.prior.scale, function(w){


      #####Models

      set.seed(seed = seed)

      if(is.null(holidays)){

        model = prophet::prophet(df = df_train,
                                 growth = "linear",
                                 weekly.seasonality = list_params$weekly.seasonality,
                                 changepoint.prior.scale = x,
                                 yearly.seasonality  = list_params$yearly.seasonality,
                                 fit = FALSE)

      }else{

        model = prophet::prophet(df = df_train,
                                 growth = "linear",
                                 weekly.seasonality = list_params$weekly.seasonality,
                                 changepoint.prior.scale = x,
                                 yearly.seasonality  = list_params$yearly.seasonality,
                                 holidays = holidays,
                                 holidays.prior.scale = w,
                                 fit = FALSE)
      }


      if(z != "no_regressor"){

        model = prophet::add_regressor(model, z, standardize = list_params$standardize_regressor, prior.scale = y)

      }

      model = prophet::fit.prophet(model, df = df_train)

      future_prophet_complete = prophet::make_future_dataframe(model, periods = nrow(df_test)) %>%
        dplyr::mutate(ds = as.Date(ds)) %>%
        dplyr::left_join(original, by = c("ds" = "Date"))

      forecast <- predict(model, future_prophet_complete)


      ##### outputs

      # If judgemental forecast points are parsed we define them here by parsing these to ypred and y in the calculation of accuracy metrics:
      if(!is.null(judgmental_forecasts)){


        adj_judmental_forecasts_log = c()
        adj_judmental_forecasts_nolog = c()
        adj_judmental_actual = c()

        for(i in 1:length(judgmental_forecasts)){
          adj_judmental_forecasts_log[i] = paste0("ypred_temp_log = ifelse(as.Date(ds) == as.Date('", names(judgmental_forecasts[i]), "'), ", judgmental_forecasts[i], ", ypred_temp_log)")
          adj_judmental_forecasts_nolog[i] = paste0("ypred_temp_nolog = ifelse(as.Date(ds) == as.Date('", names(judgmental_forecasts[i]), "'), ", judgmental_forecasts[i], ", ypred_temp_nolog)")
          adj_judmental_actual[i] = paste0("target_var_judg = ifelse(as.Date(Date) == as.Date('", names(judgmental_forecasts[i]), "'), ", judgmental_forecasts[i], ", target_var_judg)")
        }

        forecast_temp_eval = paste0("forecast_tmp = forecast %>%
          dplyr::mutate(ypred_temp_log = exp(yhat),
                        ypred_temp_nolog = yhat,",
                        paste0(adj_judmental_forecasts_log, collapse = ", "), ", ",
                        paste0(adj_judmental_forecasts_nolog, collapse = ", "),
          ") %>%
          dplyr::select(ds,  ypred_temp_log, ypred_temp_nolog)")

        eval(parse(text = forecast_temp_eval))

        original_temp_eval = paste0("original_tmp = original %>%
          dplyr::mutate(target_var_judg = target_var, ", paste0(adj_judmental_actual, collapse = ", "),
               ")")

        eval(parse(text = original_temp_eval))

        y_pred_test_log = forecast_tmp %>% dplyr::filter(as.Date(ds) >= as.Date(min(df_test$ds))) %>% dplyr::select(ypred_temp_log) %>% unlist()
        y_pred_test_nolog = forecast_tmp %>% dplyr::filter(as.Date(ds) >= as.Date(min(df_test$ds))) %>% dplyr::select(ypred_temp_nolog) %>% unlist()

        y_true_test_log = original_tmp %>% dplyr::filter(as.Date(Date) >= as.Date(min(df_test$ds))) %>% dplyr::select(target_var_judg) %>% unlist()
        y_true_test_nolog = original_tmp %>% dplyr::filter(as.Date(Date) >= as.Date(min(df_test$ds))) %>% dplyr::select(target_var_judg) %>% unlist()

        y_pred_train_log = forecast_tmp %>% dplyr::filter(as.Date(ds) < as.Date(min(df_test$ds))) %>% dplyr::select(ypred_temp_log) %>% unlist()
        y_pred_train_nolog = forecast_tmp %>% dplyr::filter(as.Date(ds) < as.Date(min(df_test$ds))) %>% dplyr::select(ypred_temp_nolog) %>% unlist()

        y_true_train_log = original_tmp %>% dplyr::filter(as.Date(Date) < as.Date(min(df_test$ds))) %>% dplyr::select(target_var_judg) %>% unlist()
        y_true_train_nolog = original_tmp %>% dplyr::filter(as.Date(Date) < as.Date(min(df_test$ds))) %>% dplyr::select(target_var_judg) %>% unlist()

      }else{

        y_pred_test_log = exp(forecast$yhat[as.Date(forecast$ds) >= as.Date(min(df_test$ds))])
        y_pred_train_log = exp(forecast$yhat[as.Date(forecast$ds) < as.Date(min(df_test$ds))])
        y_true_test_log = exp(df_test$y)
        y_true_train_log = exp(df_train$y)

        y_pred_test_nolog = forecast$yhat[as.Date(forecast$ds) >= as.Date(min(df_test$ds))]
        y_pred_train_nolog = forecast$yhat[as.Date(forecast$ds) < as.Date(min(df_test$ds))]
        y_true_test_nolog = df_test$y
        y_true_train_nolog = df_train$y

      }


      ####

      if(list_params$log_transformation){

        test = data.frame( Error_Type = "Test Set",
                           regressor = z,
                           changepoint.prior.scale = x,
                           regressor.prior.scale = y,
                           holidays.prior.scale = w,
                           MAPE = MLmetrics::MAPE(y_pred = y_pred_test_log, y_true = y_true_test_log),
                           MSE = MLmetrics::MSE(y_pred = y_pred_test_log, y_true = y_true_test_log),
                           MAE = MLmetrics::MAE(y_pred = y_pred_test_log, y_true = y_true_test_log),
                           RMSE = MLmetrics::RMSE(y_pred = y_pred_test_log, y_true = y_true_test_log),
                           stringsAsFactors = FALSE)

        train = data.frame( Error_Type = "Train Set",
                            regressor = z,
                            changepoint.prior.scale = x,
                            regressor.prior.scale = y,
                            holidays.prior.scale = w,
                            MAPE = MLmetrics::MAPE(y_pred = y_pred_train_log, y_true = y_true_train_log),
                            MSE = MLmetrics::MSE(y_pred = y_pred_train_log, y_true = y_true_train_log),
                            MAE = MLmetrics::MAE(y_pred = y_pred_train_log, y_true = y_true_train_log),
                            RMSE = MLmetrics::RMSE(y_pred = y_pred_train_log, y_true = y_true_train_log),
                            stringsAsFactors = FALSE)

      }else{

        test = data.frame( Error_Type = "Test Set",
                           regressor = z,
                           changepoint.prior.scale = x,
                           regressor.prior.scale = y,
                           holidays.prior.scale = w,
                           MAPE = MLmetrics::MAPE(y_pred = y_pred_test_nolog, y_true = y_true_test_nolog),
                           MSE = MLmetrics::MSE(y_pred = y_pred_test_nolog, y_true = y_true_test_nolog),
                           MAE = MLmetrics::MAE(y_pred = y_pred_test_nolog, y_true = y_true_test_nolog),
                           RMSE = MLmetrics::RMSE(y_pred = y_pred_test_nolog, y_true = y_true_test_nolog),
                           stringsAsFactors = FALSE)

        train = data.frame( Error_Type = "Train Set",
                            regressor = z,
                            changepoint.prior.scale = x,
                            regressor.prior.scale = y,
                            holidays.prior.scale = w,
                            MAPE = MLmetrics::MAPE(y_pred = y_pred_train_nolog, y_true = y_true_train_nolog),
                            MSE = MLmetrics::MSE(y_pred = y_pred_train_nolog, y_true = y_true_train_nolog),
                            MAE = MLmetrics::MAE(y_pred = y_pred_train_nolog, y_true = y_true_train_nolog),
                            RMSE = MLmetrics::RMSE(y_pred = y_pred_train_nolog, y_true = y_true_train_nolog),
                            stringsAsFactors = FALSE)

      }

      df_accuracy = dplyr::bind_rows(test, train)

      holiday_dates = unique(holidays$ds)

      if(!is.null(holidays)){
        map_holidays = "Holiday = (as.Date(Date) %in% as.Date(holiday_dates)), "
        select_holidays_no_log = " dplyr::select(Date, regressor, changepoint.prior.scale, regressor.prior.scale, holidays.prior.scale, actuals, yhat, yhat_lower, yhat_upper, diff_abs, diff, WeekDay, Holiday)"
        select_holidays_log = " dplyr::select(Date, regressor, changepoint.prior.scale, regressor.prior.scale, holidays.prior.scale, actuals, actuals_log, yhat, yhat_log, yhat_lower_log, yhat_upper_log, yhat_lower, yhat_upper, diff_abs, diff, WeekDay, Holiday) "
      }else{
        map_holidays = ""
        select_holidays_no_log = " dplyr::select(Date, regressor, changepoint.prior.scale, regressor.prior.scale, holidays.prior.scale, actuals, yhat, yhat_lower, yhat_upper, diff_abs, diff, WeekDay)"
        select_holidays_log = " dplyr::select(Date, regressor, changepoint.prior.scale, regressor.prior.scale, holidays.prior.scale, actuals, actuals_log, yhat, yhat_log, yhat_lower_log, yhat_upper_log, yhat_lower, yhat_upper, diff_abs, diff, WeekDay) "
      }

      if(list_params$log_transformation){
        actuals_vs_forecast_with_log_expr = paste0("forecast %>%
                                                   dplyr::rename(Date = ds,
                                                   yhat_upper_log = yhat_upper,
                                                   yhat_lower_log = yhat_lower,
                                                   yhat_log = yhat) %>%
                                                   dplyr::mutate(WeekDay = weekdays.Date(Date),",
                                                   map_holidays,
                                                   "actuals = c(y_true_train_log, y_true_test_log),
                                                   actuals_log = c(df_train$y, df_test$y),
                                                   yhat = c(y_pred_train_log, y_pred_test_log),
                                                   yhat_lower = exp(yhat_lower_log),
                                                   yhat_upper = exp(yhat_upper_log),
                                                   diff_abs = abs(actuals - yhat)/actuals,
                                                   diff = (actuals - yhat)/actuals,
                                                   changepoint.prior.scale = x,
                                                   regressor.prior.scale = y,
                                                   holidays.prior.scale = w,
                                                   regressor = z) %>%",
                                                   select_holidays_log
                                                   )

        actuals_vs_forecast = eval(parse(text = actuals_vs_forecast_with_log_expr))

      }else{

        actuals_vs_forecast_with_log_expr = paste0("forecast %>%
                                                   dplyr::rename(Date = ds) %>%
                                                   dplyr::mutate(WeekDay = weekdays.Date(Date),",
                                                   map_holidays,
                                                   "actuals = c(y_true_train_nolog, y_true_test_nolog),
                                                    yhat = c(y_pred_train_nolog, y_pred_test_nolog),
                                                   diff_abs = abs(actuals - yhat)/actuals,
                                                   diff = (actuals - yhat)/actuals,
                                                   regressor = z,
                                                   changepoint.prior.scale = x,
                                                   regressor.prior.scale = y,
                                                   holidays.prior.scale = w) %>%",
                                                   select_holidays_no_log)

        actuals_vs_forecast = eval(parse(text = actuals_vs_forecast_with_log_expr))
      }


      final_list = list(accuracy_overview = df_accuracy,
                        actuals_vs_forecast = actuals_vs_forecast)
      return(final_list)

    })

  })

  })

  })


  #~~~ Prepare the Output by reducing the lists and creating a graph of actuals vs forecasts for the best model in terms of 'best_model_in' =================================================================


  final <- unlist(unlist(unlist(final, recursive = FALSE), recursive = FALSE), recursive = FALSE)
  final <- do.call("rbind", final)

  unique_combinations = length(list_params$changepoint.prior.scale) * length(list_params$regressor.prior.scale) * length(list_params$regressor) * length(list_params$holidays.prior.scale)

  if(is.null(plotFrom)){
    plotFrom = min(df_train$ds)
  }else{
    plotFrom = as.Date(plotFrom)
  }

  #identify the best model based on best_model_in parameter:

  if(best_model_in != "mix_train_test"){
    accuracies = invisible(final[1:unique_combinations] %>%
                             dplyr::bind_rows() %>%
                             dplyr::filter(grepl(pattern = best_model_in, x = Error_Type, ignore.case = TRUE)) %>%
                             dplyr::arrange(!!rlang::sym(main_accuracy_metric)) %>%
                             dplyr::select(changepoint.prior.scale,
                                           regressor.prior.scale,
                                           regressor,
                                           holidays.prior.scale) %>%
                             head(1))

  }else{

    accuracies = final[1:unique_combinations] %>%
      dplyr::bind_rows() %>%
      dplyr::filter(grepl(pattern = "test|train", x = Error_Type, ignore.case = TRUE)) %>%
      dplyr::select(Error_Type, changepoint.prior.scale, regressor.prior.scale, regressor, holidays.prior.scale, !!main_accuracy_metric) %>%
      tidyr::spread(key = Error_Type, !!main_accuracy_metric) %>%
      dplyr::mutate(avg_accuracy_metric = (`Train Set` * train_set_imp_perc  + `Test Set` * (1- train_set_imp_perc)  )) %>%
      dplyr::arrange(avg_accuracy_metric) %>%
      dplyr::select(changepoint.prior.scale, regressor.prior.scale, regressor, holidays.prior.scale) %>%
      head(1)

  }

  accuracies_graph = final[1:unique_combinations] %>%
    dplyr::bind_rows() %>%
    dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale, regressor.prior.scale == accuracies$regressor.prior.scale, regressor == accuracies$regressor, holidays.prior.scale == accuracies$holidays.prior.scale)

  df = invisible(final[(unique_combinations + 1):(unique_combinations*2)] %>%
                   dplyr::bind_rows() %>%
                   dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale, regressor.prior.scale == accuracies$regressor.prior.scale, regressor == accuracies$regressor, holidays.prior.scale == accuracies$holidays.prior.scale) %>%
                   dplyr::select(Date, actuals, yhat, yhat_lower, yhat_upper) %>%
                   tidyr::gather(`Actuals vs Forecast`, Volumes, actuals:yhat) %>%
                   dplyr::filter(as.Date(Date) >= as.Date(plotFrom)))

  graph1 = ggplot2::ggplot(df, ggplot2::aes(x = as.Date(Date), y = Volumes, color = `Actuals vs Forecast`, linetype = `Actuals vs Forecast`)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = as.numeric(max(df_train$ds))), linetype = 4, colour = "#40dfad", alpha = 0.7) +
    ggthemes::theme_tufte() +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_x_date(breaks = scales::date_breaks("2 months")) +
    ggthemes::scale_color_stata()

  graph2 = invisible(gridExtra::tableGrob(accuracies_graph, rows = NULL))

  graph_final = invisible(gridExtra::arrangeGrob(graph2, graph1, nrow = 2, heights = c(0.3, 2)))



  list(Accuracy_Overview = invisible(final[1:unique_combinations] %>% dplyr::bind_rows() %>% dplyr::mutate(best_model = ifelse(changepoint.prior.scale == accuracies$changepoint.prior.scale & regressor.prior.scale == accuracies$regressor.prior.scale & regressor == accuracies$regressor & holidays.prior.scale == accuracies$holidays.prior.scale, 1, 0))),
       Actuals_vs_Predictions = invisible(final[(unique_combinations + 1):(unique_combinations*2)] %>% dplyr::bind_rows()),
       Plot_Actual_Predictions = graph1)


  }
