


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
#'  \item{\strong{regressor.prior.scale}}  {A regularization parameter (vector or single value) for the external regressor. If the regressor is being overfit (too much flexibility) or underfit (not enough flexibility), you can adjust the strength of this argument. By default Prophet sets this parameter to 0.05. Increasing it will make the trend more flexible.}
#'  \item{\strong{weekly.seasonality}}  {Fit weekly seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{yearly.seasonality}}  {Fit yearly seasonality. Can be 'auto', TRUE, FALSE, or a number of Fourier terms to generate.}
#'  \item{\strong{regressor}}  {The name of the external regressor to include in the model. It has to exist on df as a column.}
#'  \item{\strong{standardize_regressor}}  {Bool, specify whether this regressor will be standardized prior to fitting. Can be 'auto' (standardize if not binary), True, or False.}
#'  \item{\strong{log_transformation}}  {Bool, specify whether the Target Variable will be log transformed pre-fitting the models or not.}
#' }
#' @param best_model_in A character value either: 'train' or 'test'. Defaults to 'train'. This parameter defines the criteria to pick the best model - either based on accuracy on training set or in test set. The user might have different business requirements.
#' @param main_accuracy_metric A character value either: 'MAPE', 'MSE', 'MAE' or 'RMSE' (it defaults to MAPE). This defines the criteria for selecting the best model (together with the 'best_model_in' parameter).
#' @param holidays A data-frame with columns holiday (character) and ds (date type)and optionally columns lower_window and upper_window which specify a range of days around the date to be included as holidays. lower_window=-2 will include 2 days prior to the date as holidays. Also optionally can have a column prior_scale specifying the prior scale for each holiday. It defaults to NULL in which case no holidays are used.
#' @param plotFrom A character value ('yyyy-mm-dd') representing a date to filter the data from to plot the best model based on the 'best_model_in' parameter (actuals vs forecast).
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
#'                     main_accuracy_metric = "MAPE")
#'
#'}
#'
#' @import magrittr
#'
#' @export
#'




Prophet_Wrapper = function(df, list_params, holidays = NULL, best_model_in = "train", plotFrom = NULL, main_accuracy_metric = "MAPE"){

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

  tryCatch({df[,list_params$target_variable]}, error = function(e){stop(paste0("The Target Variable ", list_params$target_variable, " is not in df."))})
  tryCatch({df[,list_params$regressor]}, error = function(e){stop(paste0("The Regressor ", list_params$regressor, " is not in df."))})
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

  if(best_model_in != "train" & best_model_in != "test"){
    stop("The 'best_model_in' argument has to be either 'train' or 'test'.")
  }

  if(main_accuracy_metric != "MAPE" & main_accuracy_metric != "MSE" & main_accuracy_metric != "MAE" & main_accuracy_metric != "RMSE"){
    stop("The 'main_accuracy_metric' argument has to be either MAPE, MSE, MAE or RMSE (it defaults to MAPE).")
  }

  if(sum(names(list_params) %in% c("changepoint.prior.scale","regressor.prior.scale", "weekly.seasonality", "yearly.seasonality", "standardize_regressor", "log_transformation", "target_variable", "regressor" )) != 8){
    stop(paste0("The list_params argument has to include the following elements:, ", paste0(c("changepoint.prior.scale","regressor.prior.scale", "weekly.seasonality", "yearly.seasonality", "standardize_regressor", "log_transformation", "target_variable", "regressor" ), collapse = ", "), ","))
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

  if(class(list_params$changepoint.prior.scale) != "numeric"){
    stop("The 'list_params$changepoint.prior.scale' argument has to be a numeric vector.")
  }

  if(class(list_params$regressor.prior.scale) != "numeric"){
    stop("The 'list_params$regressor.prior.scale' argument has to be a numeric vector.")
  }

  #~~~ Printing Informative Messeges =================================================================

  cat(paste0("We are testing Prophet models for ", length(list_params$changepoint.prior.scale), " values of changepoint.prior.scale and ", length(list_params$regressor.prior.scale), " of regressor.prior.scale (total of ", length(list_params$regressor.prior.scale) * length(list_params$changepoint.prior.scale), " models).\n\n"))
  cat(paste0("If there are no surprises, it should take maximum of ", round(((length(list_params$regressor.prior.scale) * length(list_params$changepoint.prior.scale))*10)/60, 2), " minutes to run ...\n\n"))


  #~~~ Create Train and Testing Set =================================================================

  if(list_params$log_transformation){
    log_transf = paste0("log(target_var)")
  }else{
    log_transf = paste0("target_var")
  }

  df_train = original %>%
    padr::pad(interval = "day") %>%
    dplyr::mutate(
      ds = Date,
      y = eval(parse(text = log_transf))) %>%
    dplyr::filter(train == 1) %>%
    dplyr::select(ds, y, !!list_params$regressor)

  df_test = original %>%
    padr::pad(interval = "day") %>%
    dplyr::mutate(
      ds = Date,
      y = eval(parse(text = log_transf))) %>%
    dplyr::filter(train == 0) %>%
    dplyr::select(ds, y, !!list_params$regressor)

  #~~~ Create a list of with predictions and accuracy data.frames for each combination of  changepoint.prior.scale and prior.scale. =================================================================

  final = lapply(list_params$changepoint.prior.scale, function(x){

    lapply(list_params$regressor.prior.scale, function(y){

      #####Models

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
                                 fit = FALSE)
      }


      model <- prophet::add_regressor(model, list_params$regressor, standardize = list_params$standardize_regressor, prior.scale = y)
      model = prophet::fit.prophet(model, df = df_train)

      future_prophet_complete = prophet::make_future_dataframe(model, periods = 35) %>%
        dplyr::mutate(ds = as.Date(ds)) %>%
        dplyr::left_join(original, by = c("ds" = "Date"))

      forecast <- predict(model, future_prophet_complete)


      #####outputs
      test = data.frame( Error_Type = "Test Set",
                         changepoint.prior.scale = x,
                         regressor.prior.scale = y,
                         MAPE = MLmetrics::MAPE(y_pred = exp(forecast$yhat[forecast$ds >= as.Date(min(df_test$ds))]), y_true = exp(df_test$y)),
                         MSE = MLmetrics::MSE(y_pred = exp(forecast$yhat[forecast$ds >= as.Date(min(df_test$ds))]), y_true = exp(df_test$y)),
                         MAE = MLmetrics::MAE(y_pred = exp(forecast$yhat[forecast$ds >= as.Date(min(df_test$ds))]), y_true = exp(df_test$y)),
                         RMSE = MLmetrics::RMSE(y_pred = exp(forecast$yhat[forecast$ds >= as.Date(min(df_test$ds))]), y_true = exp(df_test$y)))

      train = data.frame( Error_Type = "Train Set",
                          changepoint.prior.scale = x,
                          regressor.prior.scale = y,
                          MAPE = MLmetrics::MAPE(y_pred = exp(forecast$yhat[forecast$ds < as.Date(min(df_test$ds))]), y_true = exp(df_train$y)),
                          MSE = MLmetrics::MSE(y_pred = exp(forecast$yhat[forecast$ds < as.Date(min(df_test$ds))]), y_true = exp(df_train$y)),
                          MAE = MLmetrics::MAE(y_pred = exp(forecast$yhat[forecast$ds < as.Date(min(df_test$ds))]), y_true = exp(df_train$y)),
                          RMSE = MLmetrics::RMSE(y_pred = exp(forecast$yhat[forecast$ds < as.Date(min(df_test$ds))]), y_true = exp(df_train$y)))

      df_accuracy = dplyr::bind_rows(test, train)

      holiday_dates = unique(holidays$ds)

      if(list_params$log_transformation){
        actuals_vs_forecast_with_log_expr = paste0("forecast %>%
                                                   dplyr::rename(Date = ds,
                                                   yhat_upper_log = yhat_upper,
                                                   yhat_lower_log = yhat_lower,
                                                   yhat_log = yhat) %>%
                                                   dplyr::mutate(WeekDay = weekdays.Date(Date),
                                                   Holiday = (as.Date(Date) %in% as.Date(holiday_dates)),
                                                   actuals = exp(c(df_train$y, df_test$y)),
                                                   actuals_log = c(df_train$y, df_test$y),
                                                   yhat = exp(yhat_log),
                                                   yhat_lower = exp(yhat_lower_log),
                                                   yhat_upper = exp(yhat_upper_log),
                                                   diff = abs(actuals - yhat)/actuals,
                                                   changepoint.prior.scale = x,
                                                   regressor.prior.scale = y) %>%
                                                   dplyr::select(Date, changepoint.prior.scale, regressor.prior.scale, actuals, actuals_log, yhat, yhat_log, yhat_lower_log, yhat_upper_log, yhat_lower, yhat_upper, diff, WeekDay)")

        actuals_vs_forecast = eval(parse(text = actuals_vs_forecast_with_log_expr))

      }else{
        actuals_vs_forecast_with_log_expr = paste0("forecast %>%
                                                   dplyr::mutate(WeekDay = weekdays.Date(ds),
                                                   Holiday = (as.Date(ds) %in% as.Date(holiday_dates)),
                                                   actuals = c(df_train$y, df_test$y),
                                                   diff = abs(actuals - yhat)/actuals,
                                                   changepoint.prior.scale = x,
                                                   regressor.prior.scale = y) %>%
                                                   dplyr::select(ds, changepoint.prior.scale, regressor.prior.scale, actuals, yhat, yhat_lower, yhat_upper, diff, WeekDay) %>%
                                                   dplyr::rename(Date = ds)")

        actuals_vs_forecast = eval(parse(text = actuals_vs_forecast_with_log_expr))
      }


      final_list = list(accuracy_overview = df_accuracy,
                        actuals_vs_forecast = actuals_vs_forecast)
      return(final_list)

    })

  })

  #~~~ Prepare the Output by reducing the lists and creating a graph of actuals vs forecasts for the best model in terms of 'best_model_in' =================================================================


  final <- unlist(final, recursive = FALSE)
  final <- do.call("rbind", final)

  unique_combinations = length(list_params$changepoint.prior.scale) * length(list_params$regressor.prior.scale)

  if(is.null(plotFrom)){
    plotFrom = min(df_train$ds)
  }else{
    plotFrom = as.Date(plotFrom)
  }

  accuracies = invisible(final[1:unique_combinations] %>%
                           dplyr::bind_rows() %>%
                           dplyr::filter(grepl(pattern = best_model_in, x = Error_Type, ignore.case = TRUE)) %>%
                           dplyr::arrange(!!rlang::sym(main_accuracy_metric)) %>%
                           dplyr::select(changepoint.prior.scale,
                                  regressor.prior.scale) %>%
                           head(1))

  accuracies_graph = final[1:unique_combinations] %>%
    dplyr::bind_rows() %>%
    dplyr::filter(changepoint.prior.scale = accuracies$changepoint.prior.scale, regressor.prior.scale = accuracies$regressor.prior.scale)

  df = invisible(final[(unique_combinations + 1):(unique_combinations*2)] %>%
                   dplyr::bind_rows() %>%
                   dplyr::filter(changepoint.prior.scale == accuracies$changepoint.prior.scale, regressor.prior.scale == accuracies$regressor.prior.scale) %>%
                   dplyr::select(Date, actuals, yhat, yhat_lower, yhat_upper) %>%
                   tidyr::gather(`Actuals vs Forecast`, Volumes, actuals:yhat) %>%
                   dplyr::filter(Date >= as.Date(plotFrom)))

  graph1 = ggplot2::ggplot(df, ggplot2::aes(x = as.Date(Date), y = Volumes, color = `Actuals vs Forecast`, linetype = `Actuals vs Forecast`)) +
    gplot2::geom_line() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = as.numeric(max(df_train$ds))), linetype = 4, colour = "#40dfad", alpha = 0.7) +
    ggthemes::theme_tufte() +
    gplot2::scale_y_continuous(labels = scales::comma_format()) +
    gplot2::scale_x_date(breaks = scales::date_breaks("2 months")) +
    ggthemes::scale_color_stata()

  graph2 = invisible(gridExtra::tableGrob(accuracies_graph, rows = NULL))

  graph_final = invisible(gridExtra::arrangeGrob(graph2, graph1, nrow = 2, heights = c(0.3, 2)))



  list(Accuracy_Overview = invisible(final[1:unique_combinations] %>% dplyr::bind_rows()),
       Actuals_vs_Predictions = invisible(final[(unique_combinations + 1):(unique_combinations*2)] %>% dplyr::bind_rows()),
       Plot_Actual_Predictions = graph1)


  }
