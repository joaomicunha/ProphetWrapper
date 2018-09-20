
#' cv_wrapper
#'
#' Function used to simplify the cross validation workflow in ProphetWrapper (not exported)
#'
#' @param df The full data set
#' @param period.param Integer amount of time between cutoff dates. Same units as horizon. If not provided, 0.5 * horizon is used.
#' @param initial.param Integer size of the first training period. If not provided, 3 * horizon is used. Same units as horizon.
#' @param horizon.param Integer size of the horizon.
#' @param model.param The model
#' @param debug For debug
#' @param list.params A list with parameters passed by the user
#' @param judgmental.forecasts judgmental forecast named vector
#' @param regressor1_cv parameters from list.params
#' @param regressor2_cv parameters from list.params
#' @param changepoint.prior.scale_cv parameters from list.params
#' @param seasonality.prior.scale_cv parameters from list.params
#' @param regressor.prior.scale_cv parameters from list.params
#' @param holidays.prior.scale_cv parameters from list.params
#' @param main.accuracy.metric main accuracy metric
#' @param df.test test set
#'
#' @import magrittr
#'
#'

cv_wrapper = function(df, period.param, horizon.param, model.param, initial.param, list.params, judgmental.forecasts, regressor1_cv, regressor2_cv, changepoint.prior.scale_cv, seasonality.prior.scale_cv, regressor.prior.scale_cv, holidays.prior.scale_cv, df.test, main.accuracy.metric, debug){

  if(debug){browser()}

  cat("\n\nCross-Validation initiated ...\n\n")

  #If no horizon parameter is set we default to the size of the test set:
  horizon.param =  ifelse(is.null(horizon.param), nrow(df.test), horizon.param)

  cv.results = prophet::cross_validation(model = model.param,
                                         period = period.param,
                                         initial = initial.param,
                                         horizon = horizon.param,
                                         units = paste0(padr::get_interval(df$ds), "s")) %>%
                            dplyr::mutate(Date = ds)


  #aggregate the results by cv cutoff:
  # Have to take into account that the results can potentially be log transformed and/or affected by judgemental forecasts:


  cv.results.processed = cv.results %>%
                            dplyr::mutate(y = ifelse(rep(list.params$log_transformation, nrow(cv.results)), exp(y), y) ,
                                          yhat = ifelse(rep(list.params$log_transformation, nrow(cv.results)), exp(yhat), yhat),
                                          yhat_lower = ifelse(rep(list.params$log_transformation, nrow(cv.results)), exp(yhat_lower), yhat_lower),
                                          yhat_upper = ifelse(rep(list.params$log_transformation, nrow(cv.results)), exp(yhat_upper), yhat_upper))


  if(is.null(judgmental.forecasts)){

    cv.results.final = cv.results.processed

  }else{

    adj_judmental_forecasts = c()
    adj_judmental_actual = c()

    for(i in 1:length(judgmental.forecasts)){
      adj_judmental_forecasts[i] = paste0("yhat = ifelse(as.Date(Date) == as.Date('", names(judgmental.forecasts[i]), "'), ", judgmental.forecasts[i], ", yhat)")
      adj_judmental_actual[i] = paste0("y = ifelse(as.Date(Date) == as.Date('", names(judgmental.forecasts[i]), "'), ", judgmental.forecasts[i], ", y)")
    }

    forecast_temp_eval = paste0("cv.results.final = cv.results.processed %>%",
                                "dplyr::mutate(",
                                paste0(adj_judmental_forecasts, collapse = ", "), ", ",
                                paste0(adj_judmental_actual, collapse = ", "),
                                ")")


    eval(parse(text = forecast_temp_eval))

  }


  overview_cv_results = cv.results.final %>%
                        dplyr::group_by(cutoff) %>%
                        dplyr::summarise(min_date_fold = min(Date),
                                         max_date_fold = max(Date),
                                         number_of_periods_fold = max_date_fold - min_date_fold,
                                         MAPE = MLmetrics::MAPE(y_pred = yhat, y_true = y),
                                         MSE = MLmetrics::MSE(y_pred = yhat, y_true = y),
                                         MAE = MLmetrics::MAE(y_pred = yhat, y_true = y),
                                         RMSE = MLmetrics::RMSE(y_pred = yhat, y_true = y),
                                         MPE = mean((y - yhat)/y)) %>%
                        dplyr::mutate(fold_number = row_number(),
                                      regressor1 = regressor1_cv,
                                      regressor2 = regressor2_cv,
                                      changepoint.prior.scale = changepoint.prior.scale_cv,
                                      seasonality.prior.scale = seasonality.prior.scale_cv,
                                      regressor.prior.scale = regressor.prior.scale_cv,
                                      holidays.prior.scale = holidays.prior.scale_cv) %>%
                        dplyr::ungroup() %>%
                        dplyr::select(fold_number, dplyr::everything())



    accuracies_cv =  data.frame(
                            Error_Type = "CV",
                            regressor1 = regressor1_cv,
                            regressor2 = regressor2_cv,
                            changepoint.prior.scale = changepoint.prior.scale_cv,
                            seasonality.prior.scale = seasonality.prior.scale_cv,
                            regressor.prior.scale = regressor.prior.scale_cv,
                            holidays.prior.scale = holidays.prior.scale_cv,
                            MAPE = mean(overview_cv_results$MAPE),
                            MSE = mean(overview_cv_results$MSE),
                            MAE = mean(overview_cv_results$MAE),
                            RMSE = mean(overview_cv_results$RMSE),
                            MPE = mean(overview_cv_results$MPE),
                        stringsAsFactors = FALSE  )

    graph_cv = prophet::plot_cross_validation_metric(cv.results.final, metric = ifelse(!(tolower(main.accuracy.metric) %in% c('mse', 'rmse', 'mae', 'mape', 'coverage')), 'mape', tolower(main.accuracy.metric)))

     return(
       list(accuracies_cv = accuracies_cv,
                 overview_cv_results = overview_cv_results,
                 graph_cv = graph_cv)
       )

}
