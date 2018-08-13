
#' cv_wrapper
#'
#' Function used to simplify the cross validation workflow in ProphetWrapper
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
#' @param regressor.prior.scale_cv parameters from list.params
#' @param holidays.prior.scale_cv parameters from list.params
#'
#'
#' @import magrittr
#'
#'

cv_wrapper = function(df, period.param, horizon.param, model.param, initial.param, list.params, judgmental.forecasts, regressor1_cv, regressor2_cv, changepoint.prior.scale_cv, regressor.prior.scale_cv, holidays.prior.scale_cv, df.test, debug){

  if(debug){browser()}

  #If no horizon parameter is set we default to the size of the test set:
  horizon.param =  ifelse(is.null(horizon.param), nrow(df.test), horizon.param)

  cv.results = prophet::cross_validation(model = model.param,
                                         period = period.param,
                                         initial = initial.param,
                                         horizon = horizon.param + 1,
                                         units = paste0(padr::get_interval(df$ds), "s"))

  #aggregate the results by cv cutoff:
  # Have to take into account that the results can potentially be log transformed and/or affected by judgemental forecasts:


  cv.results.processed = cv.results %>%
                            dplyr::mutate(y = ifelse(rep(list.params$log_transformation, nrow(cv.results)), exp(y), y) ,
                                          yhat = ifelse(rep(list.params$log_transformation, nrow(cv.results)), exp(yhat), yhat))


  if(is.null(judgmental.forecasts)){

    cv.results.processed = cv.results.processed %>%
                              dplyr::group_by(cutoff) %>%
                              dplyr::summarise(max_ds = max(ds),
                                               min_ds = min(ds),
                                               MAPE = MLmetrics::MAPE(y_pred = yhat, y_true = y),
                                               MSE = MLmetrics::MSE(y_pred = yhat, y_true = y),
                                               MAE = MLmetrics::MAE(y_pred = yhat, y_true = y),
                                               RMSE = MLmetrics::RMSE(y_pred = yhat, y_true = y),
                                               MPE = mean((y - yhat)/y)) %>%
                              dplyr::ungroup()

  }else{

    adj_judmental_forecasts = c()
    adj_judmental_actual = c()

    for(i in 1:length(judgmental.forecasts)){
      adj_judmental_forecasts[i] = paste0("yhat = ifelse(as.Date(ds) == as.Date('", names(judgmental.forecasts[i]), "'), ", judgmental.forecasts[i], ", yhat)")
      adj_judmental_actual[i] = paste0("y = ifelse(as.Date(Date) == as.Date('", names(judgmental.forecasts[i]), "'), ", judgmental.forecasts[i], ", y)")
    }

    forecast_temp_eval = paste0("cv.results.processed = cv.results.processed %>%",
                                "dplyr::mutate(Date = as.Date(ds), ",
                                paste0(adj_judmental_forecasts, collapse = ", "), ", ",
                                paste0(adj_judmental_actual, collapse = ", "),
                                ") %>%
                                dplyr::group_by(cutoff) %>%
                                dplyr::summarise(max_ds = max(ds),
                                min_ds = min(ds),
                                MAPE = MLmetrics::MAPE(y_pred = yhat, y_true = y),
                                MSE = MLmetrics::MSE(y_pred = yhat, y_true = y),
                                MAE = MLmetrics::MAE(y_pred = yhat, y_true = y),
                                RMSE = MLmetrics::RMSE(y_pred = yhat, y_true = y),
                                MPE = mean((y - yhat)/y)) %>%
                                dplyr::ungroup()")

    eval(parse(text = forecast_temp_eval))

  }

    accuracies_cv =  data.frame(
                            Error_Type = "CV",
                            regressor1 = regressor1_cv,
                            regressor2 = regressor2_cv,
                            changepoint.prior.scale = changepoint.prior.scale_cv,
                            regressor.prior.scale = regressor.prior.scale_cv,
                            holidays.prior.scale = holidays.prior.scale_cv,
                            MAPE = mean(cv.results.processed$MAPE),
                            MSE = mean(cv.results.processed$MSE),
                            MAE = mean(cv.results.processed$MAE),
                            RMSE = mean(cv.results.processed$RMSE),
                            MPE = mean(cv.results.processed$MPE),
                            stringsAsFactors = FALSE  )

     return(accuracies_cv)

}
