
#' cv_wrapper
#'
#' Function used to simplify the cross validation workflow in ProphetWrapper
#'
#' @param df The full data set
#' @param period_param Size of the total length of periods tested/validated.
#' @param horizon_param The length of each of the cross-validation folds.
#' @param model_param The model
#' @param debug For debug
#' @param list.params A list with parameters passed by the user
#' @param judgmental.forecasts judgmental forecast named vector
#'
#'
#' @import magrittr
#'
#'

cv_wrapper = function(df, period_param, horizon_param, model_param, list.params, judgmental.forecasts, regressor1_cv, regressor2_cv, changepoint.prior.scale_cv, regressor.prior.scale_cv, holidays.prior.scale_cv, debug){

  if(debug){browser()}

  cv.results = prophet::cross_validation(model = model_param, initial = (nrow(df) - period_param), horizon = horizon_param + 1, units = paste0(padr::get_interval(df$ds), "s"))

  #aggregate the results by cv cutoff

  if(!list.params$log_transformation & is.null(judgmental.forecasts)){

    cv.results.processed = cv.results %>%
                            dplyr::group_by(cutoff) %>%
                            dplyr::summarise(max_ds = max(ds),
                                             min_ds = min(ds),
                                             MAPE = MLmetrics::MAPE(y_pred = yhat, y_true = y),
                                             MSE = MLmetrics::MSE(y_pred = yhat, y_true = y),
                                             MAE = MLmetrics::MAE(y_pred = yhat, y_true = y),
                                             RMSE = MLmetrics::RMSE(y_pred = yhat, y_true = y),
                                             MPE = mean((y - yhat)/y)) %>%
                            dplyr::ungroup()

  }else if(!list.params$log_transformation & !is.null(judgmental.forecasts)){

    cv.results.processed = cv.results %>%
               dplyr::mutate(yhat = ifelse(as.Date(ds) == as.Date(names(judgmental.forecasts[i])), judgmental.forecasts[i], ypred_temp_nolog)) %>%
               dplyr::group_by(cutoff) %>%
               dplyr::summarise(max_ds = max(ds),
                                min_ds = min(ds),
                                MAPE = MLmetrics::MAPE(y_pred = yhat, y_true = y),
                                MSE = MLmetrics::MSE(y_pred = yhat, y_true = y),
                                MAE = MLmetrics::MAE(y_pred = yhat, y_true = y),
                                RMSE = MLmetrics::RMSE(y_pred = yhat, y_true = y),
                                MPE = mean((y - yhat)/y)) %>%
               dplyr::ungroup()

  }else if(list.params$log_transformation & is.null(judgmental.forecasts)){

    cv.results.processed = cv.results %>%
                      dplyr::group_by(cutoff) %>%
                      dplyr::summarise(max_ds = max(ds),
                                       min_ds = min(ds),
                                       MAPE = MLmetrics::MAPE(y_pred = exp(yhat), y_true = exp(y)),
                                       MSE = MLmetrics::MSE(y_pred = exp(yhat), y_true = exp(y)),
                                       MAE = MLmetrics::MAE(y_pred = exp(yhat), y_true = exp(y)),
                                       RMSE = MLmetrics::RMSE(y_pred = exp(yhat), y_true = exp(y)),
                                       MPE = mean((exp(y) - exp(yhat))/exp(y))) %>%
                      dplyr::ungroup()

  }else{

    #Needs fix:
    cv.results.processed = cv.results %>%
      dplyr::mutate(yhat = ifelse(as.Date(ds) == as.Date(names(judgmental.forecasts[i])), judgmental.forecasts[i], ypred_temp_nolog)) %>%
                      dplyr::group_by(cutoff) %>%
                      dplyr::summarise(max_ds = max(ds),
                                       min_ds = min(ds),
                                       MAPE = MLmetrics::MAPE(y_pred = exp(yhat), y_true = exp(y)),
                                       MSE = MLmetrics::MSE(y_pred = exp(yhat), y_true = exp(y)),
                                       MAE = MLmetrics::MAE(y_pred = exp(yhat), y_true = exp(y)),
                                       RMSE = MLmetrics::RMSE(y_pred = exp(yhat), y_true = exp(y)),
                                       MPE = mean((exp(y) - exp(yhat))/exp(y))) %>%
                      dplyr::ungroup()

  }


         accuracies_cv =  data.frame(
                  Error_Type = "CV",
                  regressor1 = regressor1_cv,
                  regressor2 = regressor2_cv,
                  changepoint.prior.scale = changepoint.prior.scale_cv,
                  regressor.prior.scale = regressor.prior.scale_cv,
                  holidays.prior.scale = holidays.prior.scale_cv,
                  MAPE_cv = mean(cv.results.processed$MAPE),
                  MSE_cv = mean(cv.results.processed$MSE),
                  MAE_cv = mean(cv.results.processed$MAE),
                  RMSE_cv = mean(cv.results.processed$RMSE),
                  MPE_cv = mean(cv.results.processed$MPE),
                  stringsAsFactors = F

                )

     return(accuracies_cv)





}
