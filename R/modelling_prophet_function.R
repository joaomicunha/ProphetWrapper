
#' modelling_prophet_function
#'
#' Function to run the Prophet models (not exported)
#'
#' @param modelling_type Either 'test' or 'final'. This parameter controls whether we are running test/train forecasts or a final one.
#' @param list_params_modelling A list with parameters passed by the user
#' @param judgmental_forecasts.modelling judgmental forecast named vector
#' @param regressor1.modelling parameters from list.params
#' @param regressor2.modelling parameters from list.params
#' @param changepoint.prior.scale.modelling parameters from list.params
#' @param regressor.prior.scale.modelling parameters from list.params
#' @param holidays.prior.scale.modelling parameters from list.params
#' @param df_test_modelling test data-frame
#' @param df_train_modelling train data-frame
#' @param df_all_modelling All data
#' @param debug_modelling to debug
#' @param holidays_modelling holidays
#'
#' @import magrittr
#'
#'


modelling_prophet_function = function(df_all_modelling, df_test_modelling, df_train_modelling, modelling_type, list_params_modelling, changepoint.prior.scale.modelling, holidays.prior.scale.modelling, regressor.prior.scale.modelling, regressor1.modelling, regressor2.modelling, judgmental_forecasts.modelling, holidays_modelling, debug_modelling){


    if(debug_modelling){browser()}

    if(modelling_type == 'final'){
      df_modelling = df_all_modelling
    }else{
      df_modelling = df_train_modelling
    }

    if(is.null(holidays_modelling)){

      model = prophet::prophet(df = df_modelling,
                               growth = "linear",
                               weekly.seasonality = list_params_modelling$weekly.seasonality,
                               changepoint.prior.scale = changepoint.prior.scale.modelling,
                               yearly.seasonality  = list_params_modelling$yearly.seasonality,
                               fit = FALSE)

    }else{

      model = prophet::prophet(df = df_modelling,
                               growth = "linear",
                               weekly.seasonality = list_params_modelling$weekly.seasonality,
                               changepoint.prior.scale = changepoint.prior.scale.modelling,
                               yearly.seasonality  = list_params_modelling$yearly.seasonality,
                               holidays = holidays_modelling,
                               holidays.prior.scale = holidays.prior.scale.modelling,
                               fit = FALSE)
    }

    #adding regressors:
    if(regressor1.modelling != "no_regressor"){

      model = prophet::add_regressor(model, regressor1.modelling, standardize = list_params_modelling$standardize_regressor, prior.scale = regressor.prior.scale.modelling)

    }

    if(regressor2.modelling != "no_regressor"){

      model = prophet::add_regressor(model, regressor2.modelling, standardize = list_params_modelling$standardize_regressor, prior.scale = regressor.prior.scale.modelling)

    }

    #fit the model:
    model = prophet::fit.prophet(model, df = df_modelling)

    future_prophet_complete = prophet::make_future_dataframe(model, periods = nrow(df_test_modelling), freq = padr::get_interval(df_all_modelling$ds)) %>%
      dplyr::mutate(ds = as.Date(ds))

    forecast <- predict(model, future_prophet_complete) %>%
      dplyr::mutate(ds = as.Date(ds)) %>%
      dplyr::left_join(dplyr::select(df_all_modelling, -Date), by = "ds") %>%
      dplyr::mutate(yhat = ifelse(rep(list_params_modelling$log_transformation, nrow(.)), exp(yhat), yhat),
                    yhat_lower = ifelse(rep(list_params_modelling$log_transformation, nrow(.)), exp(yhat_lower), yhat_lower),
                    yhat_upper = ifelse(rep(list_params_modelling$log_transformation, nrow(.)), exp(yhat_upper), yhat_upper)) %>%
      dplyr::rename(Date = ds)


    ##### outputs

    # If judgemental forecast points are parsed we define them here by parsing these to ypred and y in the calculation of accuracy metrics:
    if(!is.null(judgmental_forecasts.modelling)){

      #Creating vectors of judgmental forecasts:
      adj_judmental_forecasts = c()
      adj_judmental_actual = c()

      for(i in 1:length(judgmental_forecasts.modelling)){
        adj_judmental_forecasts[i] = paste0("yhat = ifelse(as.Date(Date) == as.Date('", names(judgmental_forecasts.modelling[i]), "'), ", judgmental_forecasts.modelling[i], ", yhat)")
        adj_judmental_actual[i] = paste0("target_var = ifelse(as.Date(Date) == as.Date('", names(judgmental_forecasts.modelling[i]), "'), ", judgmental_forecasts.modelling[i], ", target_var)")
      }

      forecast_temp_eval = paste0("forecast_tmp = forecast %>%",
                                  "dplyr::mutate(",
                                  paste0(adj_judmental_forecasts, collapse = ", "), ", ",
                                  paste0(adj_judmental_actual, collapse = ", "),
                                  ") %>%
                                      dplyr::select(Date, yhat, target_var, yhat_lower, yhat_upper, Date)")

      eval(parse(text = forecast_temp_eval))

      y_pred_test = forecast_tmp$yhat[forecast_tmp$Date >= as.Date(min(df_test_modelling$ds))]
      y_true_test = forecast_tmp$target_var[forecast_tmp$Date >= as.Date(min(df_test_modelling$ds))]

      y_pred_train = forecast_tmp$yhat[forecast_tmp$Date < as.Date(min(df_test_modelling$ds))]
      y_true_train = forecast_tmp$target_var[forecast_tmp$Date < as.Date(min(df_test_modelling$ds))]

    }else{

      forecast_tmp = forecast

      y_pred_test = forecast_tmp$yhat[as.Date(forecast_tmp$Date) >= as.Date(min(df_test_modelling$ds))]
      y_true_test = df_test_modelling$target_var

      y_pred_train = forecast_tmp$yhat[as.Date(forecast_tmp$Date) < as.Date(min(df_test_modelling$ds))]
      y_true_train = df_modelling$target_var

    }

    #Creating the forecast vs actuals data-frame:
    holiday_dates = unique(holidays_modelling$ds)

    forecast_tmp = forecast_tmp %>%
      dplyr::mutate(WeekDay = weekdays.Date(Date),
                    Holiday = ifelse(rep(is.null(holidays_modelling), nrow(forecast)), FALSE, (as.Date(Date) %in% as.Date(holiday_dates))),
                    actuals = target_var,
                    diff_abs = abs(actuals - yhat)/actuals,
                    diff = (actuals - yhat)/actuals,
                    changepoint.prior.scale = changepoint.prior.scale.modelling,
                    regressor.prior.scale = regressor.prior.scale.modelling,
                    holidays.prior.scale = holidays.prior.scale.modelling,
                    regressor1 = regressor1.modelling,
                    regressor2 = regressor2.modelling,
                    train = ifelse(as.Date(Date) > max(as.Date(df_modelling$ds)), 0, 1)) %>%
      dplyr::select(Date,
                    regressor1,
                    regressor2,
                    changepoint.prior.scale,
                    regressor.prior.scale,
                    holidays.prior.scale,
                    actuals,
                    yhat,
                    yhat_lower,
                    yhat_upper,
                    diff_abs,
                    diff,
                    WeekDay,
                    Holiday,
                    train)

    return(

      list(forecasts_all = forecast_tmp,
           train_test_actuals_preds = list(y_pred_test = y_pred_test,
                                           y_true_test = y_true_test,
                                           y_pred_train = y_pred_train,
                                           y_true_train = y_true_train),
           model = model
           )

      )

}




