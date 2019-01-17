
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
#' @param seasonality.prior.scale.modelling parameters from list.params
#' @param regressor.prior.scale.modelling parameters from list.params
#' @param holidays.prior.scale.modelling parameters from list.params
#' @param df_test_modelling test data-frame
#' @param df_train_modelling train data-frame
#' @param df_all_modelling All data
#' @param debug_modelling to debug
#' @param holidays_modelling holidays
#' @param final_predictions_modelling length of final predictions
#'
#' @import magrittr
#'
#'


modelling_prophet_function = function(df_all_modelling, df_test_modelling, df_train_modelling, modelling_type, list_params_modelling, changepoint.prior.scale.modelling, holidays.prior.scale.modelling, regressor.prior.scale.modelling, seasonality.prior.scale.modelling, regressor1.modelling, regressor2.modelling, judgmental_forecasts.modelling, holidays_modelling, final_predictions_modelling, debug_modelling){


    if(debug_modelling){browser()}

    if(modelling_type == 'final'){
      df_modelling = df_all_modelling

          #defaulting the value of final_predictions to the length of test set if NULL is parsed
          if(is.null(final_predictions_modelling)){
            cat(paste0("\nRunning the final optimised model on all available data. Forecast horizon set to ", nrow(df_test_modelling), " (test set length) ...\n"))
            final_predictions_length = nrow(df_test_modelling)
            final_predictions_df = NULL

          }else if(is.integer(final_predictions_modelling) | is.numeric(final_predictions_modelling)){
            cat(paste0("\nRunning the final optimised model on all available data. Forecast horizon set to ", final_predictions_modelling, " ...\n"))
            final_predictions_length = final_predictions_modelling
            final_predictions_df = NULL

          }else if(regressor1.modelling == "no_regressor" & regressor2.modelling == "no_regressor"){
            cat(paste0("\nRunning the final optimised model on all available data. Forecast horizon set to ", nrow(df_test_modelling), " (test set length) ...\n"))
            final_predictions_length = nrow(df_test_modelling)
            final_predictions_df = NULL

          }else{
            futureRegressors_list = FutureRegressors( final_forecasts_list_future_regressors = final_predictions_modelling,
                                                     best_regressor1_future_regressors = regressor1.modelling,
                                                     best_regressor2_future_regressors = regressor2.modelling,
                                                     all_data_future_regressors = df_all_modelling,
                                                     debug_future_regressors = debug_modelling)

            final_predictions_length = futureRegressors_list$future_horizon
            final_predictions_df = futureRegressors_list$future_regressors_df

            cat(paste0("\nRunning the final optimised model on all available data. Forecast horizon set to ",final_predictions_length , " (length of the future forecasts on the ProphetWrapper object parsed) ...\n"))

            }


    }else{
      df_modelling = df_train_modelling
      final_predictions_length = nrow(df_test_modelling)
    }


  holiday_dates = unique(holidays_modelling$ds)



    if(is.null(holidays_modelling)){

      model = prophet::prophet(df = df_modelling,
                               growth = "linear",
                               weekly.seasonality = list_params_modelling$weekly.seasonality,
                               seasonality.prior.scale = seasonality.prior.scale.modelling,
                               changepoint.prior.scale = changepoint.prior.scale.modelling,
                               yearly.seasonality  = list_params_modelling$yearly.seasonality,
                               daily.seasonality = list_params_modelling$daily.seasonality,
                               fit = FALSE)

    }else{

      model = prophet::prophet(df = df_modelling,
                               growth = "linear",
                               weekly.seasonality = list_params_modelling$weekly.seasonality,
                               changepoint.prior.scale = changepoint.prior.scale.modelling,
                               seasonality.prior.scale = seasonality.prior.scale.modelling,
                               yearly.seasonality  = list_params_modelling$yearly.seasonality,
                               daily.seasonality = list_params_modelling$daily.seasonality,
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
    model = tryCatch(
      {
        cat("\nUsing the default Stan optimiser: LBFGS\n")
        prophet::fit.prophet(model, df = df_modelling)
      },
      error = function(cond) {
        cat("\nUsing optimiser algorithm: Newton\n")
        prophet::fit.prophet(model, df = df_modelling, algorithm = 'Newton')
      },
      warning = function(cond) {
        cat("\nUsing optimiser algorithm: Newton\n")
        prophet::fit.prophet(model, df = df_modelling, algorithm = 'Newton')
      })



    future_prophet_complete = prophet::make_future_dataframe(model, periods = final_predictions_length, freq = padr::get_interval(df_all_modelling$ds)) %>%
      dplyr::mutate(ds = as.Date(ds)) %>%
      dplyr::left_join(dplyr::select(df_all_modelling, -ds), by = c("ds" = "Date"))


    tryCatch({
      future_prophet_complete = future_prophet_complete %>%
        dplyr::left_join(final_predictions_df, by = c("ds" = "Date")) %>%
        select(ds, dplyr::contains(".y"))

      colnames(future_prophet_complete) = gsub(x = colnames(future_prophet_complete), pattern = ".y", "")

    },
             error = function(e){
               future_prophet_complete = future_prophet_complete
               })


    forecast_pred <- predict(model, future_prophet_complete)

    forecast = forecast_pred %>%
      dplyr::mutate(ds = as.Date(ds)) %>%
      dplyr::left_join(dplyr::select(df_all_modelling, ds, target_var), by = "ds") %>%
      dplyr::mutate(yhat = ifelse(rep(list_params_modelling$log_transformation, nrow(.)), exp(yhat), yhat),
                    yhat_lower = ifelse(rep(list_params_modelling$log_transformation, nrow(.)), exp(yhat_lower), yhat_lower),
                    yhat_upper = ifelse(rep(list_params_modelling$log_transformation, nrow(.)), exp(yhat_upper), yhat_upper)) %>%
      dplyr::rename(Date = ds)


    ##### outputs

    # If judgemental forecast points are parsed we define them here by parsing these to ypred and y in the calculation of accuracy metrics:
    if(!is.null(judgmental_forecasts.modelling)){

      #hack to avoid calculating the MAPE as NaN. Later we return the original value of judgmental_forecasts:
      judgmental_forecasts_tmp = ifelse(judgmental_forecasts.modelling == 0, 1, judgmental_forecasts.modelling)

      #Creating vectors of judgmental forecasts:
      adj_judmental_forecasts = c()
      adj_judmental_actual = c()
      adj_judmental_lower = c()
      adj_judmental_upper = c()

      for(i in 1:length(judgmental_forecasts.modelling)){
        adj_judmental_forecasts[i] = paste0("yhat = ifelse(as.Date(Date) == as.Date('", names(judgmental_forecasts_tmp[i]), "'), ", judgmental_forecasts_tmp[i], ", yhat)")
        adj_judmental_actual[i] = paste0("target_var = ifelse(as.Date(Date) == as.Date('", names(judgmental_forecasts_tmp[i]), "'), ", judgmental_forecasts_tmp[i], ", target_var)")
        adj_judmental_lower[i] = paste0("yhat_lower = ifelse(as.Date(Date) == as.Date('", names(judgmental_forecasts_tmp[i]), "'), ", judgmental_forecasts_tmp[i], ", yhat_lower)")
        adj_judmental_upper[i] = paste0("yhat_upper = ifelse(as.Date(Date) == as.Date('", names(judgmental_forecasts_tmp[i]), "'), ", judgmental_forecasts_tmp[i], ", yhat_upper)")

      }

      forecast_temp_eval = paste0("forecast_tmp = forecast %>%",
                                  "dplyr::mutate(",
                                  paste0(adj_judmental_forecasts, collapse = ", "), ", ",
                                  paste0(adj_judmental_actual, collapse = ", "), ", ",
                                  paste0(adj_judmental_lower, collapse = ", "), ", ",
                                  paste0(adj_judmental_upper, collapse = ", "),
                                  ") %>%
                                      dplyr::select(Date, yhat, target_var, yhat_lower, yhat_upper, Date)")

      eval(parse(text = forecast_temp_eval))

      y_pred_test = forecast_tmp$yhat[forecast_tmp$Date >= as.Date(min(df_test_modelling$ds))]
      y_true_test = forecast_tmp$target_var[forecast_tmp$Date >= as.Date(min(df_test_modelling$ds))]

      y_pred_train = forecast_tmp$yhat[forecast_tmp$Date < as.Date(min(df_test_modelling$ds))]
      y_true_train = forecast_tmp$target_var[forecast_tmp$Date < as.Date(min(df_test_modelling$ds))]

      #If judgemental forecats are parsed we will use them in the final results:
      judgmental_forecasts_df = data.frame(judgmental_forecasts.modelling) %>%
        dplyr::mutate(Date = as.Date(row.names(.))) %>%
        dplyr::select(Date, judgmental_forecasts.modelling)

      forecast_tmp = forecast_tmp %>%
        dplyr::left_join(judgmental_forecasts_df, by = c("Date")) %>%
        dplyr::mutate(WeekDay = weekdays.Date(Date),
                      Holiday = ifelse(rep(is.null(holidays_modelling), nrow(forecast)), FALSE, (as.Date(Date) %in% as.Date(holiday_dates))),
                      actuals = ifelse(!is.na(judgmental_forecasts.modelling), judgmental_forecasts.modelling, target_var),
                      yhat = ifelse(!is.na(judgmental_forecasts.modelling), judgmental_forecasts.modelling, yhat),
                      yhat_lower = ifelse(!is.na(judgmental_forecasts.modelling), judgmental_forecasts.modelling, yhat_lower),
                      yhat_upper = ifelse(!is.na(judgmental_forecasts.modelling), judgmental_forecasts.modelling, yhat_upper),
                      diff_abs = abs(actuals - yhat)/actuals,
                      diff = (actuals - yhat)/actuals,
                      diff_abs = ifelse(!is.na(judgmental_forecasts.modelling), 0, diff_abs),
                      diff = ifelse(!is.na(judgmental_forecasts.modelling), 0, diff),
                      changepoint.prior.scale = changepoint.prior.scale.modelling,
                      seasonality.prior.scale = seasonality.prior.scale.modelling,
                      regressor.prior.scale = regressor.prior.scale.modelling,
                      holidays.prior.scale = holidays.prior.scale.modelling,
                      regressor1 = regressor1.modelling,
                      regressor2 = regressor2.modelling,
                      train = ifelse(as.Date(Date) > max(as.Date(df_modelling$ds)), 0, 1)) %>%
        dplyr::select(Date,
                      regressor1,
                      regressor2,
                      changepoint.prior.scale,
                      seasonality.prior.scale,
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

    }else{

      forecast_tmp = forecast

      y_pred_test = forecast_tmp$yhat[as.Date(forecast_tmp$Date) >= as.Date(min(df_test_modelling$ds))]
      y_true_test = df_test_modelling$target_var

      y_pred_train = forecast_tmp$yhat[as.Date(forecast_tmp$Date) < as.Date(min(df_test_modelling$ds))]
      y_true_train = df_modelling$target_var

      forecast_tmp = forecast_tmp %>%
        dplyr::mutate(WeekDay = weekdays.Date(Date),
                      Holiday = ifelse(rep(is.null(holidays_modelling), nrow(forecast)), FALSE, (as.Date(Date) %in% as.Date(holiday_dates))),
                      actuals = target_var,
                      diff_abs = abs(actuals - yhat)/actuals,
                      diff = (actuals - yhat)/actuals,
                      changepoint.prior.scale = changepoint.prior.scale.modelling,
                      seasonality.prior.scale = seasonality.prior.scale.modelling,
                      regressor.prior.scale = regressor.prior.scale.modelling,
                      holidays.prior.scale = holidays.prior.scale.modelling,
                      regressor1 = regressor1.modelling,
                      regressor2 = regressor2.modelling,
                      train = ifelse(as.Date(Date) > max(as.Date(df_modelling$ds)), 0, 1)) %>%
        dplyr::select(Date,
                      regressor1,
                      regressor2,
                      changepoint.prior.scale,
                      seasonality.prior.scale,
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

    }


    ### IF modelling_type == 'final' we also want to export the final model and the final prophet graphs:

    if(modelling_type == 'final'){

      plot_components = prophet::prophet_plot_components(m = model,
                                                         fcst = forecast_pred,
                                                         uncertainty = TRUE,
                                                         render_plot = FALSE )

      #Plot Components has to be rendered in a different way because returns a list with graphical elements:
      plot_components_expr = c()

      for(i in 1:length(plot_components)){
        plot_components_expr[i] = paste0("plot_components[[", i, "]]")
      }

      eval(parse(text = paste0("final_grid_components = gridExtra::arrangeGrob(", paste(plot_components_expr, collapse = ", "), ", ncol = 1)")))
      final_plot_components = ggplotify::as.ggplot(final_grid_components)

      plot_changepoints = plot(model, forecast_pred) + prophet::add_changepoints_to_plot(model)


      return(
        list(forecasts_all = forecast_tmp,
             train_test_actuals_preds = list(y_pred_test = y_pred_test,
                                                  y_true_test = y_true_test,
                                                  y_pred_train = y_pred_train,
                                                  y_true_train = y_true_train),
             model = model,
             plot_components = final_plot_components,
             plot_changepoints = plot_changepoints)

        )

      }

    return(

      list(forecasts_all = forecast_tmp,
           train_test_actuals_preds = list(y_pred_test = y_pred_test,
                                           y_true_test = y_true_test,
                                           y_pred_train = y_pred_train,
                                           y_true_train = y_true_train),
           model = model)

      )

}




