

#' FutureRegressors:
#'
#' This is an internal function which unique purpose is to prepare future regressors (not exported).
#'
#' @param final_forecasts_list_future_regressors List with either ProphetWrapper objects or future regressors and Date
#' @param best_regressor1_future_regressors Name of the best regressor 1
#' @param best_regressor2_future_regressors Name of the best regressor 2
#' @param all_data_future_regressors All data available
#' @param debug_future_regressors Debug
#'
#' @import magrittr
#'
#'


FutureRegressors = function(final_forecasts_list_future_regressors, best_regressor1_future_regressors, best_regressor2_future_regressors, all_data_future_regressors, debug_future_regressors){

  #FutureRegressors takes and produces regressors in the following format:

  #   a) List with two ProphetWrapper Objects
  #   b) Data-Frame with Date and Regressors future values
  #   c) Single ProphetWrapper Object



  #Debug:
  if(debug_future_regressors){browser()}

  #Field formatting and error handling:
  if(length(final_forecasts_list_future_regressors) >2){
    stop("A max of two regressors is used in Prophet. final_forecasts_list has to be length two or lower.")
  }else if(is.null(final_forecasts_list_future_regressors)){
    break()
  }

  if(length(final_forecasts_list_future_regressors) == 2){

      if(class(final_forecasts_list_future_regressors[[1]]) == "ProphetWrapper" & class(final_forecasts_list_future_regressors[[2]]) == "ProphetWrapper"){

        if(is.null(final_forecasts_list_future_regressors[[1]]$Final_Forecasts) | is.null(final_forecasts_list_future_regressors[[2]]$Final_Forecasts)){
          stop("Input of FutureRegressors not in the right format. The ProphetWrapper objects parsed to final_forecasts_list_future_regressors don't contain 'Final_Forecasts'. Try to input a data.frame with Date and future regressor values.")
        }

         final_predictions = final_forecasts_list_future_regressors$regressor1$Final_Forecasts %>%
          dplyr::mutate(!!best_regressor1_future_regressors := dplyr::coalesce(actuals, yhat)) %>%
          dplyr::select(Date, dplyr::matches(best_regressor1_future_regressors))

          final_predictions2 = final_forecasts_list_future_regressors$regressor2$Final_Forecasts %>%
            mutate(!!best_regressor2_future_regressors := dplyr::coalesce(actuals, yhat)) %>%
            select(Date, dplyr::matches(best_regressor2_future_regressors))

          if(nrow(final_predictions) != nrow(final_predictions2)){
            stop("The Final_Forecasts from the two ProphetWrapper objects parsed don't have the same length.")
          }

          final_predictions = cbind(final_predictions, final_predictions2[,2, drop=FALSE])

          list_final_forecast_length = lapply(final_forecasts_list_future_regressors, function(x){
            x$Final_Forecasts %>%
              dplyr::filter(Date > max(all_data_future_regressors$ds)) %>%
              nrow()
            })

          if(list_final_forecast_length[[1]] != list_final_forecast_length[[2]]){
            stop("The two ProphetWrapper objects parsed have different length future forecasts (and therefore different length regressors).")
          }else{
            future_forecast_length = list_final_forecast_length[[1]]
          }

      }else{
        stop("Input of FutureRegressors not in the right format")
      }


  }else{

      if(class(final_forecasts_list_future_regressors[[1]]) == "ProphetWrapper"){


        if(is.null(final_forecasts_list_future_regressors[[1]]$Final_Forecasts)){
          stop("Input of FutureRegressors not in the right format. The ProphetWrapper objects parsed to final_forecasts_list_future_regressors don't contain 'Final_Forecasts'. Try to input a data.frame with Date and future regressor values.")
        }

        final_predictions = final_forecasts_list_future_regressors[[1]]$Final_Forecasts %>%
          dplyr::mutate(!!best_regressor1_future_regressors := dplyr::coalesce(actuals, yhat)) %>%
          dplyr::select(Date, dplyr::matches(best_regressor1_future_regressors))

        future_forecast_length = final_forecasts_list_future_regressors[[1]]$Final_Forecasts %>%
            dplyr::filter(Date > max(all_data_future_regressors$ds)) %>%
            nrow()

      }else if(sum(class(final_forecasts_list_future_regressors[[1]]) %in% "data.frame") == 1){

        best_regressors = ifelse(length(final_forecasts_list_future_regressors[[1]]) == 3, paste(best_regressor1_future_regressors, best_regressor2_future_regressors, sep = "|"), best_regressor1_future_regressors)
        future_forecast_length = nrow(final_forecasts_list_future_regressors[[1]])



        tryCatch({

          is.date <- function(x) inherits(x, 'Date')

          final_predictions = all_data_future_regressors %>%
            dplyr::select(-ds) %>%
            dplyr::rename_if(is.date ,
                             dplyr::funs(sub(., 'Date', .))) %>%
            dplyr::select(Date, dplyr::matches(best_regressors)) %>%
            dplyr::bind_rows(final_forecasts_list_future_regressors[[1]])

          }, error = function(e){
            stop(paste0("The data.frame parsed as final_predictions doesn't contain the select regressors: ", best_regressor1_future_regressors, " and/or ", best_regressor2_future_regressors))
          })

        #Check if the future regressors parsed are complete and plug into the actuals well:

        if(nrow(final_predictions) < nrow(padr::pad(final_predictions))){
          stop("The future regressors parsed in 'final_predictions' are not a date continuation of the actual data.")
        }else if(sum(sapply(final_predictions, function(x){sum(is.na(x))})) > 0){
          stop("The future regressors parsed in 'final_predictions' contain NAs.")
        }else if(final_predictions %>% dplyr::group_by(Date) %>% dplyr::summarise(n = n()) %>% dplyr::ungroup() %>% dplyr::filter(n>1) %>% nrow() > 0){
          stop("The dates parsed in 'final_predictions' are duplicated with some dates from actuals.")
          }

      }

  }

  return(list(future_regressors_df = final_predictions,
              future_horizon = future_forecast_length))

}
