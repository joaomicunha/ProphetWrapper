
#' Accuracies_Agg
#'
#' Function used to explore the accuracy (MAPE, MSE, MAE, RMSE, MPE) for several models simultaneously, choosing the aggregation level
#'
#' @param listProphet_Results A list with outputs from ProphetWrapper function
#' @param interval_agg A character value defining the level to be used to aggregate the series to compute the error metrics. It should be either the same interval of the series or higher.
#' @param results_type A character value defining how to calculate the accuracy metrics (test_set is default).
#'
#' @return This function returns a list with 2 elements:
#'  \itemize{
#'  \item{\strong{Accuracies}}  {A data-frame with the accuracies of the best model for each element parsed in listProphet_Results argument.}
#'  \item{\strong{Graphs_Accuracy_Agg}} {A list with plots of actuals vs forecasts of the best model for each element parsed in listProphet_Results argument.}
#' }
#'
#' @import magrittr
#'


Accuracies_Agg = function(listProphet_Results, interval_agg = "day", results_type = "test_set"){

  ######## Error Handling:
  if(! interval_agg %in% c("sec", "min", "hour", "day", "DSTday", "week", "month", "quarter", "year") | length(interval_agg) != 1){
    stop("Interval_agg should be a single value: sec, min, hour, day, DSTday, week, month, quarter, year")
  }

  if(! results_type %in% c("test_set", "train_set", "all") | length(interval_agg) != 1){
    stop("results_type has to be 'test_set' (test set error), 'train_set' (train set error) or 'all' (both)")
  }

  if(class(listProphet_Results) != "list"){
    stop("listProphet_Results has to be a list object")
  }

  if(sum(sapply(listProphet_Results, class) == "ProphetWrapper") != length(listProphet_Results)){
    stop("listProphet_Results has to be a list consisting of objects with 'ProphetWrapper' class (output of ProphetWrapper::Prophet_Wrapper() function)")
  }


  ######## Define what kind of accuracy analysis the user requires:

  if(results_type == "test_set"){
    filter_exp = "dplyr::filter(train == 0)"
  }else if(results_type == "train_set"){
    filter_exp = "dplyr::filter(train == 1)"
  }else{
    filter_exp = "dplyr::filter(train == 1 | train == 0)"
  }

  ######## Loop over all the ProphetWrapper results parsed:
  final_list = lapply(listProphet_Results, function(x){

    #If the interval required is the same as the current interval dont bother aggregating:
      if(padr::get_interval(x$Actual_vs_Predictions_Best$Date) == interval_agg){

        exp_df = paste0("df = x$Actual_vs_Predictions_Best %>% ", filter_exp, " %>% dplyr::rename(NewDate = Date)")

      }else{

        exp_df = paste0("df = x$Actual_vs_Predictions_Best %>%
              padr::thicken(interval = interval_agg) %>%
              dplyr::select(-Date) %>%",
              filter_exp,
              " %>%
              dplyr::rename('NewDate' = !!names(.[length(.)])) %>%
              dplyr::group_by(NewDate) %>%
              dplyr::summarise_all(funs(sum(.))) %>%
              ungroup()")
      }

    #Create the df:
    eval(parse(text = exp_df))

    #Create accuracy overview tab:
    df_accuracy = data.frame( Model = gsub(pattern = "Actuals vs Forecasts \\(|\\)", replacement = "", x = x$Plot_Actual_Predictions$labels$title),
                       Error_Type = paste0(results_type, " (", interval_agg, ")"),
                       Period_Tested = paste0(min(x$Actual_vs_Predictions_Best$Date[x$Actual_vs_Predictions_Best$train == 0]), " to ", max(x$Actual_vs_Predictions_Best$Date[x$Actual_vs_Predictions_Best$train == 0])),
                       regressor1 = x$Best_Parameters$regressor1,
                       regressor2 = x$Best_Parameters$regressor2,
                       changepoint.prior.scale = x$Best_Parameters$changepoint.prior.scale,
                       seasonality.prior.scale = x$Best_Parameters$seasonality.prior.scale,
                       regressor.prior.scale = x$Best_Parameters$regressor.prior.scale,
                       holidays.prior.scale = x$Best_Parameters$holidays.prior.scale,
                       MAPE = MLmetrics::MAPE(y_pred = sum(df$yhat), y_true = sum(df$actuals)),
                       MSE =  MLmetrics::MSE(y_pred = sum(df$yhat), y_true = sum(df$actuals)),
                       MAE =  MLmetrics::MAE(y_pred = sum(df$yhat), y_true = sum(df$actuals)),
                       RMSE = MLmetrics::RMSE(y_pred = sum(df$yhat), y_true = sum(df$actuals)),
                       MPE = mean((sum(df$actuals) - sum(df$yhat))/sum(df$actuals)),
                       stringsAsFactors = FALSE)

    #Gather the data for the final graph:

    if(padr::get_interval(x$Actual_vs_Predictions_Best$Date) == interval_agg){

      exp_graph = paste0("df_graph = x$Actual_vs_Predictions_Best %>% dplyr::rename(NewDate = Date) %>% tidyr::gather(`Actuals vs Forecast`, Volumes, actuals:yhat)")

    }else{

      exp_graph = paste0("df_graph = x$Actual_vs_Predictions_Best %>%
                      padr::thicken(interval = interval_agg) %>%
                      dplyr::select(-Date) %>%
                      dplyr::rename('NewDate' = !!names(.[length(.)])) %>%
                      dplyr::group_by(NewDate) %>%
                      dplyr::summarise_all(funs(sum(.))) %>%
                      dplyr::ungroup() %>%
                      tidyr::gather(`Actuals vs Forecast`, Volumes, actuals:yhat)")
    }

    eval(parse(text = exp_graph))


    ######### Graph:
    graph_agg = ggplot2::ggplot(data = df_graph, ggplot2::aes(x = as.Date(NewDate), y = Volumes, color = `Actuals vs Forecast`, linetype = `Actuals vs Forecast`)) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(ggplot2::aes(xintercept = as.numeric(min(x$Actual_vs_Predictions_Best$Date[x$Actual_vs_Predictions_Best$train == 0]))), linetype = 4, colour = "black", alpha = 0.8) +
      ggthemes::theme_tufte() +
      ggplot2::scale_y_continuous("\nActuals/Forecasts\n", labels = scales::comma_format()) +
      ggplot2::scale_x_date(name = "\nDate", breaks = scales::date_breaks("2 months")) +
      #ggthemes::scale_color_tableau(palette = 'tableau10medium') +
      ggplot2::ggtitle(label = x$Plot_Actual_Predictions$labels$title, subtitle = x$Plot_Actual_Predictions$labels$subtitle) +
      ggplot2::theme(legend.position = "bottom")


    return(list(Accuracy_individual = df_accuracy,
                Graph_Agg = graph_agg))

    })

  final_list = unlist(final_list, recursive = FALSE)
  accuracies_final = final_list[names(final_list) == "Accuracy_individual"] %>% dplyr::bind_rows()

  list(Accuracies = accuracies_final,
         Graphs_Accuracy_Agg = final_list[names(final_list) == "Graph_Agg"] %>% set_names(accuracies_final$Model))

}


