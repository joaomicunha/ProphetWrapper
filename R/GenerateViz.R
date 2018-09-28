


#' GenerateViz
#'
#' Function to generate visualisations:
#'
#' @param df_best_model_viz Best model post testing
#' @param Final_Forecasts_viz final future forecasts
#' @param list_params_viz list of parameters
#' @param plotFrom_viz plotFrom variable
#' @param debug_viz debug
#'
#' @import magrittr
#'
#'



GenerateViz = function(df_best_model_viz, Final_Forecasts_viz, list_params_viz, plotFrom_viz, debug_viz){

  df_graph1 = df_best_model_viz %>%
    dplyr::filter(as.Date(Date) >= as.Date(plotFrom_viz)) %>%
    tidyr::gather(`Actuals vs Forecasts`, Volumes, actuals:yhat)

  df_graph2 = df_best_model_viz %>%
    dplyr::mutate(diff_abs = abs(actuals - yhat)/actuals,
           label = ifelse(diff_abs > 0.5, as.character(Date), NA ))


  #Graph 1 - Actuals vs Forecasts best model with a vertical line separating test and train sets:
  graph1 = ggplot2::ggplot(data = df_graph1, ggplot2::aes(x = as.Date(Date), y = Volumes, color = `Actuals vs Forecasts`, linetype = `Actuals vs Forecasts`)) +
            ggplot2::geom_line() +
            ggplot2::geom_vline(ggplot2::aes(xintercept = as.numeric(min(df_graph2$Date[df_graph2$train == 0]))), linetype = 4, colour = "#40dfad", alpha = 0.7) +
            ggthemes::theme_tufte() +
            ggplot2::scale_y_continuous("\nActuals/Forecasts\n", labels = scales::comma_format()) +
            ggplot2::scale_x_date(name = "\nDate", breaks = scales::date_breaks("2 months")) +
            #ggthemes::scale_color_tableau(palette = 'tableau10medium') +
            ggplot2::ggtitle(label = paste0("Actuals vs Forecasts (", list_params_viz$target_variable, ")"), subtitle = paste0("From: ", min(df_graph1$Date), " To: ", max(df_graph1$Date), " (test set from ", min(df_graph1$Date[df_graph1$train == 0]), " onwards)")) +
            ggplot2::theme(legend.position = "bottom")

  #Graph 2 - Pointwise Absolute % Difference Actuals and Forecasts:
  graph2 = ggplot2::ggplot(data = df_graph2, aes(x = Date, y = diff_abs, color = diff_abs, label = label)) +
            geom_point() +
            geom_text(aes(label=label), hjust=0, vjust=0, size = 2) +
            ggplot2::geom_vline(ggplot2::aes(xintercept = as.numeric(min(df_graph2$Date[df_graph2$train == 0]))), linetype = 4, colour = "#40dfad", alpha = 0.7) +
            ggthemes::theme_tufte() +
            ggplot2::scale_y_continuous("\nAbsolute % Difference Actuals vs Forecasts\n", labels = scales::percent_format()) +
            ggplot2::scale_x_date(name = "\nDate", breaks = scales::date_breaks("2 months")) +
            ggplot2::ggtitle(label = paste0("Pointwise Absolute % Difference Actuals vs Forecasts (", list_params_viz$target_variable, ")"), subtitle = paste0("From: ", min(df_graph2$Date), " To: ", max(df_graph2$Date), " (test set from ", min(df_graph2$Date[df_graph2$train == 0]), " onwards)")) +
            ggplot2::theme(legend.position = "bottom")


  if(!is.null(Final_Forecasts_viz)){

    #Graph 3 - for the final forecasts:
    df_graph3 = Final_Forecasts_viz %>%
      dplyr::select(Date, actuals, yhat) %>%
      tidyr::gather(`Actuals vs Forecasts`, Volumes, actuals:yhat)

    graph_final_predictions = ggplot2::ggplot(df_graph3, ggplot2::aes(x = Date, y = Volumes, color = `Actuals vs Forecasts`, linetype = `Actuals vs Forecasts`)) +
      ggplot2::geom_line() +
      ggthemes::theme_tufte() +
      ggplot2::scale_y_continuous(labels = scales::comma_format(), breaks = scales::pretty_breaks(5)) +
      ggplot2::scale_x_date(labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("2 months")) +
      ggplot2::ggtitle(label = paste0("Actuals vs Predictions Future Forecasts (", list_params_viz$target_variable, ")")) +
      ggplot2::theme(legend.position = "bottom")

  }else{
    graph_final_predictions = NULL
    }


  #Final Results:
   final_graphs_list = list( Plot_Actual_Predictions_Best = graph1,
                             Plot_Point_Accuracies = graph2,
                             Plot_Actual_Predictions_Final = graph_final_predictions)

   return(final_graphs_list[!sapply(final_graphs_list, is.null)])

}



