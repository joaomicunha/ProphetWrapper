
#' ValidateArguments
#'
#' Function used to validate the arguments parsed to ProphetWrapper:
#'
#' @param df A data-frame original
#' @param list_params list of params
#' @param best_model_in best_model_in
#' @param plotFrom plotFrom
#' @param holidays holidays df
#' @param main_accuracy_metric main_accuracy_metric
#' @param train_set_imp_perc train_set_imp_perc
#' @param judgmental_forecasts judgmental_forecasts
#' @param final_predictions final_predictions
#' @param testing_period testing_period
#' @param debug debugger
#'
#' @import magrittr
#'
#'


ValidateArguments = function(df, list_params, best_model_in, plotFrom, main_accuracy_metric, train_set_imp_perc, judgmental_forecasts, final_predictions, holidays, testing_period, debug = FALSE){

  if(debug){browser()}

  #~~~ ERROR CONTROLS =================================================================

  is.date <- function(x) inherits(x, 'Date')


  if(is.null(df)){
    stop("No df provided ('df').")
  }

  if( !("data.frame" %in% class(df))){
    stop("The object parsed as 'df' is not a data.frame.")
  }

  if(ncol(df) < 2){
    stop('The df object has to include at least 2 columns (a target_var and a date variable).')
  }

  tryCatch({holidays}, error = function(e){stop(paste0("The 'holidays' argument parsed doesn't exist in memory."))})

  if(sum(c("holiday", "ds") %in% colnames(holidays)) <2 & !is.null(holidays)){
    stop("'holidays' data frame has to contain columns holiday (character) and ds (date type) and optionally columns lower_window and upper_window which specify a range of days around the date to be included as holidays. ")
  }

  if(sum(sapply(df, is.numeric)) == 0){
    stop("No numeric/integer type column was identified in 'df'. Please include a numeric/integer and a date column to continue.")
  }

  if(sum(sapply(df, is.date)) != 1){
    stop("No Date type column was identified in 'df'. Please include a numeric and a date column to continue.")
  }

  if(is.null(testing_period) || (testing_period == 0 | (!is.integer(testing_period) & !is.numeric(testing_period)))){
    stop("Please specify a 'testing_period' higher than 0 as an integer value.")
  }

  if(is.null(list_params$target_var)){
    stop("Please specify the 'target_var' argument.")
  }

  #~~~ Clean the main data-frame (df) =================================================================

  vars = c(target_var = list_params$target_variable)

  original = df %>%
    dplyr::rename_if(is.date ,
                     dplyr::funs(sub(., 'Date', .))) %>%
    dplyr::rename(target_var = !!vars) %>%
    dplyr::mutate(rowNumber = dplyr::row_number(),
                  train = ifelse(rowNumber > max(rowNumber) - round(testing_period), 0, 1)) %>%
    dplyr::select(-rowNumber)


  if(!is.null(plotFrom)){
    if(as.Date(plotFrom) > max(original$Date) | as.Date(plotFrom) < min(original$Date)){
      stop("The argument 'plotFrom' date is out-of-bounds.")
    }
  }

  if(best_model_in != "train" & best_model_in != "test" & best_model_in != "mix_train_test" & best_model_in != "cv"){
    stop("The 'best_model_in' argument has to be either 'train', 'test', 'mix_train_test' or 'cv'.")
  }

  if(train_set_imp_perc > 1  & train_set_imp_perc <0 ){
    stop("The 'train_set_imp_perc' argument has to have a value between 0 and 1.")
  }

  if(main_accuracy_metric != "MAPE" & main_accuracy_metric != "MSE" & main_accuracy_metric != "MAE" & main_accuracy_metric != "RMSE" & main_accuracy_metric != "MPE"){
    stop("The 'main_accuracy_metric' argument has to be either MAPE, MSE, MAE, RMSE or MPE (it defaults to MAPE).")
  }

  if(!is.null(list_params$regressor1)){

    if(class(list_params$regressor1) == "list"){stop("list_params$regressor1 has to be a character vector")}

    length_reg1 = length(list_params$regressor1)

    for(i in 1:length_reg1){

      if(list_params$regressor1[i] != "no_regressor"){
        tryCatch({df[,list_params$regressor1[i]]}, error = function(e){stop(paste0("The Regressor ", list_params$regressor1[i], " is not in df."))})

      }else{next()}

    }
  }

  tryCatch({df[,list_params$target_variable]}, error = function(e){stop(paste0("The Target Variable ", list_params$target_variable, " is not in df."))})


  if(!is.null(list_params$regressor2)){

    if(class(list_params$regressor2) == "list"){stop("list_params$regressor2 has to be a character vector")}

    length_reg2 = length(list_params$regressor2)

    for(i in 1:length_reg2){

      if(list_params$regressor2[i] != "no_regressor"){
        tryCatch({df[,list_params$regressor2[i]]}, error = function(e){stop(paste0("The Regressor ", list_params$regressor2[i], " is not in df."))})

      }

    }
  }


  if(!is.null(list_params$weekly.seasonality) && (list_params$weekly.seasonality != FALSE & list_params$weekly.seasonality != TRUE & !is.numeric(list_params$weekly.seasonality) & list_params$weekly.seasonality != 'auto')){
    stop("The 'list_params$weekly.seasonality' argument has to be a bolean (TRUE or FALSE) or a number representing the Fourier terms to generate or 'auto'.")
  }

  if(!is.null(list_params$yearly.seasonality) && (list_params$yearly.seasonality != FALSE & list_params$yearly.seasonality != TRUE & !is.numeric(list_params$yearly.seasonality) & list_params$yearly.seasonality != 'auto')){
    stop("The 'list_params$yearly.seasonality' argument has to be a bolean (TRUE or FALSE) or a number representing the Fourier terms to generate or 'auto'.")
  }

  if(!is.null(list_params$daily.seasonality) && (list_params$daily.seasonality != FALSE & list_params$daily.seasonality != TRUE & !is.numeric(list_params$daily.seasonality) & list_params$daily.seasonality != 'auto' )){
    stop("The 'list_params$daily.seasonality' argument has to be a bolean (TRUE or FALSE) or a number representing the Fourier terms to generate or 'auto'.")
  }

  if(!is.null(list_params$standardize_regressor) && (list_params$standardize_regressor != FALSE & list_params$standardize_regressor != TRUE & list_params$standardize_regressor != "auto" )){
    stop("The 'list_params$standardize_regressor' argument has to be a bolean (TRUE or FALSE) or 'auto'.")
  }

  if(!is.null(list_params$log_transformation) && (list_params$log_transformation != FALSE & list_params$log_transformation != TRUE )){
    stop("The 'list_params$log_transformation' argument has to be a bolean (TRUE or FALSE).")
  }

  if(class(list_params$changepoint.prior.scale) != "numeric" & !is.null(list_params$changepoint.prior.scale)){
    stop("The 'list_params$changepoint.prior.scale' argument has to be a numeric vector.")
  }

  if(!is.null(list_params$seasonality.prior.scale) && class(list_params$seasonality.prior.scale) != "numeric"){
    stop("The 'list_params$seasonality.prior.scale' argument has to be a numeric vector.")
  }

  if(!is.null(list_params$regressor.prior.scale) && class(list_params$regressor.prior.scale) != "numeric"){
    stop("The 'list_params$regressor.prior.scale' argument has to be a numeric vector.")
  }

  if(!is.null(list_params$holidays.prior.scale) && class(list_params$holidays.prior.scale) != "numeric"){
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

  if((class(final_predictions) == "numeric" | class(final_predictions) == "integer") & length(final_predictions) >1){
    stop("'final_predictions' once set to numeric/integer will define the final forecast horizon. So it has to be length 1.")
  }

  if(class(final_predictions) == "data.frame"){

    if(! "Date" %in% colnames(final_predictions)){
      stop("'final_predictions' once set to data.frame it has to contain a column Date (class Date).")

    }else if(class(final_predictions$Date) != "Date"){
     stop("final_predictions$Date column has to be class Date.")

    }else if(!is.null(list_params$regressor1) && (list_params$regressor1 != "no_regressor" & !(list_params$regressor1 %in% colnames(final_predictions))   )){
      stop(paste0(list_params$regressor1, " is not included in the df final_predictions"))

    }else if(!is.null(list_params$regressor2) && (list_params$regressor2 != "no_regressor" & !(list_params$regressor2 %in% colnames(final_predictions))   )){
      stop(paste0(list_params$regressor2, " is not included in the df final_predictions"))
    }

  }

  if(class(final_predictions) == "ProphetWrapper"){
    if(is.null(final_predictions$Final_Forecasts)){
      stop("Not possible to use a 'ProphetWrapper' object WITHOUT Final_Forecasts on the 'final_predictions' argument. final_predicitons should only be populated with ProphetWrapper object if regressors are being tested.")
    }else if((!is.null(list_params$regressor1) && list_params$regressor1 != "no_regressor") &  (!is.null(list_params$regressor2) && list_params$regressor2 != "no_regressor") ){
      stop("Only 1 ProphetWrapper object was parsed but 2 regressors are used. Please consider either parsing a list with two ProphetWrapper objects or a data-frame of future regressors.")
    }
  }

  if(length(final_predictions) == 2 & is.null(names(final_predictions))){
    stop("'final_predictions' should be a named list with 'regressor1' as first element and 'regressor2' as second (if a list length 2 is parsed)")
  }

  if(class(final_predictions) == "ProphetWrapper"){
    if(sum(unique(original$Date) %in% unique(final_predictions$Final_Forecasts$Date)) != length(unique(original$Date))){
      stop("The 'ProphetWrapper' object parsed as final_predictions doesn't contain all the dates from the parsed data-frame meaning it can't be used to make final future forecasts.")
    }
  }

  if(class(final_predictions) == "list" & length(final_predictions) >2){
    stop("A max of two regressors are used in Prophet. final_predictions (if set to a list) has to be length two or lower.")
  }

  if(sum(sapply(final_predictions, class) == "ProphetWrapper") == length(final_predictions) & !is.null(final_predictions)){

    if(sum(sapply(final_predictions, function(x){is.null(x$Final_Forecasts)})) > 0){
      stop("Input of FutureRegressors not in the right format. The ProphetWrapper objects parsed to final_forecasts_list_future_regressors don't contain 'Final_Forecasts'. Try to input a data.frame with Date and future regressor values alternatively.")
    }

    if(nrow(final_predictions[[1]]$Final_Forecasts) != nrow(final_predictions[[2]]$Final_Forecasts)){
      stop("The Final_Forecasts from the two ProphetWrapper objects parsed to 'final_predictions' argument don't have the same length.")
    }

  }

  #Return the original df if all the errors pass:
  return(original)

}




