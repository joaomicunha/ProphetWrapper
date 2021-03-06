# ProphetWrapper
ProphetWrapper is a package wrapping Facebook's Prophet R Package for Time-Series Forecasting. The main rationale behind the package was to build a reproducible function to train and test several models simultaneously. It allows the user to define a train/test split and evaluate the models produced under different scenarios with several error-metrics exporting diagnostics and visualisations.

For more background on Prophet please consider the resources below. The documentation for Prophet is very rich and detailed information about the model and parameters used can be found here:

- [Prophet: forecasting at scale](https://research.fb.com/prophet-forecasting-at-scale/ "Prophet: forecasting at scale")
- [Quick Start Prophet](https://facebook.github.io/prophet/docs/quick_start.html "Quick Start Prophet")
- [Forecasting at Scale (Prophet White Paper)](https://peerj.com/preprints/3190/ "Forecasting at Scale (Prophet White Paper)")


## How to use it

Below you can find a simple example of how to explore the functionality of ProphetWrapper. The package contains two exported functions:

- ProphetWrapper::Prophet_Wrapper()
- ProphetWrapper::Accuracies_Agg()

```{r eval=FALSE}

library(prophet)
library(ProphetWrapper)

parameters_models = list( changepoint.prior.scale =  c(0.001, 0.01),
                                       holidays.prior.scale = c(1, 25),
                                       seasonality.prior.scale = c(10, 15),
                                       regressor.prior.scale = c(0.05),
                                       weekly.seasonality = TRUE,
                                       yearly.seasonality = TRUE,
                                       daily.seasonality = TRUE,
                                       standardize_regressor = TRUE,
                                       log_transformation = TRUE,
                                       target_variable = "sessions",
                                       regressor1 = c("no_regressor"),
                                       regressor2 = c("no_regressor"))


sessions_prophet_wrapper = ProphetWrapper::Prophet_Wrapper(  df = ProphetWrapper::sessions,
                                                                 list_params = parameters_models,
                                                                 best_model_in = "test",
                                                                 main_accuracy_metric = "MAPE",
                                                                 final_predictions = 20,
                                                                 testing_period = 20,
                                                                 plotFrom = "2017-01-01",
                                                                 seed = 9999,
                                                                 debug = FALSE,
                                                                 parallel = FALSE)


#To explore Acutals vs Predictions 
sessions_prophet_wrapper$Plot_Actual_Predictions

#To explore the Final Forecasts (in this case 20 periods horizon):
sessions_prophet_wrapper$Final_Forecasts

#To explore the Accuracy of all the Models trained accross different error metrics:
sessions_prophet_wrapper$Accuracy_Overview

#To explore the predictions for all the models:
sessions_prophet_wrapper$Actuals_vs_Predictions_All

#The results of the best model predicitions (on test set):
sessions_prophet_wrapper$Actual_vs_Predictions_Best

#The best hyperparameters selected from all the models trained in terms of 'main_accuracy_metric':
sessions_prophet_wrapper$Best_Parameters

```

The sessions dataset is a time-series data-frame with the daily volumes of sessions for an imaginary website between 2013-01-01 and 2017-01-09.
In the example above, we first create a 'parameters_model' list with some important options/parameters to be parsed to the Prophet_Wrapper function. changepoint.prior.scale, regressor.prior.scale, holidays.prior.scale, regressor1 and regressor2 will be treated as a grid of parameters that is going to be iterated over the modeling process. target_variable defines the target_variable whilst log_transformation and standardize_regressor control transformations to be applied to the target variable and/or regressors respectively.

The example runs 63 models (9 changepoint.prior.scale times 7 holidays.prior.scale times 1 regressor.prior.scale values) with weekly and yearly seasonality and the target variable is log transformed. The current example doesn't use external regressors but it's possible to iterate over two groups of regressors (regressor1 and regressor2).

The best model is selected based on the MAPE ('main_accuracy_metric' parameter) on a cross-validation framework ('best_model_in' parameter). The models can also be evaluated based on test set accuracy, train set accuracy or 'mix_train_test' (an option where the user can identify best models based on the performance on portions of test and train set).

The parameter 'debug' when set to TRUE can be used for debugging and the 'parallel' arguments defines if the user wants to run the models in parallel (using all minus one cores).

### Padding and NA imputation

ProphetWrapper will automatically detect the frequency of the data and pad the series accordingly. The method to impute the padded or missing series values can be controlled by the user. 'method_impute' parameter selects the moving average method to be used for imputation ('simple', 'linear' or 'exponential') whilst k_impute can be used to define the length of the moving average (defaults to 4).


### Judgmental Forecasts

There is also the possibility to evaluate the models based on 'judgmental forecasts'. Sometimes the user has business knowledge and certainty about the future value of the target variable on a specific period. As an example, when predicting sales from a particular group of stores, the user might know that on the Xmas day there is no sales since the shops are closed. By parsing a named vector with dates and values, this prior knowledge can be taken into account when evaluating the models. The named vector to be parsed as judgmental_forecasts should have the format c('2016-12-25' = 0,  '2017-12-25' = 0, '2018-12-25' = 0).

### Holidays

Holidays can be parsed in the same way as in prophet package ([see here](https://facebook.github.io/prophet/docs/seasonality,_holiday_effects,_and_regressors.html "see here")). 
On the context of ProhetWrapper these are parsed by creating a data-frame (example below) and parsing it to 'holidays' argument.

```{r eval=FALSE}

xmas_days = data_frame(
  holiday = 'xmas_days',
  ds = as.Date(c('2016-12-25', '2017-12-25', '2018-12-25')),
  lower_window = -1,
  upper_window = 0
)


```

### The output of Prophet_Wrapper is an R list with class 'ProphetWrapper' containing:

- **Final_Forecasts:** A data-frame with final forecasts on unseen data produced by training the optimised model (based on 'main_accuracy_metric' and 'best_model_in' parameters) on the entire data. The user can use the 'final_predictions' parameter to control the horizon of the final forecasts (more detail on the function documentation). ProphetWrapper class object can be parsed directly to 'final_predictions' which reveals to be useful when using 'stacked' modelling. The regressors used are the Final_Forecasts from the ProphetWrapper object parsed and the length/horizon of the final forecasts is the number of future rows in Final_Forecasts. The forecast length (horizon) can also be set to be equal to the length of the test set (when 'final_predictions' is NULL) or to a specified integer.

- **Accuracy_Overview:** A data-frame with the performance of all the models estimated with a flag for the best model (based on 'main_accuracy_metric' and 'best_model_in' parameters). The error metrics available are: MAPE, MSE, MAE, RMSE and MPE.

- **Actuals_vs_Predictions_All:** A data-frame with point predictions vs actuals, upper and lower bound predictions as per Prophet and other details about the series and the models (for train and test set). There is a row per date and model trained pair (i.e. if n models are trained this data-frame returns n rows for each of the date points).

- **Actual_vs_Predictions_Best:** A data-frame with the predictions of the best model (selected based on 'main_accuracy_metric' and 'best_model_in' parameters). Actuals, predictions, upper and lower bound predictions are included as well as a 'prediction_and_predictor' field wich contains actuals from the train set and predictions from the test set (useful when stacked models are used). 

- **Best_Parameters:** A data-frame with the best parameters used to estimate the best model (selected based on 'main_accuracy_metric' and 'best_model_in' parameters).

- **CV_Overview:** A data-frame with an overview of the in-fold accuracy performance for the best model (selected based on 'main_accuracy_metric' and 'best_model_in' parameters). This output is only made available if best_model_in parameter is set to 'cv'.

- **Final_Prophet_Model:** The Prophet model generated on all available data for the final future forecasts. This output is only made available when it is possible to generate Final_Forecasts.

- **Plots:** A list with several visualisations of the results including:

    - **Plot_Actual_Predictions_Best:** A ggplot graph of Actuals vs Predictions of the best model with train/test split. The 'plotFrom' parameter controls from when to plot from.

    - **Plot_Point_Accuracies:** A ggplot graph with pointwise Absolute Percentage Difference of Actuals vs Forecasts

    - **Plot_Actual_Predictions_Final:** A ggplot graph of Actuals vs Predictions of the final future forecasts. This output is only made available when it is possible to generate Final_Forecasts.

    - **Plot_Final_Model_Components:** Plot the components of the final Prophet forecast. A ggplot2 with the elements that are available of: trend, holidays, weekly seasonality, yearly seasonality, and additive and multiplicative extra regressors. This output is only made available when it is possible to generate Final_Forecasts.

    - **Plot_Final_Model_Changepoints:** Plot of the changepoints of the final Prophet forecast. This output is only made available when it is possible to generate Final_Forecasts.

    - **Plot_CV_Accuracy:** A ggplot graph illustrating the performance on cross-validation for different horizon (look-ahead) periods. This output is only made available if best_model_in parameter is set to 'cv'.


### Accuracies_Agg Function:

Sometimes is useful to report/analyse the accuracy of several models at once and/or at different levels of aggregation (i.e. the forecasts were produced on a daily level but what is the accuracy of the monhtly aggregations). Accuracies_Agg function exported from ProphetWrapper can be used for this user cases. It takes a list of class 'ProphetWrapper' objects (output of prophet_wrapper function) and it outputs a list containing:

- **Accuracies:** A data-frame with the accuracies of the best model for each element parsed in listProphet_Results argument.

- **Graphs_Accuracy_Agg:** A list with plots of actuals vs forecasts of the best model for each element parsed in 'listProphet_Results' argument.

Please refer to the ProphetWrapper::Accuracies_Agg() documentation for more details.



