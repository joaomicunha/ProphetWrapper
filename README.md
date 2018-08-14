# ProphetWrapper
ProphetWrapper is a package wrapping Facebook's Prophet R Package for Time-Series Forecasting. The main rationale behind the package was to build a reproducible function to train and test several models simultaneously. It allows the user to define a train/test split and evaluate the models produced under different scenarios with several error-metrics exporting diagnostics and visualisations.

For more background on Prophet please consider the resources below. The documentation for Prophet is very rich and detailed information about the model and parameters used can be found here:

- [Prophet: forecasting at scale](https://research.fb.com/prophet-forecasting-at-scale/ "Prophet: forecasting at scale")
- [Quick Start Prophet](https://facebook.github.io/prophet/docs/quick_start.html "Quick Start Prophet")
- [Forecasting at Scale (Prophet White Paper)](https://peerj.com/preprints/3190/ "Forecasting at Scale (Prophet White Paper)")


## How to use it

Below you can find a simple example of how to explore the functionality of ProphetWrapper. The package contains only one exported function ProphetWrapper::Prophet_Wrapper().


```{r eval=FALSE}

library(prophet)

parameters_model = list(changepoint.prior.scale =  c(0.001, 0.003, 0.005, 0.01, 0.03, 0.05, 0.08, 0.1, 0.15),
                        holidays.prior.scale = c(1, 3, 6, 10, 13, 15, 25),
                        regressor.prior.scale = c(0.05),
                        weekly.seasonality = TRUE,
                        yearly.seasonality = TRUE,
                        standardize_regressor = TRUE,
                        log_transformation = TRUE,
                        target_variable = "sessions",
                        regressor1 = c("no_regressor"),
                        regressor2 = c("no_regressor"))

prophetWrapper_output = ProphetWrapper::Prophet_Wrapper(df = df_model,
                                                        list_params = parameters_model,
                                                        best_model_in = "cv",
                                                        main_accuracy_metric = "MAPE",
                                                        plotFrom = "2017-01-01",
                                                        seed = 11111,
                                                        debug = FALSE)
```

The sessions dataset is a time-series data-frame with the daily volumes of sessions for an imaginary website between 2013-01-01 and 2017-01-09.
In the example above, we first create a 'parameters_model' list with some important options/parameters to be parsed to the Prophet_Wrapper function. changepoint.prior.scale, regressor.prior.scale, holidays.prior.scale, regressor1 and regressor2 will be treated as a grid of parameters that is going to be iterated over the modeling process. target_variable defines the target_variable whilst log_transformation and standardize_regressor control transformations to be applied to the target variable and/or regressors respectively.

The example runs 63 models (9 changepoint.prior.scale times 7 holidays.prior.scale times 1 regressor.prior.scale values) with weekly and yearly seasonality and the target variable is log transformed. The current example doesn't use external regressors but it's possible to iterate over two groups of regressors (regressor1 and regressor2).

The best model is selected based on the MAPE ('main_accuracy_metric' parameter) on a cross-validation framework ('best_model_in' parameter). The models can also be evaluated based on test set accuracy, train set accuracy or 'mix_train_test' (an option where the user can identify best models based on the performance on portions of test and train set).

There is also the possibility to evaluate the models based on 'judgmental forecasts'. Sometimes the user has business knowledge and certainty about the future value of the target variable on a specific period. As an example, when predicting sales from a particular group of stores, the user might know that on the Xmas day there is no sales since the shops are closed. My parsing a named vector with dates and values, this prior knowledge can be taken into account when evaluating the models. The named vector to be parsed as judgmental_forecasts should have the format c('2016-12-25' = 1,  '2017-12-25' = 1, '2018-12-25' = 1).

The parameter 'debug' when set to TRUE can be used for debugging and the 'parallel' arguments defines if the user wants to run the models in parallel (using all minus one cores).


The output of Prophet_Wrapper is an R list containing:

- **Accuracy_Overview:** A data-frame with the performance of all the models estimated with a flag for the best model (based on 'main_accuracy_metric' and 'best_model_in' parameters). The error metrics available are: MAPE, MSE, MAE, RMSE and MPE.

- **Actuals_vs_Predictions_All:** A data-frame with point predictions vs actuals, upper and lower bound predictions as per Prophet and other details about the series and the models (for train and test set). There is a row per date and model trained pair (i.e. if n models are trained this data-frame returns n rows for each of the date points).

- **Actual_vs_Predictions_Best:** A data-frame with the predictions of the best model (based on 'main_accuracy_metric' and 'best_model_in' parameters). Actuals, predictions, upper and lower bound predictions are included as well as a 'prediction_and_predictor' field wich contains actuals from the train set and predictions from the test set (useful when stacked models are used). 

- **Best_Parameters:** A data-frame with the best parameters used to estimate the best model (based on 'main_accuracy_metric' and 'best_model_in' parameters).

- **Plot_Actual_Predictions:** A ggplot of Actuals vs Predictions. The 'plotFrom' parameter controls from when to plot from.


