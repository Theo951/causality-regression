

# causality-regression

R Shiny App designed to explore causality and multiple regression.

# Overview

This Causality and Multiple Regression Explorer is a web-based
interactive tool built using R Shiny. The app allows users to explore
relationships between variables, run multiple linear regression models,
and visualize important diagnostic information.

The primary goal of this app is to provide an intuitive way to:

-   Select dependent and independent variables from a dataset.
-   Run multiple regression models.
-   Explore regression diagnostics, including residual plots and
    variable importance.
-   Visualize results to better understand how changes in independent
    variables impact the dependent variable.

This app is designed for educational purposes, especially for those
interested in learning about linear regression, model diagnostics, and
causality in statistical modeling.

# Features

1.  Model Summary: Outputs the regression summary
    generated by the lm() function in R. This includes the coefficients,
    R-squared value, p-values, and other model statistics. How to
    interpret: Coefficients: Represent the effect of each independent
    variable on the dependent variable. P-value: Indicates whether each
    coefficient is statistically significant. R-squared: A measure of
    how well the independent variables explain the variation in the
    dependent variable.
2.  Residuals Plot: A scatter plot of residuals (errors)
    versus fitted values (predicted values). It also includes a dashed
    line at y = 0 for reference. How to interpret: Good fit: Residuals
    should be randomly scattered around the horizontal line at zero. If
    there's a pattern (e.g., a curve), it may indicate that the model is
    not appropriately capturing the data's relationship.
    Homoscedasticity: Residuals should have constant variance; large
    spreads of points could indicate heteroscedasticity (a violation of
    model assumptions).
3.  Diagnostics: A set of diagnostic plots (Q-Q plot,
    residuals vs. leverage, etc.) to help assess the model's
    assumptions. How to interpret: Q-Q Plot: Helps check if residuals
    are normally distributed. Points should fall along the diagonal
    line. Residuals vs. Fitted: Similar to the residual plot, checks for
    patterns in the residuals. Scale-Location: Should show a horizontal
    line with equally spread points; deviations suggest issues like
    heteroscedasticity. Residuals vs. Leverage: Identifies influential
    data points that could be disproportionately affecting the model.
4.  Variable Importance: A horizontal bar plot displaying
    the estimated coefficients of each independent variable in the
    regression model. How to interpret: Variables with larger
    coefficients (in absolute terms) have a larger impact on the
    dependent variable. Positive values indicate a positive
    relationship, and negative values indicate a negative relationship
    with the dependent variable.

# Usage

run the following command: runApp('causality-regression')
