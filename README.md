# A Price Prediction Model for Listings on Airbnb
## Files in this repository 
1. airbnb.csv: Please refer to the dataset section below for more information.
2. airbnb.R: This is the R script file used for analysis.
3. "London_Sport": This folder stores information for Geographic Information Visualisation.

## Dataset
The data is collected from Inside Airbnb (http://insideairbnb.com/get-the-data.html) on November 5, 2019, 
High-priced outliers are removed, and the dataset contains only properties with daily prices less than 500 pounds. 
The dataset has 83,327 unique records on the property listed on Airbnb in London covering 24 potential useful attributes. 

## Methodologies
Machine learning models including Linear Regression, Decision Trees, Random Forest Regression, Supporting Vector Machine (SVR), 
Gradient Boosting Machine (GBM), and Neural Network are applied and compared.

## Outcomes
1. GBM is considered the most suitable model with the lowest test set MSE (1,759.4)
2. The tree-based models and ensembled method based on tree (GBM) are used to analyze the relative importance of predictors. 
Based on the training result, room type, number of bathrooms, number of accommodates allowed, latitude, and availability in a year appear 
to be the five most important predictors for pricing on Airbnb.
