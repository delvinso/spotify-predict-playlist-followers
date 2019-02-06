+++
title =  "  Spotify - Predicting the Success of a Playlist Based on # of Followers"
author = "Delvin So"
date = "2019-01-19"
lastmod = "2019-01-19"
categories = ["prediction", "spotify", "linear regression", "eda", "xgboost", "random forest", "lasso"]
tags = ["independent"]
summary = "Predicting the number of followers a Spotify playlist will garner using playlist-aggregate features."
draft = false
highlight = true
[image]
  preview_only = true
math = true
+++

*Overall, this was a great exercise in data gathering, exploratory data analysis and predictive modelling as it pushed me to communicate my exploratory analysis, results, and understand what was happening behind the scenes of the models rather than treating each one as a black box.*

## Problem Statement and Motivation
- - - -
*The following was taken from from Harvard’s CS109A Final Project - Spotify Playlist Analysis*

Spotify is a music, podcast, and video streaming service. It provides digital rights management- protected content from record labels and media companies. Spotify is a freemium service, meaning that basic services are free with advertisements, with additional features, such as improved stream- ing quality, offered via paid subscriptions.

One of Spotify’s primary products is Playlists, collections of tracks that individual users (or Spotify) can build for every mood or event. Spotify users can make or follow as many playlists as they like. With over 40 million songs available, the company attempts to direct the most relevant songs to users based on their preferences, and Playlists often comprise the most convenient and effective way to convey these recommended songs to a user.

Spotify participates in the creation and curation of Playlists that are *followed*, or listened to, by millions of Spotify users. These Playlists are compiled in a complex manner, involving both human-led and computer-led processes. What stands is that algorithmically-curated discovery playlists, and their effectiveness, remain an important business interest for the company. The goal is to better understand how these algorithms can be evaluated and improved with machine learning techniques learned in the class.


The goal of this project is to predict the number of followers a Spotify playlist will obtain based on aggregate playlist-level features.  

The project is broken down into the following notebooks:

1. [Data Retrieval](https://github.com/delvinso/spotify-predict-followers/blob/master/01_mining.md)
2. [EDA and Feature Engineering](https://github.com/delvinso/spotify-predict-followers/blob/master/02_wrangle_eda_feat.md)
3. [Modeling](https://github.com/delvinso/spotify-predict-followers/blob/master/03_modeling.md)
4. ~~Playlist Generator~~

## The Data
- - - -
Spotify’s API does not currently offer any access to searching playlists with the most followers - however, we do have access to individual user's playlists. As Spotify's [own account](https://open.spotify.com/user/spotify) has numerous, highly featured playlists, we can use these for our goal of gaining insight into what features make a playlist successful. It's likely that Spotify features these playlists not only because they contain popular songs, but because there are intrinsic characteristics that make them popular.

92876 songs over 1541 playlists were retrieved from Spotify's featured playlists using their API in June 2018 using the spotifyr package and self-modified functions. For each track in each playlist, qualitative audio features such as the key, mode, the artist genre, and quantitative features such as loudness, acoustic ness, valence, etc were also fetched.   Artist information was retrieved based on the first match for a given query, if nothing was returned then the artist information was simply NA. For more detail, please refer to the [data retrieval notebook here](https://github.com/delvinso/spotify-predict-followers/blob/master/02_wrangle_eda_feat.md).


### Response - Playlist Followers
- - - -
The response variable is highly right skewed, and so I log transform it. This expands the values in the range of lower magnitudes while compressing the values in the range of higher magnitudes, essentially normalizing the distribution as much as possible.

![](/img/spotify/before_after_log.png)


### EDA & Feature Engineering
- - - -
As part of [the EDA notebook here](https://github.com/delvinso/spotify-predict-followers/blob/master/02_wrangle_eda_feat.md), I used data on the song level to create playlist-aggregated features  that may be predictive of a playlist’s success, measured in the log of a playlists’ followers.Numerous features were created, mostly by aggregating onto the playlist level as follows:

1.  Total Number of Tracks in the Playlist **(1)**
2.  Log of Artist Followers, Artist Popularity and Track Popularity  - Mean and Standard Deviation (except for artist followers) **(5)**
3. Two features were created based on the occurrence of frequently occurring (top 1%) artists and (top 5%) genres within ‘highly successful playlists’ determined as having the top 33% of playlist followers
	* Artists **(99)**
		* This was engineered by taking a list of artists appearing in playlists (of the top 33% playlists), tallying the  occurrence of artists and then looking at the top 5% occurrence. These  artists were hypothesized  to be important in determining the number of playlist followers a playlist garners, and one-hot encoded.
	* Genres **(49)**
		*  Spotify does not have genre information on the song level, however it does provide this information on the artist level.  This was engineered by taking a list of associated genres with each unique artist in a playlist (of the top 33% playlists), tallying the genres and then looking at the top 5% occurrence of genres. These genres were hypothesized  to be important in determining the number of playlist followers a playlist garners, and one-hot encoded.
4. Quantitative Audio Features -
	* Mean **(10)** & Standard Deviation **(10)**
	* Pairwise Interaction Terms **(190)**,
	* Second Degree Polynomials **(20)**
5.  Categorical Audio Features - Key, Mode, Key Mode **(Majority Vote; 37)**

After removing playlists that had missing values, the resultant data frame contained 1462 playlists and 421 predictors.


The data was split in a 80/20  (training/testing) fashion, with EDA and feature engineering performed on the training data.  This allows for the 20% held out data to act as a true validation dataset to estimate our model error.


## Approach to Model Selection
- - - -
The resulting data frame contained 1462 playlists with 421 predictors.  To predict the number of log playlist followers, I assessed the performance of 4 different models, Ordinary Least Squares Regression as a baseline model, Random Forests, LASSO, and XGBoost.  Where applicable, 5-fold cross-validation was used to estimate the model’s test error.

### Performance Metrics
- - - -
 3 standard performance metrics were chosen to evaluate the models.

1.  **RSquared** is a measure of how future datasets are likely to be predicted by our model. R2 is measured between 0 - 1, and is a measure of how much variation of the response can be explained by the predictors. The closer this value is to 1, the greater the variation is explained by the predictors. In practice, an R2 of 1 is unachievable. In other words, R2 is the explanatory power of the model.

$$R^2(y, \hat{y}) = 1 - \frac{\sum_{i=0}^{n-1}(y_i-\hat{y}i)^2)}{\sum{i=0}^{n-1}(y_i-\bar{y})^2}, n = \text{sample size}$$


3.  **MAE - Mean Absolute Error** is the average of absolute differences between the observed values and the predictions. More robust to outliers as individual differences are weighted equally.

$$MAE(y, \hat{y}) = \frac{1}{n} \sum_{i=0}^{n-1} |y_i-\hat{y}_i| $$

3. **RMSE - Root Mean Squared Error** is the square root of the average squared difference between predictors and observations, or more simply put, it is the standard deviation of the prediction error. More sensitive to outliers due to the differences being squared prior to the mean being taken.


$$RMSE(y, \hat{y}) = \sqrt{\frac{1}{n} \sum_{i=0}^{n-1} (y_i-\hat{y}_i)^2 }$$


For all the error functions, the smaller the error, the better the model.

These metrics will be the basis for model comparison at the end.

### Models
- - - -
For more detail on the models themselves, please refer to [the modelling notebook here](https://github.com/delvinso/spotify-predict-followers/blob/master/03_modeling.md).

#### OLS
- - - -
A baseline model was fit using the mean track and artist popularity, and mean artist followers using OLS regression.  All other models will be compared to this model, which had the following performance metrics:

```R
     RMSE  Rsquared       MAE
2.5567104 0.3559442 2.0105193
```

We can see that the fitted values explains 35.5% amount of variation in the observed values, meaning that the baseline model, using only 3 predictors boasts moderate explanatory power!



#### Random Forest
- - - -
For random forest,  all predictors were used. No hyper-parameter search was performed for Random Forest, and model defaults of ntrees = 500 and m = p/3 features were selected at each split. The out of bag error was used as an estimate of the test error.

```R
	  RMSE 	Rsquared
2.142374  0.5477789
```

An out of bag random forest does much better in terms of model performance! R2 increased by 154%, RMSE decreased by 119%.

The most important predictors as determined by the % increase in MSE if the variable is randomly permuted is as follows:

![](/img/spotify/rf_imp.png)


We see that the number of tracks and popularity related features are the most important variables, that is, when these variables are randomly shuffled (permuted), they result in the greatest increase of the test error, or reduction in accuracy. As we saw in the EDA, the track popularity is an important variable and thus likely an important characteristic of determining the success of a playlist. We also see that the majority of the top 30 most important variables are summary statistics related to quantitative audio features, with a few genres and one artist.

#### LASSO
- - - -

All predictors were normalized (ie. mean = 0 and variance = 1) to allow the penalization scheme to fairly affect all predictors.

For LASSO, I decided to fit the model first using only the mean track and artist popularity, and the mean artist followers similar to what was done for OLS.  This results in a lambda of 0.0045, RMSE of 2.56, R2 of 35.6% and MAE of 2.02.  To see if I could improve on the model, I sequentially add in predictors as follows:

* Quantitative Audio Features (20)
* Top 50 Predictors Correlated with the Log # of playlist followers (50)
* Top 50 Predictors + One-Hot Encoded Categorical Audio Features  (87)
* All Predictors (421)

The cross-validation metrics for each of the models can be seen below:

```r
# A tibble: 5 x 7
  alpha lambda  RMSE Rsquared   MAE set              type                                          
  <dbl>  <dbl> <dbl>    <dbl> <dbl> <chr>            <chr>                                         
1     1 0.0045  2.56    0.356  2.02 cross-validation Popularity and Follower Predictors            
2     1 0.0165  2.49    0.388  1.94 cross-validation Quantitative Predictors                       
3     1 0.0435  2.37    0.447  1.81 cross-validation Top 50 Predictors                             
4     1 0.064   2.33    0.468  1.77 cross-validation Top 50 Predictors + Categorical Audio Features
5     1 0.0745  2.34    0.466  1.78 cross-validation All Predictors  
```

A regularized regression model with only the popularity and follower predictors performs identically to the baseline model. As predictors are added in, we see a slight increase in accuracy and variation explained. However, adding in ALL predictors does not make the model perform any better than when the top 50 predictors and categorical audio features are included, in fact it performs slightly worse.

 This is likely because  the relevant predictors when all predictors are included, ie. those predictors whose coefficients were not shrunk to zero, are highly similar to those of the top 50 correlated predictors + categorical audio features, validating our inclusion of the top 50 correlated predictors.  Similarly, we can see that including categorical audio features does not drastically improve model performance compared to when the top 50 correlated predictors alone are used in the model.

Thus, the 'best' model here would be /#4, where the top 50 correlated predictors and categorical audio features were included. The most important predictors as determined by the absolute magnitude of the coefficients are

![](/img/spotify/lasso_imp.png)

The most important predictors corroborate with what we saw in the EDA, that is, track popularity, the number of tracks, and artist popularity. The remaining predictors are mostly categorical in nature and include key, key mode, along with highly popular genres and artists found in the the most popular playlists. In contrast, random forest had relatively more quantitative audio features.

#### XGBOOST
- - - -
Gradient boosting was used with ALL predictors to see if it could yield improvements compared to the other linear regression methods.  5 fold cross-validation was used to tune hyper-parameters. As there are various hyper-parameters to be identified, I first started with the optimal number of rounds by using default parameters, and an ETA of 0.1. The optimal number of rounds was found to be 87. Then, a grid search was performed with the optimal number of rounds, resulting in:

* rounds = 87
* ETA (Learning Rate) = 0.05
* Max Depth = 5
* Min Child Weight = 4

The cross-validation performance of is:
```r
       RMSE  Rsquared     MAE
44 2.082822 0.5762445 1.58734
```
The most important predictors:

![](/img/spotify/xgboost_imp.png)

Similar to random forest, the most important predictors are the number of tracks and those related to track popularity, which are dominant over all other predictors. Overall, it seems that quantitative audio features are relatively more important than categorical ones.

#### Cross-Validation
- - - -
Below are model performance statistics for each of the four models:

```r         
           ols    rf lasso   xgb
RMSE     2.557 2.142 2.328 2.083
Rsquared 0.356 0.548 0.468 0.576
MAE      2.011    NA 1.770 1.587

```


Based on the training data alone, we can see that the models that best explain the training data in order from worst to best are:

* Ordinary Least Squares Regression
* LASSO
* Random Forest
* XGBoost

Along with their corresponding scatterplots of predicted vs observed values  arising from the ‘best’ model:



![](/img/spotify/all_cv_scatter_same_axis.png)


Although the XGBoost model has the greatest cross-validation performance, Random Forest falls slightly behind, only 2.8% shy of Xgboost in terms of R2 while boasting a very similar RMSE. Furthermore, XGBoost requires several hyper-parameters to be tuned before the model could be finalized, whereas the Random Forest model was ‘out of box’ and is highly time efficient relative to XGBoost. However, Random Forest models are weakly interpretable,  whereas gradient boosting is relatively more interpretable so there is definitely a trade-off between the two.


##### Validation
- - - -
As XGBoost had the best performance, we will estimate the test error of our model using XGBoost using our held out, 20% validation dataset. The final model has the following performance metrics:

```r
     RMSE  Rsquared       MAE
2.0854239 0.4846405 1.5495922
```

And a corresponding scatterplot of predicted vs observed log # of playlist followers:


![](/img/spotify/xgboost_val_scatter.png)

Generally speaking, the model performs well as seen above. However, we can see it performs poorly when predicting the log number of followers on the lower end (given the cone/funnel shape of the plot), particularly when the log (# playlist followers) is under 10.  This is likely due to variables related to playlists with relatively fewer followers  not being included nor explored.

## Future Directions and Conclusions
- - - -
Generally, all models had a somewhat reasonable prediction of the response variable, with XGBoost having the best cross-validation performance. Using XGBoost on the held out 20% dataset, it yielded the  following performance metrics: RMSE of 2.08, R2 of 48.5% and MAE of 1.55.   Given that we are limited in data we can pull on the API, it may be worthwhile to explore other facets of these quantitative tracks as given such as the Million Song Dataset. Overall, this was a great exercise in data gathering, exploratory data analysis and modelling as it pushed me to understand what was happening behind the scenes of the models rather than treating each one as a black box.

Future directions to explore may be:

* Frequently Occurring Genres + Artists in Highly Successful Playlists - one hot encoding vs a simple categorical variable indicating their presence
* Integrating features from the Million Song Dataset
* Create a playlist generator
* Download more playlists as it has been ~ 6 months since the data was initially downloaded in order to increase the size of dataset and the robustness of the models
* Interactions of Popularity and the Total Number of Tracks
