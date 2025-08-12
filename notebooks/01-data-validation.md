Data Validation
================
Rohan Krishnan
2025-08-09

This notebook serves as the initial data validation and sanity check for
the raw train.csv and test.csv datasets. Our goal is to verify the
structural integrity of the data by examining dimensions, column types,
and checking for missing and unique values before moving on to
exploratory analysis and feature engineering.

## Import libraries

In this section, we load the core R packages required for our data
validation. In this notebook, we’ll only need tidyverse for data
manipulation and display and our custom data validation functions from
`R/data-validation.R`.

``` r
library(tidyverse)
source(here::here("R","data-validation.R"))
```

## Load train and test CSVs

We load the raw training and testing datasets from the data/raw
directory using the `here` package to ensure a reproducible file path,
regardless of where the project is run.

``` r
# Use the here library to get relative paths from .Rproj directory
train_raw <- read.csv(here::here("data", "raw", "train.csv"))
test_raw <- read.csv(here::here("data", "raw", "test.csv"))
```

## Get a basic snapshot of what is contained in train.csv and test.csv

The first thing we look at is just the first 5 rows of the train and
test CSV files.

``` r
train_raw %>% head()
```

    ##   id                      album_name                             track_name
    ## 1  1 Tum Hi Aana (From "Marjaavaan")        Tum Hi Aana (From "Marjaavaan")
    ## 2  2                  Hounds Of Love Running Up That Hill (A Deal With God)
    ## 3  3                       Fine Line                       Watermelon Sugar
    ## 4  4                  Sunny Mornings                       I Feel It Coming
    ## 5  5                     Elis Regina                         Tiro Ao Álvaro
    ## 6  6            Weihnachten Playlist                              Mistletoe
    ##   popularity duration_ms explicit danceability energy key loudness mode
    ## 1         70      249126    FALSE        0.473  0.401   4   -7.018    0
    ## 2         90      298933    FALSE        0.629  0.547  10  -13.123    0
    ## 3         89      174000    FALSE        0.548  0.816   0   -4.209    1
    ## 4          3      269186    FALSE        0.773  0.820   0   -5.897    0
    ## 5         39      162400    FALSE        0.603  0.649  10   -7.006    1
    ## 6          0      183066    FALSE        0.623  0.668   6   -7.282    0
    ##   speechiness acousticness instrumentalness liveness valence   tempo
    ## 1      0.0241        0.629         1.17e-05   0.1540   0.345  90.076
    ## 2      0.0550        0.720         3.14e-03   0.0604   0.197 108.375
    ## 3      0.0465        0.122         0.00e+00   0.3350   0.557  95.390
    ## 4      0.1150        0.394         0.00e+00   0.0739   0.555  92.996
    ## 5      0.1040        0.463         0.00e+00   0.1010   0.957  93.781
    ## 6      0.0531        0.475         0.00e+00   0.0862   0.823 161.948
    ##   time_signature track_genre
    ## 1              4         pop
    ## 2              4        rock
    ## 3              4         pop
    ## 4              4         pop
    ## 5              4        jazz
    ## 6              4         pop

``` r
test_raw %>% head()
```

    ##     id                                    album_name
    ## 1 1201                        Beste Weihnachtslieder
    ## 2 1202                                    LUGNA HITS
    ## 3 1203                                    Blue Train
    ## 4 1204 Kannil Kannil [From "Sita Ramam (Malayalam)"]
    ## 5 1205                                    REGNIG DAG
    ## 6 1206                                       Shiddat
    ##                                      track_name duration_ms explicit
    ## 1                            Frosty The Snowman      131733    FALSE
    ## 2                                   Out of Time      214190    FALSE
    ## 3                                    Blue Train      643127    FALSE
    ## 4 Kannil Kannil (From "Sita Ramam (Malayalam)")      232513    FALSE
    ## 5                 As Tears Go By - Mono Version      165213    FALSE
    ## 6                           Shiddat Title Track      230875    FALSE
    ##   danceability energy key loudness mode speechiness acousticness
    ## 1        0.579  0.502   8   -7.570    1      0.0513        0.733
    ## 2        0.629  0.763   0   -4.279    0      0.0453        0.244
    ## 3        0.503  0.286   8  -15.425    1      0.0450        0.643
    ## 4        0.516  0.409   6  -11.925    0      0.0566        0.718
    ## 5        0.329  0.282   7  -10.863    1      0.0294        0.710
    ## 6        0.489  0.625   9   -6.497    0      0.0584        0.288
    ##   instrumentalness liveness valence   tempo time_signature track_genre
    ## 1          0.00000    0.281   0.836  76.783              4        jazz
    ## 2          0.00000    0.312   0.809  93.023              4         pop
    ## 3          0.00112    0.156   0.569 136.098              4        jazz
    ## 4          0.00000    0.350   0.731 179.852              3         pop
    ## 5          0.00265    0.158   0.393 112.944              4        rock
    ## 6          0.00000    0.133   0.439 149.921              3         pop

We can see from the first 5 rows of both datasets that each row
represents a “track” (or song) and its relevant features like duration
in milliseconds, whether or not it is explicit, its danceablilty (score
created by Spotify), and other similar features. An important difference
to note between train.csv and test.csv is that test.csv does have the
“popularity” (target) colummn. Since our task is to understand what
makes a song popular and explore if it is possible to predict a song’s
popularity from its features, the current structure of the data is
already ideal and we likely will not need to perform any aggregations.

## Look at the basic dataset structure for train.csv and test.csv

We perform a high-level comparison of the training and testing sets,
examining their dimensions and identifying the target column, to ensure
the datasets are correctly structured for our analysis.

``` r
compare_train_test_raw_dims(train_df_raw = train_raw, test_df_raw = test_raw)
```

    ##  Here is a quick comparison between your raw train and test datasets:
    ##  
    ##   - Training data rows: 1200
    ##   - Testing data rows: 500
    ##   - Training data columns: 19
    ##   - Testing data columns: 18
    ##   - Target column: popularity
    ##   
    ## The total number of rows of data is: 1700. 
    ## 
    ## The training set makes up 71% of the total data while the test set makes up 29% of the data.

## Look at column data types for each dataset and make sure they match

Here we confirm that the data types of all matching columns are
consistent between the training and testing sets. This is a critical
check to ensure a smooth preprocessing pipeline.

``` r
check_col_dtypes(train_df_raw = train_raw, test_df_raw = test_raw)
```

    ## All common column types match between train and test sets

Since all of the matching columns have the same data type. We can just
look at `train_raw`’s classes to get a sense of each columns data type.

``` r
for (i in 1:ncol(train_raw)){
  cat(paste0(colnames(train_raw[i]),": ", class(train_raw[,i]), "\n"))
}
```

    ## id: integer
    ## album_name: character
    ## track_name: character
    ## popularity: integer
    ## duration_ms: integer
    ## explicit: logical
    ## danceability: numeric
    ## energy: numeric
    ## key: integer
    ## loudness: numeric
    ## mode: integer
    ## speechiness: numeric
    ## acousticness: numeric
    ## instrumentalness: numeric
    ## liveness: numeric
    ## valence: numeric
    ## tempo: numeric
    ## time_signature: integer
    ## track_genre: character

## Check for missing values

This section defines and uses a function to check for any missing values
(NAs) in the datasets. A clean dataset with no missing values is crucial
for building reliable models.

``` r
check_NAs(train_df_raw = train_raw, test_df_raw = test_raw, target_col = "popularity")
```

    ##              Column Train_NA_Count Test_NA_Count
    ## 1                id              0             0
    ## 2        album_name              0             0
    ## 3        track_name              0             0
    ## 4        popularity              0            NA
    ## 5       duration_ms              0             0
    ## 6          explicit              0             0
    ## 7      danceability              0             0
    ## 8            energy              0             0
    ## 9               key              0             0
    ## 10         loudness              0             0
    ## 11             mode              0             0
    ## 12      speechiness              0             0
    ## 13     acousticness              0             0
    ## 14 instrumentalness              0             0
    ## 15         liveness              0             0
    ## 16          valence              0             0
    ## 17            tempo              0             0
    ## 18   time_signature              0             0
    ## 19      track_genre              0             0

Our check confirms that there are no NA values in any of the columns in
the train and test datasets. In subsequent notebooks, we’ll explore if
there are non-sensical values in any of these columns but for now we can
move on.

## Unique values

In this final validation step, we examine the number of distinct values
per column to identify which variables are good candidates for being
treated as categorical or binary in our feature engineering.

``` r
check_distinct_vals(train_df_raw = train_raw, test_df_raw = test_raw, target_col = "popularity")
```

    ##              Column Train_Distinct_Values Test_Distict_Values
    ## 1                id                  1200                 500
    ## 2        album_name                   718                 351
    ## 3        track_name                   840                 388
    ## 4        popularity                    70                  NA
    ## 5       duration_ms                   861                 400
    ## 6          explicit                     2                   2
    ## 7      danceability                   428                 285
    ## 8            energy                   556                 319
    ## 9               key                    12                  12
    ## 10         loudness                   845                 393
    ## 11             mode                     2                   2
    ## 12      speechiness                   484                 285
    ## 13     acousticness                   680                 364
    ## 14 instrumentalness                   454                 215
    ## 15         liveness                   435                 261
    ## 16          valence                   554                 312
    ## 17            tempo                   869                 398
    ## 18   time_signature                     5                   4
    ## 19      track_genre                     3                   3

The above table lists all of the distinct values for all of the columns.
Since we are only interested in potential binary or categorical
variables, let’s filter the table down so it only displays the columns
with less than 15 distinct values in either the train or test dataset.

The following table filters the full unique values summary to only show
columns with fewer than 15 distinct values in either the train or test
sets. This helps us quickly pinpoint potential categorical features.

``` r
distinct_vals <- check_distinct_vals(train_df_raw = train_raw, test_df_raw = test_raw, target_col = "popularity", print_output = FALSE)

distinct_vals %>%
  dplyr::filter(Train_Distinct_Values < 15 | Test_Distict_Values < 15)
```

    ##           Column Train_Distinct_Values Test_Distict_Values
    ## 1       explicit                     2                   2
    ## 2            key                    12                  12
    ## 3           mode                     2                   2
    ## 4 time_signature                     5                   4
    ## 5    track_genre                     3                   3

After filtering, we can see that:

1)  The columns *almost* match between train and test sets. It looks
    like there is one less time_signature in the test set.
2)  There are 5 columns that are either binary or categorical.
    Specifically:

- `explicit` is a binary variable and can be made into a dummy column
- `key` is a categorical variable and should be turned into factors
- `mode` is a binary variable and can be made into a dummy column
- `time_signature` is a categorical variable and should be turned into
  factors
- `track_genre` is a categorical variable and should be turned into
  factors

## Conclusion

Based on our initial data validation, we can draw the following
conclusions that will inform our next steps in the project:

- *Data Integrity*: The train and test datasets are structurally sound,
  with consistent column types and no missing values across all
  features.
- *Categorical Features*: We have identified five columns with a low
  number of unique values that are suitable for conversion to
  categorical factors: explicit, key, mode, time_signature, and
  track_genre.
- *Target Column*: The popularity column is correctly identified as the
  target variable, present only in the training set.
- *Structural Discrepancy*: The time_signature variable has a different
  number of distinct values between the training and testing sets. This
  will need to be addressed during the feature engineering phase to
  prevent errors in subsequent analysis.
- *Unused Columns*: The id, album_name, and track_name columns are not
  relevant for modeling and should be removed from the datasets before
  any further analysis.

These findings provide a solid foundation for the data cleaning and
preprocessing steps that will be carried out in the next notebook.
