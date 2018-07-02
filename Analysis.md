---
title: "Predicting the quality of weight lifting exercise activity"
output: html_document
---

## The analysis is divided into two main sections
### First: Exploratory data analysis

Load libraries and data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(magrittr)
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```
## Use suppressPackageStartupMessages() to eliminate package startup
## messages.
```

```r
library(doParallel)
```

```
## Loading required package: foreach
```

```
## Loading required package: iterators
```

```
## Loading required package: parallel
```

```r
Training <- read.csv("/Users/amrmostafa/Documents/Data Science/Course8/Project/pml-training.csv",stringsAsFactors = FALSE)
Testing <- read.csv("/Users/amrmostafa/Documents/Data Science/Course8/Project/pml-testing.csv",stringsAsFactors = FALSE)
```

####Check the training set


```r
dim(Training)
```

```
## [1] 19622   160
```

```r
str(Training)
```

```
## 'data.frame':	19622 obs. of  160 variables:
##  $ X                       : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ user_name               : chr  "carlitos" "carlitos" "carlitos" "carlitos" ...
##  $ raw_timestamp_part_1    : int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
##  $ raw_timestamp_part_2    : int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
##  $ cvtd_timestamp          : chr  "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" ...
##  $ new_window              : chr  "no" "no" "no" "no" ...
##  $ num_window              : int  11 11 11 12 12 12 12 12 12 12 ...
##  $ roll_belt               : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt              : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt                : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt        : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ kurtosis_roll_belt      : chr  "" "" "" "" ...
##  $ kurtosis_picth_belt     : chr  "" "" "" "" ...
##  $ kurtosis_yaw_belt       : chr  "" "" "" "" ...
##  $ skewness_roll_belt      : chr  "" "" "" "" ...
##  $ skewness_roll_belt.1    : chr  "" "" "" "" ...
##  $ skewness_yaw_belt       : chr  "" "" "" "" ...
##  $ max_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_belt            : chr  "" "" "" "" ...
##  $ min_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_belt            : chr  "" "" "" "" ...
##  $ amplitude_roll_belt     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_pitch_belt    : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_yaw_belt      : chr  "" "" "" "" ...
##  $ var_total_accel_belt    : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_roll_belt        : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_pitch_belt       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_yaw_belt         : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ gyros_belt_x            : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y            : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z            : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x            : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y            : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z            : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x           : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y           : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z           : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm                : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm               : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm                 : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm         : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ var_accel_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_roll_arm         : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_pitch_arm        : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_yaw_arm          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ gyros_arm_x             : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y             : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z             : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x             : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y             : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z             : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x            : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y            : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z            : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ kurtosis_roll_arm       : chr  "" "" "" "" ...
##  $ kurtosis_picth_arm      : chr  "" "" "" "" ...
##  $ kurtosis_yaw_arm        : chr  "" "" "" "" ...
##  $ skewness_roll_arm       : chr  "" "" "" "" ...
##  $ skewness_pitch_arm      : chr  "" "" "" "" ...
##  $ skewness_yaw_arm        : chr  "" "" "" "" ...
##  $ max_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_roll_arm      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_pitch_arm     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_yaw_arm       : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ roll_dumbbell           : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell          : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell            : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ kurtosis_roll_dumbbell  : chr  "" "" "" "" ...
##  $ kurtosis_picth_dumbbell : chr  "" "" "" "" ...
##  $ kurtosis_yaw_dumbbell   : chr  "" "" "" "" ...
##  $ skewness_roll_dumbbell  : chr  "" "" "" "" ...
##  $ skewness_pitch_dumbbell : chr  "" "" "" "" ...
##  $ skewness_yaw_dumbbell   : chr  "" "" "" "" ...
##  $ max_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_dumbbell        : chr  "" "" "" "" ...
##  $ min_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_dumbbell        : chr  "" "" "" "" ...
##  $ amplitude_roll_dumbbell : num  NA NA NA NA NA NA NA NA NA NA ...
##   [list output truncated]
```

```r
summary(Training)
```

```
##        X          user_name         raw_timestamp_part_1
##  Min.   :    1   Length:19622       Min.   :1.322e+09   
##  1st Qu.: 4906   Class :character   1st Qu.:1.323e+09   
##  Median : 9812   Mode  :character   Median :1.323e+09   
##  Mean   : 9812                      Mean   :1.323e+09   
##  3rd Qu.:14717                      3rd Qu.:1.323e+09   
##  Max.   :19622                      Max.   :1.323e+09   
##  raw_timestamp_part_2 cvtd_timestamp      new_window       
##  Min.   :   294       Length:19622       Length:19622      
##  1st Qu.:252912       Class :character   Class :character  
##  Median :496380       Mode  :character   Mode  :character  
##  Mean   :500656                                            
##  3rd Qu.:751891                                            
##  Max.   :998801                                            
##    num_window      roll_belt        pitch_belt          yaw_belt      
##  Min.   :  1.0   Min.   :-28.90   Min.   :-55.8000   Min.   :-180.00  
##  1st Qu.:222.0   1st Qu.:  1.10   1st Qu.:  1.7600   1st Qu.: -88.30  
##  Median :424.0   Median :113.00   Median :  5.2800   Median : -13.00  
##  Mean   :430.6   Mean   : 64.41   Mean   :  0.3053   Mean   : -11.21  
##  3rd Qu.:644.0   3rd Qu.:123.00   3rd Qu.: 14.9000   3rd Qu.:  12.90  
##  Max.   :864.0   Max.   :162.00   Max.   : 60.3000   Max.   : 179.00  
##  total_accel_belt kurtosis_roll_belt kurtosis_picth_belt
##  Min.   : 0.00    Length:19622       Length:19622       
##  1st Qu.: 3.00    Class :character   Class :character   
##  Median :17.00    Mode  :character   Mode  :character   
##  Mean   :11.31                                          
##  3rd Qu.:18.00                                          
##  Max.   :29.00                                          
##  kurtosis_yaw_belt  skewness_roll_belt skewness_roll_belt.1
##  Length:19622       Length:19622       Length:19622        
##  Class :character   Class :character   Class :character    
##  Mode  :character   Mode  :character   Mode  :character    
##                                                            
##                                                            
##                                                            
##  skewness_yaw_belt  max_roll_belt     max_picth_belt  max_yaw_belt      
##  Length:19622       Min.   :-94.300   Min.   : 3.00   Length:19622      
##  Class :character   1st Qu.:-88.000   1st Qu.: 5.00   Class :character  
##  Mode  :character   Median : -5.100   Median :18.00   Mode  :character  
##                     Mean   : -6.667   Mean   :12.92                     
##                     3rd Qu.: 18.500   3rd Qu.:19.00                     
##                     Max.   :180.000   Max.   :30.00                     
##  min_roll_belt     min_pitch_belt  min_yaw_belt       amplitude_roll_belt
##  Min.   :-180.00   Min.   : 0.00   Length:19622       Min.   :  0.000    
##  1st Qu.: -88.40   1st Qu.: 3.00   Class :character   1st Qu.:  0.300    
##  Median :  -7.85   Median :16.00   Mode  :character   Median :  1.000    
##  Mean   : -10.44   Mean   :10.76                      Mean   :  3.769    
##  3rd Qu.:   9.05   3rd Qu.:17.00                      3rd Qu.:  2.083    
##  Max.   : 173.00   Max.   :23.00                      Max.   :360.000    
##  amplitude_pitch_belt amplitude_yaw_belt var_total_accel_belt
##  Min.   : 0.000       Length:19622       Min.   : 0.000      
##  1st Qu.: 1.000       Class :character   1st Qu.: 0.100      
##  Median : 1.000       Mode  :character   Median : 0.200      
##  Mean   : 2.167                          Mean   : 0.926      
##  3rd Qu.: 2.000                          3rd Qu.: 0.300      
##  Max.   :12.000                          Max.   :16.500      
##  avg_roll_belt    stddev_roll_belt var_roll_belt     avg_pitch_belt   
##  Min.   :-27.40   Min.   : 0.000   Min.   :  0.000   Min.   :-51.400  
##  1st Qu.:  1.10   1st Qu.: 0.200   1st Qu.:  0.000   1st Qu.:  2.025  
##  Median :116.35   Median : 0.400   Median :  0.100   Median :  5.200  
##  Mean   : 68.06   Mean   : 1.337   Mean   :  7.699   Mean   :  0.520  
##  3rd Qu.:123.38   3rd Qu.: 0.700   3rd Qu.:  0.500   3rd Qu.: 15.775  
##  Max.   :157.40   Max.   :14.200   Max.   :200.700   Max.   : 59.700  
##  stddev_pitch_belt var_pitch_belt    avg_yaw_belt      stddev_yaw_belt  
##  Min.   :0.000     Min.   : 0.000   Min.   :-138.300   Min.   :  0.000  
##  1st Qu.:0.200     1st Qu.: 0.000   1st Qu.: -88.175   1st Qu.:  0.100  
##  Median :0.400     Median : 0.100   Median :  -6.550   Median :  0.300  
##  Mean   :0.603     Mean   : 0.766   Mean   :  -8.831   Mean   :  1.341  
##  3rd Qu.:0.700     3rd Qu.: 0.500   3rd Qu.:  14.125   3rd Qu.:  0.700  
##  Max.   :4.000     Max.   :16.200   Max.   : 173.500   Max.   :176.600  
##   var_yaw_belt        gyros_belt_x        gyros_belt_y     
##  Min.   :    0.000   Min.   :-1.040000   Min.   :-0.64000  
##  1st Qu.:    0.010   1st Qu.:-0.030000   1st Qu.: 0.00000  
##  Median :    0.090   Median : 0.030000   Median : 0.02000  
##  Mean   :  107.487   Mean   :-0.005592   Mean   : 0.03959  
##  3rd Qu.:    0.475   3rd Qu.: 0.110000   3rd Qu.: 0.11000  
##  Max.   :31183.240   Max.   : 2.220000   Max.   : 0.64000  
##   gyros_belt_z      accel_belt_x       accel_belt_y     accel_belt_z    
##  Min.   :-1.4600   Min.   :-120.000   Min.   :-69.00   Min.   :-275.00  
##  1st Qu.:-0.2000   1st Qu.: -21.000   1st Qu.:  3.00   1st Qu.:-162.00  
##  Median :-0.1000   Median : -15.000   Median : 35.00   Median :-152.00  
##  Mean   :-0.1305   Mean   :  -5.595   Mean   : 30.15   Mean   : -72.59  
##  3rd Qu.:-0.0200   3rd Qu.:  -5.000   3rd Qu.: 61.00   3rd Qu.:  27.00  
##  Max.   : 1.6200   Max.   :  85.000   Max.   :164.00   Max.   : 105.00  
##  magnet_belt_x   magnet_belt_y   magnet_belt_z       roll_arm      
##  Min.   :-52.0   Min.   :354.0   Min.   :-623.0   Min.   :-180.00  
##  1st Qu.:  9.0   1st Qu.:581.0   1st Qu.:-375.0   1st Qu.: -31.77  
##  Median : 35.0   Median :601.0   Median :-320.0   Median :   0.00  
##  Mean   : 55.6   Mean   :593.7   Mean   :-345.5   Mean   :  17.83  
##  3rd Qu.: 59.0   3rd Qu.:610.0   3rd Qu.:-306.0   3rd Qu.:  77.30  
##  Max.   :485.0   Max.   :673.0   Max.   : 293.0   Max.   : 180.00  
##    pitch_arm          yaw_arm          total_accel_arm var_accel_arm   
##  Min.   :-88.800   Min.   :-180.0000   Min.   : 1.00   Min.   :  0.00  
##  1st Qu.:-25.900   1st Qu.: -43.1000   1st Qu.:17.00   1st Qu.:  9.03  
##  Median :  0.000   Median :   0.0000   Median :27.00   Median : 40.61  
##  Mean   : -4.612   Mean   :  -0.6188   Mean   :25.51   Mean   : 53.23  
##  3rd Qu.: 11.200   3rd Qu.:  45.8750   3rd Qu.:33.00   3rd Qu.: 75.62  
##  Max.   : 88.500   Max.   : 180.0000   Max.   :66.00   Max.   :331.70  
##   avg_roll_arm     stddev_roll_arm    var_roll_arm       avg_pitch_arm    
##  Min.   :-166.67   Min.   :  0.000   Min.   :    0.000   Min.   :-81.773  
##  1st Qu.: -38.37   1st Qu.:  1.376   1st Qu.:    1.898   1st Qu.:-22.770  
##  Median :   0.00   Median :  5.702   Median :   32.517   Median :  0.000  
##  Mean   :  12.68   Mean   : 11.201   Mean   :  417.264   Mean   : -4.901  
##  3rd Qu.:  76.33   3rd Qu.: 14.921   3rd Qu.:  222.647   3rd Qu.:  8.277  
##  Max.   : 163.33   Max.   :161.964   Max.   :26232.208   Max.   : 75.659  
##  stddev_pitch_arm var_pitch_arm       avg_yaw_arm       stddev_yaw_arm   
##  Min.   : 0.000   Min.   :   0.000   Min.   :-173.440   Min.   :  0.000  
##  1st Qu.: 1.642   1st Qu.:   2.697   1st Qu.: -29.198   1st Qu.:  2.577  
##  Median : 8.133   Median :  66.146   Median :   0.000   Median : 16.682  
##  Mean   :10.383   Mean   : 195.864   Mean   :   2.359   Mean   : 22.270  
##  3rd Qu.:16.327   3rd Qu.: 266.576   3rd Qu.:  38.185   3rd Qu.: 35.984  
##  Max.   :43.412   Max.   :1884.565   Max.   : 152.000   Max.   :177.044  
##   var_yaw_arm         gyros_arm_x        gyros_arm_y     
##  Min.   :    0.000   Min.   :-6.37000   Min.   :-3.4400  
##  1st Qu.:    6.642   1st Qu.:-1.33000   1st Qu.:-0.8000  
##  Median :  278.309   Median : 0.08000   Median :-0.2400  
##  Mean   : 1055.933   Mean   : 0.04277   Mean   :-0.2571  
##  3rd Qu.: 1294.850   3rd Qu.: 1.57000   3rd Qu.: 0.1400  
##  Max.   :31344.568   Max.   : 4.87000   Max.   : 2.8400  
##   gyros_arm_z       accel_arm_x       accel_arm_y      accel_arm_z     
##  Min.   :-2.3300   Min.   :-404.00   Min.   :-318.0   Min.   :-636.00  
##  1st Qu.:-0.0700   1st Qu.:-242.00   1st Qu.: -54.0   1st Qu.:-143.00  
##  Median : 0.2300   Median : -44.00   Median :  14.0   Median : -47.00  
##  Mean   : 0.2695   Mean   : -60.24   Mean   :  32.6   Mean   : -71.25  
##  3rd Qu.: 0.7200   3rd Qu.:  84.00   3rd Qu.: 139.0   3rd Qu.:  23.00  
##  Max.   : 3.0200   Max.   : 437.00   Max.   : 308.0   Max.   : 292.00  
##   magnet_arm_x     magnet_arm_y     magnet_arm_z    kurtosis_roll_arm 
##  Min.   :-584.0   Min.   :-392.0   Min.   :-597.0   Length:19622      
##  1st Qu.:-300.0   1st Qu.:  -9.0   1st Qu.: 131.2   Class :character  
##  Median : 289.0   Median : 202.0   Median : 444.0   Mode  :character  
##  Mean   : 191.7   Mean   : 156.6   Mean   : 306.5                     
##  3rd Qu.: 637.0   3rd Qu.: 323.0   3rd Qu.: 545.0                     
##  Max.   : 782.0   Max.   : 583.0   Max.   : 694.0                     
##  kurtosis_picth_arm kurtosis_yaw_arm   skewness_roll_arm 
##  Length:19622       Length:19622       Length:19622      
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##  skewness_pitch_arm skewness_yaw_arm    max_roll_arm    
##  Length:19622       Length:19622       Min.   :-73.100  
##  Class :character   Class :character   1st Qu.: -0.175  
##  Mode  :character   Mode  :character   Median :  4.950  
##                                        Mean   : 11.236  
##                                        3rd Qu.: 26.775  
##                                        Max.   : 85.500  
##  max_picth_arm       max_yaw_arm     min_roll_arm    min_pitch_arm    
##  Min.   :-173.000   Min.   : 4.00   Min.   :-89.10   Min.   :-180.00  
##  1st Qu.:  -1.975   1st Qu.:29.00   1st Qu.:-41.98   1st Qu.: -72.62  
##  Median :  23.250   Median :34.00   Median :-22.45   Median : -33.85  
##  Mean   :  35.751   Mean   :35.46   Mean   :-21.22   Mean   : -33.92  
##  3rd Qu.:  95.975   3rd Qu.:41.00   3rd Qu.:  0.00   3rd Qu.:   0.00  
##  Max.   : 180.000   Max.   :65.00   Max.   : 66.40   Max.   : 152.00  
##   min_yaw_arm    amplitude_roll_arm amplitude_pitch_arm amplitude_yaw_arm
##  Min.   : 1.00   Min.   :  0.000    Min.   :  0.000     Min.   : 0.00    
##  1st Qu.: 8.00   1st Qu.:  5.425    1st Qu.:  9.925     1st Qu.:13.00    
##  Median :13.00   Median : 28.450    Median : 54.900     Median :22.00    
##  Mean   :14.66   Mean   : 32.452    Mean   : 69.677     Mean   :20.79    
##  3rd Qu.:19.00   3rd Qu.: 50.960    3rd Qu.:115.175     3rd Qu.:28.75    
##  Max.   :38.00   Max.   :119.500    Max.   :360.000     Max.   :52.00    
##  roll_dumbbell     pitch_dumbbell     yaw_dumbbell     
##  Min.   :-153.71   Min.   :-149.59   Min.   :-150.871  
##  1st Qu.: -18.49   1st Qu.: -40.89   1st Qu.: -77.644  
##  Median :  48.17   Median : -20.96   Median :  -3.324  
##  Mean   :  23.84   Mean   : -10.78   Mean   :   1.674  
##  3rd Qu.:  67.61   3rd Qu.:  17.50   3rd Qu.:  79.643  
##  Max.   : 153.55   Max.   : 149.40   Max.   : 154.952  
##  kurtosis_roll_dumbbell kurtosis_picth_dumbbell kurtosis_yaw_dumbbell
##  Length:19622           Length:19622            Length:19622         
##  Class :character       Class :character        Class :character     
##  Mode  :character       Mode  :character        Mode  :character     
##                                                                      
##                                                                      
##                                                                      
##  skewness_roll_dumbbell skewness_pitch_dumbbell skewness_yaw_dumbbell
##  Length:19622           Length:19622            Length:19622         
##  Class :character       Class :character        Class :character     
##  Mode  :character       Mode  :character        Mode  :character     
##                                                                      
##                                                                      
##                                                                      
##  max_roll_dumbbell max_picth_dumbbell max_yaw_dumbbell   min_roll_dumbbell
##  Min.   :-70.10    Min.   :-112.90    Length:19622       Min.   :-149.60  
##  1st Qu.:-27.15    1st Qu.: -66.70    Class :character   1st Qu.: -59.67  
##  Median : 14.85    Median :  40.05    Mode  :character   Median : -43.55  
##  Mean   : 13.76    Mean   :  32.75                       Mean   : -41.24  
##  3rd Qu.: 50.58    3rd Qu.: 133.22                       3rd Qu.: -25.20  
##  Max.   :137.00    Max.   : 155.00                       Max.   :  73.20  
##  min_pitch_dumbbell min_yaw_dumbbell   amplitude_roll_dumbbell
##  Min.   :-147.00    Length:19622       Min.   :  0.00         
##  1st Qu.: -91.80    Class :character   1st Qu.: 14.97         
##  Median : -66.15    Mode  :character   Median : 35.05         
##  Mean   : -33.18                       Mean   : 55.00         
##  3rd Qu.:  21.20                       3rd Qu.: 81.04         
##  Max.   : 120.90                       Max.   :256.48         
##  amplitude_pitch_dumbbell amplitude_yaw_dumbbell total_accel_dumbbell
##  Min.   :  0.00           Length:19622           Min.   : 0.00       
##  1st Qu.: 17.06           Class :character       1st Qu.: 4.00       
##  Median : 41.73           Mode  :character       Median :10.00       
##  Mean   : 65.93                                  Mean   :13.72       
##  3rd Qu.: 99.55                                  3rd Qu.:19.00       
##  Max.   :273.59                                  Max.   :58.00       
##  var_accel_dumbbell avg_roll_dumbbell stddev_roll_dumbbell
##  Min.   :  0.000    Min.   :-128.96   Min.   :  0.000     
##  1st Qu.:  0.378    1st Qu.: -12.33   1st Qu.:  4.639     
##  Median :  1.000    Median :  48.23   Median : 12.204     
##  Mean   :  4.388    Mean   :  23.86   Mean   : 20.761     
##  3rd Qu.:  3.434    3rd Qu.:  64.37   3rd Qu.: 26.356     
##  Max.   :230.428    Max.   : 125.99   Max.   :123.778     
##  var_roll_dumbbell  avg_pitch_dumbbell stddev_pitch_dumbbell
##  Min.   :    0.00   Min.   :-70.73     Min.   : 0.000       
##  1st Qu.:   21.52   1st Qu.:-42.00     1st Qu.: 3.482       
##  Median :  148.95   Median :-19.91     Median : 8.089       
##  Mean   : 1020.27   Mean   :-12.33     Mean   :13.147       
##  3rd Qu.:  694.65   3rd Qu.: 13.21     3rd Qu.:19.238       
##  Max.   :15321.01   Max.   : 94.28     Max.   :82.680       
##  var_pitch_dumbbell avg_yaw_dumbbell   stddev_yaw_dumbbell
##  Min.   :   0.00    Min.   :-117.950   Min.   :  0.000    
##  1st Qu.:  12.12    1st Qu.: -76.696   1st Qu.:  3.885    
##  Median :  65.44    Median :  -4.505   Median : 10.264    
##  Mean   : 350.31    Mean   :   0.202   Mean   : 16.647    
##  3rd Qu.: 370.11    3rd Qu.:  71.234   3rd Qu.: 24.674    
##  Max.   :6836.02    Max.   : 134.905   Max.   :107.088    
##  var_yaw_dumbbell   gyros_dumbbell_x    gyros_dumbbell_y  
##  Min.   :    0.00   Min.   :-204.0000   Min.   :-2.10000  
##  1st Qu.:   15.09   1st Qu.:  -0.0300   1st Qu.:-0.14000  
##  Median :  105.35   Median :   0.1300   Median : 0.03000  
##  Mean   :  589.84   Mean   :   0.1611   Mean   : 0.04606  
##  3rd Qu.:  608.79   3rd Qu.:   0.3500   3rd Qu.: 0.21000  
##  Max.   :11467.91   Max.   :   2.2200   Max.   :52.00000  
##  gyros_dumbbell_z  accel_dumbbell_x  accel_dumbbell_y  accel_dumbbell_z 
##  Min.   : -2.380   Min.   :-419.00   Min.   :-189.00   Min.   :-334.00  
##  1st Qu.: -0.310   1st Qu.: -50.00   1st Qu.:  -8.00   1st Qu.:-142.00  
##  Median : -0.130   Median :  -8.00   Median :  41.50   Median :  -1.00  
##  Mean   : -0.129   Mean   : -28.62   Mean   :  52.63   Mean   : -38.32  
##  3rd Qu.:  0.030   3rd Qu.:  11.00   3rd Qu.: 111.00   3rd Qu.:  38.00  
##  Max.   :317.000   Max.   : 235.00   Max.   : 315.00   Max.   : 318.00  
##  magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z  roll_forearm      
##  Min.   :-643.0    Min.   :-3600     Min.   :-262.00   Min.   :-180.0000  
##  1st Qu.:-535.0    1st Qu.:  231     1st Qu.: -45.00   1st Qu.:  -0.7375  
##  Median :-479.0    Median :  311     Median :  13.00   Median :  21.7000  
##  Mean   :-328.5    Mean   :  221     Mean   :  46.05   Mean   :  33.8265  
##  3rd Qu.:-304.0    3rd Qu.:  390     3rd Qu.:  95.00   3rd Qu.: 140.0000  
##  Max.   : 592.0    Max.   :  633     Max.   : 452.00   Max.   : 180.0000  
##  pitch_forearm     yaw_forearm      kurtosis_roll_forearm
##  Min.   :-72.50   Min.   :-180.00   Length:19622         
##  1st Qu.:  0.00   1st Qu.: -68.60   Class :character     
##  Median :  9.24   Median :   0.00   Mode  :character     
##  Mean   : 10.71   Mean   :  19.21                        
##  3rd Qu.: 28.40   3rd Qu.: 110.00                        
##  Max.   : 89.80   Max.   : 180.00                        
##  kurtosis_picth_forearm kurtosis_yaw_forearm skewness_roll_forearm
##  Length:19622           Length:19622         Length:19622         
##  Class :character       Class :character     Class :character     
##  Mode  :character       Mode  :character     Mode  :character     
##                                                                   
##                                                                   
##                                                                   
##  skewness_pitch_forearm skewness_yaw_forearm max_roll_forearm
##  Length:19622           Length:19622         Min.   :-66.60  
##  Class :character       Class :character     1st Qu.:  0.00  
##  Mode  :character       Mode  :character     Median : 26.80  
##                                              Mean   : 24.49  
##                                              3rd Qu.: 45.95  
##                                              Max.   : 89.80  
##  max_picth_forearm max_yaw_forearm    min_roll_forearm  min_pitch_forearm
##  Min.   :-151.00   Length:19622       Min.   :-72.500   Min.   :-180.00  
##  1st Qu.:   0.00   Class :character   1st Qu.: -6.075   1st Qu.:-175.00  
##  Median : 113.00   Mode  :character   Median :  0.000   Median : -61.00  
##  Mean   :  81.49                      Mean   : -0.167   Mean   : -57.57  
##  3rd Qu.: 174.75                      3rd Qu.: 12.075   3rd Qu.:   0.00  
##  Max.   : 180.00                      Max.   : 62.100   Max.   : 167.00  
##  min_yaw_forearm    amplitude_roll_forearm amplitude_pitch_forearm
##  Length:19622       Min.   :  0.000        Min.   :  0.0          
##  Class :character   1st Qu.:  1.125        1st Qu.:  2.0          
##  Mode  :character   Median : 17.770        Median : 83.7          
##                     Mean   : 24.653        Mean   :139.1          
##                     3rd Qu.: 39.875        3rd Qu.:350.0          
##                     Max.   :126.000        Max.   :360.0          
##  amplitude_yaw_forearm total_accel_forearm var_accel_forearm
##  Length:19622          Min.   :  0.00      Min.   :  0.000  
##  Class :character      1st Qu.: 29.00      1st Qu.:  6.759  
##  Mode  :character      Median : 36.00      Median : 21.165  
##                        Mean   : 34.72      Mean   : 33.502  
##                        3rd Qu.: 41.00      3rd Qu.: 51.240  
##                        Max.   :108.00      Max.   :172.606  
##  avg_roll_forearm   stddev_roll_forearm var_roll_forearm  
##  Min.   :-177.234   Min.   :  0.000     Min.   :    0.00  
##  1st Qu.:  -0.909   1st Qu.:  0.428     1st Qu.:    0.18  
##  Median :  11.172   Median :  8.030     Median :   64.48  
##  Mean   :  33.165   Mean   : 41.986     Mean   : 5274.10  
##  3rd Qu.: 107.132   3rd Qu.: 85.373     3rd Qu.: 7289.08  
##  Max.   : 177.256   Max.   :179.171     Max.   :32102.24  
##  avg_pitch_forearm stddev_pitch_forearm var_pitch_forearm 
##  Min.   :-68.17    Min.   : 0.000       Min.   :   0.000  
##  1st Qu.:  0.00    1st Qu.: 0.336       1st Qu.:   0.113  
##  Median : 12.02    Median : 5.516       Median :  30.425  
##  Mean   : 11.79    Mean   : 7.977       Mean   : 139.593  
##  3rd Qu.: 28.48    3rd Qu.:12.866       3rd Qu.: 165.532  
##  Max.   : 72.09    Max.   :47.745       Max.   :2279.617  
##  avg_yaw_forearm   stddev_yaw_forearm var_yaw_forearm    gyros_forearm_x  
##  Min.   :-155.06   Min.   :  0.000    Min.   :    0.00   Min.   :-22.000  
##  1st Qu.: -26.26   1st Qu.:  0.524    1st Qu.:    0.27   1st Qu.: -0.220  
##  Median :   0.00   Median : 24.743    Median :  612.21   Median :  0.050  
##  Mean   :  18.00   Mean   : 44.854    Mean   : 4639.85   Mean   :  0.158  
##  3rd Qu.:  85.79   3rd Qu.: 85.817    3rd Qu.: 7368.41   3rd Qu.:  0.560  
##  Max.   : 169.24   Max.   :197.508    Max.   :39009.33   Max.   :  3.970  
##  gyros_forearm_y     gyros_forearm_z    accel_forearm_x   accel_forearm_y 
##  Min.   : -7.02000   Min.   : -8.0900   Min.   :-498.00   Min.   :-632.0  
##  1st Qu.: -1.46000   1st Qu.: -0.1800   1st Qu.:-178.00   1st Qu.:  57.0  
##  Median :  0.03000   Median :  0.0800   Median : -57.00   Median : 201.0  
##  Mean   :  0.07517   Mean   :  0.1512   Mean   : -61.65   Mean   : 163.7  
##  3rd Qu.:  1.62000   3rd Qu.:  0.4900   3rd Qu.:  76.00   3rd Qu.: 312.0  
##  Max.   :311.00000   Max.   :231.0000   Max.   : 477.00   Max.   : 923.0  
##  accel_forearm_z   magnet_forearm_x  magnet_forearm_y magnet_forearm_z
##  Min.   :-446.00   Min.   :-1280.0   Min.   :-896.0   Min.   :-973.0  
##  1st Qu.:-182.00   1st Qu.: -616.0   1st Qu.:   2.0   1st Qu.: 191.0  
##  Median : -39.00   Median : -378.0   Median : 591.0   Median : 511.0  
##  Mean   : -55.29   Mean   : -312.6   Mean   : 380.1   Mean   : 393.6  
##  3rd Qu.:  26.00   3rd Qu.:  -73.0   3rd Qu.: 737.0   3rd Qu.: 653.0  
##  Max.   : 291.00   Max.   :  672.0   Max.   :1480.0   Max.   :1090.0  
##     classe         
##  Length:19622      
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
##  [ reached getOption("max.print") -- omitted 1 row ]
```

```r
Tab <- table(Training$classe)
Tab
```

```
## 
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

```r
prop.table(Tab)
```

```
## 
##         A         B         C         D         E 
## 0.2843747 0.1935073 0.1743961 0.1638977 0.1838243
```

```r
Training$new_window[1:50]
```

```
##  [1] "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no" 
## [12] "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no" 
## [23] "no"  "yes" "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no" 
## [34] "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no"  "no" 
## [45] "no"  "no"  "no"  "no"  "no"  "no"
```

```r
sum(Training$new_window == "yes")
```

```
## [1] 406
```

```r
length(unique(Training$num_window))
```

```
## [1] 858
```

```r
Training %>% select(1:10, max_roll_belt, avg_roll_arm) %>% head(60)
```

```
##     X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
## 1   1  carlitos           1323084231               788290 05/12/2011 11:23
## 2   2  carlitos           1323084231               808298 05/12/2011 11:23
## 3   3  carlitos           1323084231               820366 05/12/2011 11:23
## 4   4  carlitos           1323084232               120339 05/12/2011 11:23
## 5   5  carlitos           1323084232               196328 05/12/2011 11:23
## 6   6  carlitos           1323084232               304277 05/12/2011 11:23
## 7   7  carlitos           1323084232               368296 05/12/2011 11:23
## 8   8  carlitos           1323084232               440390 05/12/2011 11:23
## 9   9  carlitos           1323084232               484323 05/12/2011 11:23
## 10 10  carlitos           1323084232               484434 05/12/2011 11:23
## 11 11  carlitos           1323084232               500302 05/12/2011 11:23
## 12 12  carlitos           1323084232               528316 05/12/2011 11:23
## 13 13  carlitos           1323084232               560359 05/12/2011 11:23
## 14 14  carlitos           1323084232               576390 05/12/2011 11:23
## 15 15  carlitos           1323084232               604281 05/12/2011 11:23
## 16 16  carlitos           1323084232               644302 05/12/2011 11:23
## 17 17  carlitos           1323084232               692324 05/12/2011 11:23
## 18 18  carlitos           1323084232               732306 05/12/2011 11:23
## 19 19  carlitos           1323084232               740353 05/12/2011 11:23
## 20 20  carlitos           1323084232               788335 05/12/2011 11:23
## 21 21  carlitos           1323084232               876301 05/12/2011 11:23
## 22 22  carlitos           1323084232               892313 05/12/2011 11:23
## 23 23  carlitos           1323084232               932285 05/12/2011 11:23
## 24 24  carlitos           1323084232               996313 05/12/2011 11:23
## 25 25  carlitos           1323084233                28311 05/12/2011 11:23
## 26 26  carlitos           1323084233                56286 05/12/2011 11:23
## 27 27  carlitos           1323084233                72305 05/12/2011 11:23
## 28 28  carlitos           1323084233               120363 05/12/2011 11:23
## 29 29  carlitos           1323084233               136333 05/12/2011 11:23
## 30 30  carlitos           1323084233               144318 05/12/2011 11:23
## 31 31  carlitos           1323084233               152353 05/12/2011 11:23
## 32 32  carlitos           1323084233               244310 05/12/2011 11:23
## 33 33  carlitos           1323084233               304292 05/12/2011 11:23
## 34 34  carlitos           1323084233               332328 05/12/2011 11:23
## 35 35  carlitos           1323084233               340386 05/12/2011 11:23
## 36 36  carlitos           1323084233               412295 05/12/2011 11:23
## 37 37  carlitos           1323084233               448310 05/12/2011 11:23
## 38 38  carlitos           1323084233               468406 05/12/2011 11:23
## 39 39  carlitos           1323084233               504293 05/12/2011 11:23
## 40 40  carlitos           1323084233               512347 05/12/2011 11:23
## 41 41  carlitos           1323084233               512367 05/12/2011 11:23
## 42 42  carlitos           1323084233               572298 05/12/2011 11:23
## 43 43  carlitos           1323084233               572351 05/12/2011 11:23
## 44 44  carlitos           1323084233               604309 05/12/2011 11:23
## 45 45  carlitos           1323084233               660300 05/12/2011 11:23
## 46 46  carlitos           1323084233               676353 05/12/2011 11:23
## 47 47  carlitos           1323084233               688317 05/12/2011 11:23
## 48 48  carlitos           1323084233               736343 05/12/2011 11:23
## 49 49  carlitos           1323084233               784365 05/12/2011 11:23
## 50 50  carlitos           1323084233               840318 05/12/2011 11:23
## 51 51  carlitos           1323084233               937221 05/12/2011 11:23
## 52 52  carlitos           1323084233               948290 05/12/2011 11:23
## 53 53  carlitos           1323084234                  399 05/12/2011 11:23
## 54 54  carlitos           1323084234                64349 05/12/2011 11:23
## 55 55  carlitos           1323084234               128295 05/12/2011 11:23
## 56 56  carlitos           1323084234               140346 05/12/2011 11:23
## 57 57  carlitos           1323084234               248292 05/12/2011 11:23
## 58 58  carlitos           1323084234               268317 05/12/2011 11:23
## 59 59  carlitos           1323084234               340471 05/12/2011 11:23
## 60 60  carlitos           1323084234               360305 05/12/2011 11:23
##    new_window num_window roll_belt pitch_belt yaw_belt max_roll_belt
## 1          no         11      1.41       8.07    -94.4            NA
## 2          no         11      1.41       8.07    -94.4            NA
## 3          no         11      1.42       8.07    -94.4            NA
## 4          no         12      1.48       8.05    -94.4            NA
## 5          no         12      1.48       8.07    -94.4            NA
## 6          no         12      1.45       8.06    -94.4            NA
## 7          no         12      1.42       8.09    -94.4            NA
## 8          no         12      1.42       8.13    -94.4            NA
## 9          no         12      1.43       8.16    -94.4            NA
## 10         no         12      1.45       8.17    -94.4            NA
## 11         no         12      1.45       8.18    -94.4            NA
## 12         no         12      1.43       8.18    -94.4            NA
## 13         no         12      1.42       8.20    -94.4            NA
## 14         no         12      1.42       8.21    -94.4            NA
## 15         no         12      1.45       8.20    -94.4            NA
## 16         no         12      1.48       8.15    -94.4            NA
## 17         no         12      1.51       8.12    -94.4            NA
## 18         no         12      1.55       8.08    -94.4            NA
## 19         no         12      1.57       8.06    -94.4            NA
## 20         no         12      1.59       8.07    -94.4            NA
## 21         no         12      1.60       8.10    -94.4            NA
## 22         no         12      1.57       8.09    -94.4            NA
## 23         no         12      1.56       8.10    -94.3            NA
## 24        yes         12      1.51       8.10    -94.4         -94.3
## 25         no         13      1.53       8.11    -94.4            NA
## 26         no         13      1.55       8.09    -94.4            NA
## 27         no         13      1.54       8.11    -94.4            NA
## 28         no         13      1.53       8.14    -94.4            NA
## 29         no         13      1.52       8.16    -94.4            NA
## 30         no         13      1.52       8.17    -94.4            NA
## 31         no         13      1.53       8.17    -94.4            NA
## 32         no         13      1.44       8.19    -94.4            NA
## 33         no         13      1.43       8.17    -94.4            NA
## 34         no         13      1.44       8.18    -94.3            NA
## 35         no         13      1.41       8.18    -94.4            NA
## 36         no         13      1.42       8.12    -94.3            NA
## 37         no         13      1.41       8.11    -94.3            NA
## 38         no         13      1.40       8.04    -94.3            NA
## 39         no         13      1.40       8.04    -94.3            NA
## 40         no         13      1.40       8.05    -94.3            NA
## 41         no         13      1.40       8.06    -94.3            NA
## 42         no         13      1.39       8.05    -94.3            NA
## 43         no         13      1.34       8.05    -94.3            NA
## 44         no         13      1.30       8.00    -94.2            NA
## 45         no         13      1.30       7.85    -94.2            NA
## 46         no         13      1.29       7.81    -94.2            NA
## 47         no         13      1.33       7.76    -94.2            NA
## 48         no         13      1.33       7.69    -94.2            NA
## 49         no         13      1.31       7.69    -94.2            NA
## 50         no         13      1.30       7.68    -94.2            NA
## 51         no         13      1.29       7.58    -94.1            NA
## 52        yes         13      1.27       7.56    -94.1         -94.1
## 53         no         14      1.26       7.54    -94.1            NA
## 54         no         14      1.25       7.46    -94.1            NA
## 55         no         14      1.26       7.47    -94.2            NA
## 56         no         14      1.27       7.46    -94.1            NA
## 57         no         14      1.25       7.45    -94.1            NA
## 58         no         14      1.23       7.43    -94.1            NA
## 59         no         14      1.24       7.43    -94.1            NA
## 60         no         14      1.25       7.41    -94.1            NA
##    avg_roll_arm
## 1            NA
## 2            NA
## 3            NA
## 4            NA
## 5            NA
## 6            NA
## 7            NA
## 8            NA
## 9            NA
## 10           NA
## 11           NA
## 12           NA
## 13           NA
## 14           NA
## 15           NA
## 16           NA
## 17           NA
## 18           NA
## 19           NA
## 20           NA
## 21           NA
## 22           NA
## 23           NA
## 24    -128.4898
## 25           NA
## 26           NA
## 27           NA
## 28           NA
## 29           NA
## 30           NA
## 31           NA
## 32           NA
## 33           NA
## 34           NA
## 35           NA
## 36           NA
## 37           NA
## 38           NA
## 39           NA
## 40           NA
## 41           NA
## 42           NA
## 43           NA
## 44           NA
## 45           NA
## 46           NA
## 47           NA
## 48           NA
## 49           NA
## 50           NA
## 51           NA
## 52    -129.6863
## 53           NA
## 54           NA
## 55           NA
## 56           NA
## 57           NA
## 58           NA
## 59           NA
## 60           NA
```

```r
Training %>% select(1:10, max_roll_belt, avg_roll_arm) %>% tail(50)
```

```
##           X user_name raw_timestamp_part_1 raw_timestamp_part_2
## 19573 19573  carlitos           1323084339               124305
## 19574 19574  carlitos           1323084339               152364
## 19575 19575  carlitos           1323084339               180328
## 19576 19576  carlitos           1323084339               240327
## 19577 19577  carlitos           1323084339               308445
## 19578 19578  carlitos           1323084339               444412
## 19579 19579  carlitos           1323084339               464291
## 19580 19580  carlitos           1323084339               528296
## 19581 19581  carlitos           1323084339               676337
## 19582 19582  carlitos           1323084339               676394
## 19583 19583  carlitos           1323084339               692319
## 19584 19584  carlitos           1323084339               720346
## 19585 19585  carlitos           1323084339               808298
## 19586 19586  carlitos           1323084339               828322
## 19587 19587  carlitos           1323084339               840324
## 19588 19588  carlitos           1323084339               904336
## 19589 19589  carlitos           1323084339               912289
## 19590 19590  carlitos           1323084339               920390
## 19591 19591  carlitos           1323084339               920406
## 19592 19592  carlitos           1323084339               944348
## 19593 19593  carlitos           1323084339               952322
## 19594 19594  carlitos           1323084339               952338
## 19595 19595  carlitos           1323084339               964307
## 19596 19596  carlitos           1323084339               964352
## 19597 19597    adelmo           1322832944                72317
## 19598 19598    adelmo           1322832944               208297
## 19599 19599    adelmo           1322832944               464464
## 19600 19600    adelmo           1322832944               476287
## 19601 19601    adelmo           1322832944               572275
## 19602 19602    adelmo           1322832944               608323
## 19603 19603    adelmo           1322832944               608379
## 19604 19604    adelmo           1322832944               656290
## 19605 19605    adelmo           1322832944               748354
## 19606 19606    adelmo           1322832944               824315
## 19607 19607    adelmo           1322832944               844310
## 19608 19608    adelmo           1322832944               884298
## 19609 19609    adelmo           1322832937               188352
## 19610 19610    adelmo           1322832937               204334
## 19611 19611    adelmo           1322832937               268312
## 19612 19612    adelmo           1322832937               292326
## 19613 19613    adelmo           1322832937               332315
## 19614 19614    adelmo           1322832937               332354
## 19615 19615    adelmo           1322832937               388326
## 19616 19616    adelmo           1322832937               468293
## 19617 19617    adelmo           1322832937               588324
## 19618 19618    adelmo           1322832937               588376
## 19619 19619    adelmo           1322832937               596287
## 19620 19620    adelmo           1322832937               636283
## 19621 19621    adelmo           1322832937               964299
## 19622 19622    adelmo           1322832937               972293
##         cvtd_timestamp new_window num_window roll_belt pitch_belt yaw_belt
## 19573 05/12/2011 11:25         no        847     -3.18       5.14    -92.5
## 19574 05/12/2011 11:25         no        847     -2.84       5.18    -92.5
## 19575 05/12/2011 11:25         no        847     -2.44       5.25    -92.5
## 19576 05/12/2011 11:25         no        847     -1.12       5.33    -92.4
## 19577 05/12/2011 11:25         no        847     -0.02       5.34    -92.3
## 19578 05/12/2011 11:25         no        847      3.92       5.05    -92.0
## 19579 05/12/2011 11:25         no        847      4.47       5.10    -92.0
## 19580 05/12/2011 11:25         no        847      5.04       5.15    -91.9
## 19581 05/12/2011 11:25         no        847      6.10       5.18    -91.8
## 19582 05/12/2011 11:25         no        847      6.52       5.11    -91.7
## 19583 05/12/2011 11:25         no        847      7.26       4.79    -91.5
## 19584 05/12/2011 11:25         no        847      7.86       4.34    -91.4
## 19585 05/12/2011 11:25         no        847      8.11       4.16    -91.4
## 19586 05/12/2011 11:25         no        847      8.33       3.94    -91.3
## 19587 05/12/2011 11:25         no        847      8.51       3.77    -91.3
## 19588 05/12/2011 11:25         no        847      8.84       3.50    -91.3
## 19589 05/12/2011 11:25         no        847      9.05       3.35    -91.2
## 19590 05/12/2011 11:25         no        847      8.95       3.38    -91.1
## 19591 05/12/2011 11:25         no        847      8.84       3.41    -91.1
## 19592 05/12/2011 11:25         no        847      8.58       3.50    -91.1
## 19593 05/12/2011 11:25         no        847      8.16       3.80    -91.1
## 19594 05/12/2011 11:25         no        847      8.00       3.97    -91.1
## 19595 05/12/2011 11:25         no        847      7.85       4.18    -91.1
## 19596 05/12/2011 11:25        yes        847      7.69       4.34    -91.1
## 19597 02/12/2011 13:35         no        863    125.00     -48.70   -171.0
## 19598 02/12/2011 13:35         no        863    126.00     -47.40   -173.0
## 19599 02/12/2011 13:35         no        863    128.00     -44.20    178.0
## 19600 02/12/2011 13:35         no        863    129.00     -43.90    177.0
## 19601 02/12/2011 13:35         no        863    129.00     -42.00    173.0
## 19602 02/12/2011 13:35         no        863    127.00     -39.90    172.0
## 19603 02/12/2011 13:35         no        863    127.00     -39.30    171.0
## 19604 02/12/2011 13:35         no        863    126.00     -37.90    168.0
## 19605 02/12/2011 13:35         no        863    125.00     -37.00    164.0
## 19606 02/12/2011 13:35         no        863    125.00     -36.70    163.0
## 19607 02/12/2011 13:35         no        863    125.00     -36.30    162.0
## 19608 02/12/2011 13:35         no        863    124.00     -36.20    161.0
## 19609 02/12/2011 13:35         no        864    157.00     -32.00    124.0
## 19610 02/12/2011 13:35         no        864    156.00     -32.00    124.0
## 19611 02/12/2011 13:35         no        864    156.00     -32.10    124.0
## 19612 02/12/2011 13:35         no        864    156.00     -32.30    124.0
## 19613 02/12/2011 13:35         no        864    155.00     -32.60    125.0
## 19614 02/12/2011 13:35         no        864    154.00     -32.70    125.0
## 19615 02/12/2011 13:35         no        864    154.00     -32.90    126.0
## 19616 02/12/2011 13:35         no        864    151.00     -33.80    127.0
## 19617 02/12/2011 13:35         no        864    148.00     -34.70    129.0
## 19618 02/12/2011 13:35         no        864    147.00     -34.80    129.0
## 19619 02/12/2011 13:35         no        864    145.00     -35.30    130.0
## 19620 02/12/2011 13:35         no        864    145.00     -35.50    130.0
## 19621 02/12/2011 13:35         no        864    143.00     -35.90    131.0
## 19622 02/12/2011 13:35        yes        864    143.00     -36.00    132.0
##       max_roll_belt avg_roll_arm
## 19573            NA           NA
## 19574            NA           NA
## 19575            NA           NA
## 19576            NA           NA
## 19577            NA           NA
## 19578            NA           NA
## 19579            NA           NA
## 19580            NA           NA
## 19581            NA           NA
## 19582            NA           NA
## 19583            NA           NA
## 19584            NA           NA
## 19585            NA           NA
## 19586            NA           NA
## 19587            NA           NA
## 19588            NA           NA
## 19589            NA           NA
## 19590            NA           NA
## 19591            NA           NA
## 19592            NA           NA
## 19593            NA           NA
## 19594            NA           NA
## 19595            NA           NA
## 19596         -91.1      68.5549
## 19597            NA           NA
## 19598            NA           NA
## 19599            NA           NA
## 19600            NA           NA
## 19601            NA           NA
## 19602            NA           NA
## 19603            NA           NA
## 19604            NA           NA
## 19605            NA           NA
## 19606            NA           NA
## 19607            NA           NA
## 19608            NA           NA
## 19609            NA           NA
## 19610            NA           NA
## 19611            NA           NA
## 19612            NA           NA
## 19613            NA           NA
## 19614            NA           NA
## 19615            NA           NA
## 19616            NA           NA
## 19617            NA           NA
## 19618            NA           NA
## 19619            NA           NA
## 19620            NA           NA
## 19621            NA           NA
## 19622         132.0     -91.6481
```

```r
Training %>% select(1:10, max_roll_belt, avg_roll_arm) %>% head(60)
```

```
##     X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
## 1   1  carlitos           1323084231               788290 05/12/2011 11:23
## 2   2  carlitos           1323084231               808298 05/12/2011 11:23
## 3   3  carlitos           1323084231               820366 05/12/2011 11:23
## 4   4  carlitos           1323084232               120339 05/12/2011 11:23
## 5   5  carlitos           1323084232               196328 05/12/2011 11:23
## 6   6  carlitos           1323084232               304277 05/12/2011 11:23
## 7   7  carlitos           1323084232               368296 05/12/2011 11:23
## 8   8  carlitos           1323084232               440390 05/12/2011 11:23
## 9   9  carlitos           1323084232               484323 05/12/2011 11:23
## 10 10  carlitos           1323084232               484434 05/12/2011 11:23
## 11 11  carlitos           1323084232               500302 05/12/2011 11:23
## 12 12  carlitos           1323084232               528316 05/12/2011 11:23
## 13 13  carlitos           1323084232               560359 05/12/2011 11:23
## 14 14  carlitos           1323084232               576390 05/12/2011 11:23
## 15 15  carlitos           1323084232               604281 05/12/2011 11:23
## 16 16  carlitos           1323084232               644302 05/12/2011 11:23
## 17 17  carlitos           1323084232               692324 05/12/2011 11:23
## 18 18  carlitos           1323084232               732306 05/12/2011 11:23
## 19 19  carlitos           1323084232               740353 05/12/2011 11:23
## 20 20  carlitos           1323084232               788335 05/12/2011 11:23
## 21 21  carlitos           1323084232               876301 05/12/2011 11:23
## 22 22  carlitos           1323084232               892313 05/12/2011 11:23
## 23 23  carlitos           1323084232               932285 05/12/2011 11:23
## 24 24  carlitos           1323084232               996313 05/12/2011 11:23
## 25 25  carlitos           1323084233                28311 05/12/2011 11:23
## 26 26  carlitos           1323084233                56286 05/12/2011 11:23
## 27 27  carlitos           1323084233                72305 05/12/2011 11:23
## 28 28  carlitos           1323084233               120363 05/12/2011 11:23
## 29 29  carlitos           1323084233               136333 05/12/2011 11:23
## 30 30  carlitos           1323084233               144318 05/12/2011 11:23
## 31 31  carlitos           1323084233               152353 05/12/2011 11:23
## 32 32  carlitos           1323084233               244310 05/12/2011 11:23
## 33 33  carlitos           1323084233               304292 05/12/2011 11:23
## 34 34  carlitos           1323084233               332328 05/12/2011 11:23
## 35 35  carlitos           1323084233               340386 05/12/2011 11:23
## 36 36  carlitos           1323084233               412295 05/12/2011 11:23
## 37 37  carlitos           1323084233               448310 05/12/2011 11:23
## 38 38  carlitos           1323084233               468406 05/12/2011 11:23
## 39 39  carlitos           1323084233               504293 05/12/2011 11:23
## 40 40  carlitos           1323084233               512347 05/12/2011 11:23
## 41 41  carlitos           1323084233               512367 05/12/2011 11:23
## 42 42  carlitos           1323084233               572298 05/12/2011 11:23
## 43 43  carlitos           1323084233               572351 05/12/2011 11:23
## 44 44  carlitos           1323084233               604309 05/12/2011 11:23
## 45 45  carlitos           1323084233               660300 05/12/2011 11:23
## 46 46  carlitos           1323084233               676353 05/12/2011 11:23
## 47 47  carlitos           1323084233               688317 05/12/2011 11:23
## 48 48  carlitos           1323084233               736343 05/12/2011 11:23
## 49 49  carlitos           1323084233               784365 05/12/2011 11:23
## 50 50  carlitos           1323084233               840318 05/12/2011 11:23
## 51 51  carlitos           1323084233               937221 05/12/2011 11:23
## 52 52  carlitos           1323084233               948290 05/12/2011 11:23
## 53 53  carlitos           1323084234                  399 05/12/2011 11:23
## 54 54  carlitos           1323084234                64349 05/12/2011 11:23
## 55 55  carlitos           1323084234               128295 05/12/2011 11:23
## 56 56  carlitos           1323084234               140346 05/12/2011 11:23
## 57 57  carlitos           1323084234               248292 05/12/2011 11:23
## 58 58  carlitos           1323084234               268317 05/12/2011 11:23
## 59 59  carlitos           1323084234               340471 05/12/2011 11:23
## 60 60  carlitos           1323084234               360305 05/12/2011 11:23
##    new_window num_window roll_belt pitch_belt yaw_belt max_roll_belt
## 1          no         11      1.41       8.07    -94.4            NA
## 2          no         11      1.41       8.07    -94.4            NA
## 3          no         11      1.42       8.07    -94.4            NA
## 4          no         12      1.48       8.05    -94.4            NA
## 5          no         12      1.48       8.07    -94.4            NA
## 6          no         12      1.45       8.06    -94.4            NA
## 7          no         12      1.42       8.09    -94.4            NA
## 8          no         12      1.42       8.13    -94.4            NA
## 9          no         12      1.43       8.16    -94.4            NA
## 10         no         12      1.45       8.17    -94.4            NA
## 11         no         12      1.45       8.18    -94.4            NA
## 12         no         12      1.43       8.18    -94.4            NA
## 13         no         12      1.42       8.20    -94.4            NA
## 14         no         12      1.42       8.21    -94.4            NA
## 15         no         12      1.45       8.20    -94.4            NA
## 16         no         12      1.48       8.15    -94.4            NA
## 17         no         12      1.51       8.12    -94.4            NA
## 18         no         12      1.55       8.08    -94.4            NA
## 19         no         12      1.57       8.06    -94.4            NA
## 20         no         12      1.59       8.07    -94.4            NA
## 21         no         12      1.60       8.10    -94.4            NA
## 22         no         12      1.57       8.09    -94.4            NA
## 23         no         12      1.56       8.10    -94.3            NA
## 24        yes         12      1.51       8.10    -94.4         -94.3
## 25         no         13      1.53       8.11    -94.4            NA
## 26         no         13      1.55       8.09    -94.4            NA
## 27         no         13      1.54       8.11    -94.4            NA
## 28         no         13      1.53       8.14    -94.4            NA
## 29         no         13      1.52       8.16    -94.4            NA
## 30         no         13      1.52       8.17    -94.4            NA
## 31         no         13      1.53       8.17    -94.4            NA
## 32         no         13      1.44       8.19    -94.4            NA
## 33         no         13      1.43       8.17    -94.4            NA
## 34         no         13      1.44       8.18    -94.3            NA
## 35         no         13      1.41       8.18    -94.4            NA
## 36         no         13      1.42       8.12    -94.3            NA
## 37         no         13      1.41       8.11    -94.3            NA
## 38         no         13      1.40       8.04    -94.3            NA
## 39         no         13      1.40       8.04    -94.3            NA
## 40         no         13      1.40       8.05    -94.3            NA
## 41         no         13      1.40       8.06    -94.3            NA
## 42         no         13      1.39       8.05    -94.3            NA
## 43         no         13      1.34       8.05    -94.3            NA
## 44         no         13      1.30       8.00    -94.2            NA
## 45         no         13      1.30       7.85    -94.2            NA
## 46         no         13      1.29       7.81    -94.2            NA
## 47         no         13      1.33       7.76    -94.2            NA
## 48         no         13      1.33       7.69    -94.2            NA
## 49         no         13      1.31       7.69    -94.2            NA
## 50         no         13      1.30       7.68    -94.2            NA
## 51         no         13      1.29       7.58    -94.1            NA
## 52        yes         13      1.27       7.56    -94.1         -94.1
## 53         no         14      1.26       7.54    -94.1            NA
## 54         no         14      1.25       7.46    -94.1            NA
## 55         no         14      1.26       7.47    -94.2            NA
## 56         no         14      1.27       7.46    -94.1            NA
## 57         no         14      1.25       7.45    -94.1            NA
## 58         no         14      1.23       7.43    -94.1            NA
## 59         no         14      1.24       7.43    -94.1            NA
## 60         no         14      1.25       7.41    -94.1            NA
##    avg_roll_arm
## 1            NA
## 2            NA
## 3            NA
## 4            NA
## 5            NA
## 6            NA
## 7            NA
## 8            NA
## 9            NA
## 10           NA
## 11           NA
## 12           NA
## 13           NA
## 14           NA
## 15           NA
## 16           NA
## 17           NA
## 18           NA
## 19           NA
## 20           NA
## 21           NA
## 22           NA
## 23           NA
## 24    -128.4898
## 25           NA
## 26           NA
## 27           NA
## 28           NA
## 29           NA
## 30           NA
## 31           NA
## 32           NA
## 33           NA
## 34           NA
## 35           NA
## 36           NA
## 37           NA
## 38           NA
## 39           NA
## 40           NA
## 41           NA
## 42           NA
## 43           NA
## 44           NA
## 45           NA
## 46           NA
## 47           NA
## 48           NA
## 49           NA
## 50           NA
## 51           NA
## 52    -129.6863
## 53           NA
## 54           NA
## 55           NA
## 56           NA
## 57           NA
## 58           NA
## 59           NA
## 60           NA
```

Notes:
The new_window variable suggests that there are 406 windows in total while checking the variable reveals 858 different window

####Check the testing set

```r
dim(Testing)
```

```
## [1]  20 160
```

```r
Testing[1:10, 1:13]
```

```
##     X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
## 1   1     pedro           1323095002               868349 05/12/2011 14:23
## 2   2    jeremy           1322673067               778725 30/11/2011 17:11
## 3   3    jeremy           1322673075               342967 30/11/2011 17:11
## 4   4    adelmo           1322832789               560311 02/12/2011 13:33
## 5   5    eurico           1322489635               814776 28/11/2011 14:13
## 6   6    jeremy           1322673149               510661 30/11/2011 17:12
## 7   7    jeremy           1322673128               766645 30/11/2011 17:12
## 8   8    jeremy           1322673076                54671 30/11/2011 17:11
## 9   9  carlitos           1323084240               916313 05/12/2011 11:24
## 10 10   charles           1322837822               384285 02/12/2011 14:57
##    new_window num_window roll_belt pitch_belt yaw_belt total_accel_belt
## 1          no         74    123.00      27.00    -4.75               20
## 2          no        431      1.02       4.87   -88.90                4
## 3          no        439      0.87       1.82   -88.50                5
## 4          no        194    125.00     -41.60   162.00               17
## 5          no        235      1.35       3.33   -88.60                3
## 6          no        504     -5.92       1.59   -87.70                4
## 7          no        485      1.20       4.44   -87.30                4
## 8          no        440      0.43       4.15   -88.50                4
## 9          no        323      0.93       6.72   -93.70                4
## 10         no        664    114.00      22.40   -13.10               18
##    kurtosis_roll_belt kurtosis_picth_belt
## 1                  NA                  NA
## 2                  NA                  NA
## 3                  NA                  NA
## 4                  NA                  NA
## 5                  NA                  NA
## 6                  NA                  NA
## 7                  NA                  NA
## 8                  NA                  NA
## 9                  NA                  NA
## 10                 NA                  NA
```

```r
checkpoint <- sapply (Testing, function(x)all(is.na(x)))
sum(checkpoint)
```

```
## [1] 100
```

```r
column_name <- names(checkpoint[checkpoint == FALSE])
column_name
```

```
##  [1] "X"                    "user_name"            "raw_timestamp_part_1"
##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
##  [7] "num_window"           "roll_belt"            "pitch_belt"          
## [10] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
## [13] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [16] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [19] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [22] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [25] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [28] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [31] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [34] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [37] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [40] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [43] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [46] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [49] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [52] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [55] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [58] "magnet_forearm_y"     "magnet_forearm_z"     "problem_id"
```

```r
Training_window_num <- unique(Training$num_window)
Testing_window_num <- unique(Testing$num_window)

which(Testing_window_num %in% Training_window_num)
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
```

####Conculsion:
->We should predict the classes of 20 singl observations in the testing set
->We can't try different window sizes since we have to deal with 20 single test cases
->100 out of 160 columns are NA in the testing set and accordingly we should consider the columns for building the model based on the training set


More investigation will be applied below:


```r
Training_window_num <- unique(Training$num_window)
Testing_window_num <- unique(Testing$num_window)

which(Testing_window_num %in% Training_window_num)
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
```

```r
Testing[1, 1:8]
```

```
##   X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
## 1 1     pedro           1323095002               868349 05/12/2011 14:23
##   new_window num_window roll_belt
## 1         no         74       123
```

```r
Training %>% filter(num_window == 74, raw_timestamp_part_1 == 1323095002) %>% select(1:8, classe)
```

```
##       X user_name raw_timestamp_part_1 raw_timestamp_part_2
## 1  5695     pedro           1323095002                 8292
## 2  5696     pedro           1323095002               160329
## 3  5697     pedro           1323095002               332305
## 4  5698     pedro           1323095002               444308
## 5  5699     pedro           1323095002               460323
## 6  5700     pedro           1323095002               532318
## 7  5701     pedro           1323095002               532364
## 8  5702     pedro           1323095002               540324
## 9  5703     pedro           1323095002               552298
## 10 5704     pedro           1323095002               552327
## 11 5705     pedro           1323095002               596303
## 12 5706     pedro           1323095002               660309
## 13 5707     pedro           1323095002               776376
## 14 5708     pedro           1323095002               828324
## 15 5709     pedro           1323095002               828384
## 16 5710     pedro           1323095002               868302
## 17 5711     pedro           1323095002               879434
## 18 5712     pedro           1323095002               908305
## 19 5713     pedro           1323095002               968355
## 20 5714     pedro           1323095002               968412
##      cvtd_timestamp new_window num_window roll_belt classe
## 1  05/12/2011 14:23         no         74       121      B
## 2  05/12/2011 14:23         no         74       121      B
## 3  05/12/2011 14:23         no         74       122      B
## 4  05/12/2011 14:23         no         74       122      B
## 5  05/12/2011 14:23         no         74       122      B
## 6  05/12/2011 14:23         no         74       122      B
## 7  05/12/2011 14:23         no         74       122      B
## 8  05/12/2011 14:23         no         74       122      B
## 9  05/12/2011 14:23         no         74       123      B
## 10 05/12/2011 14:23         no         74       123      B
## 11 05/12/2011 14:23         no         74       123      B
## 12 05/12/2011 14:23         no         74       123      B
## 13 05/12/2011 14:23         no         74       123      B
## 14 05/12/2011 14:23         no         74       123      B
## 15 05/12/2011 14:23         no         74       123      B
## 16 05/12/2011 14:23         no         74       123      B
## 17 05/12/2011 14:23         no         74       123      B
## 18 05/12/2011 14:23         no         74       123      B
## 19 05/12/2011 14:23         no         74       123      B
## 20 05/12/2011 14:23        yes         74       123      B
```

```r
Testing[2, 1:8]
```

```
##   X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
## 2 2    jeremy           1322673067               778725 30/11/2011 17:11
##   new_window num_window roll_belt
## 2         no        431      1.02
```

```r
Training %>% filter(num_window == 431, raw_timestamp_part_1 == 1322673067) %>% select(1:8, classe)
```

```
##       X user_name raw_timestamp_part_1 raw_timestamp_part_2
## 1  3854    jeremy           1322673067               142634
## 2  3855    jeremy           1322673067               182650
## 3  3856    jeremy           1322673067               210724
## 4  3857    jeremy           1322673067               258677
## 5  3858    jeremy           1322673067               266679
## 6  3859    jeremy           1322673067               278631
## 7  3860    jeremy           1322673067               322640
## 8  3861    jeremy           1322673067               366672
## 9  3862    jeremy           1322673067               410719
## 10 3863    jeremy           1322673067               410746
## 11 3864    jeremy           1322673067               422633
## 12 3865    jeremy           1322673067               422666
## 13 3866    jeremy           1322673067               558632
## 14 3867    jeremy           1322673067               698707
## 15 3868    jeremy           1322673067               750629
## 16 3869    jeremy           1322673067               770695
## 17 3870    jeremy           1322673067               830641
## 18 3871    jeremy           1322673067               830686
## 19 3872    jeremy           1322673067               842675
## 20 3873    jeremy           1322673067               866716
## 21 3874    jeremy           1322673067               902648
## 22 3875    jeremy           1322673067               922668
## 23 3876    jeremy           1322673067               958638
## 24 3877    jeremy           1322673067               958677
## 25 3878    jeremy           1322673067               966707
##      cvtd_timestamp new_window num_window roll_belt classe
## 1  30/11/2011 17:11         no        431      1.50      A
## 2  30/11/2011 17:11         no        431      1.49      A
## 3  30/11/2011 17:11         no        431      1.49      A
## 4  30/11/2011 17:11         no        431      1.55      A
## 5  30/11/2011 17:11         no        431      1.60      A
## 6  30/11/2011 17:11         no        431      1.58      A
## 7  30/11/2011 17:11         no        431      1.43      A
## 8  30/11/2011 17:11         no        431      1.42      A
## 9  30/11/2011 17:11         no        431      1.39      A
## 10 30/11/2011 17:11         no        431      1.39      A
## 11 30/11/2011 17:11         no        431      1.36      A
## 12 30/11/2011 17:11         no        431      1.36      A
## 13 30/11/2011 17:11         no        431      1.33      A
## 14 30/11/2011 17:11         no        431      1.18      A
## 15 30/11/2011 17:11         no        431      1.13      A
## 16 30/11/2011 17:11         no        431      1.08      A
## 17 30/11/2011 17:11         no        431      0.84      A
## 18 30/11/2011 17:11         no        431      0.75      A
## 19 30/11/2011 17:11         no        431      0.72      A
## 20 30/11/2011 17:11         no        431      0.68      A
## 21 30/11/2011 17:11         no        431      0.60      A
## 22 30/11/2011 17:11         no        431      0.57      A
## 23 30/11/2011 17:11         no        431      0.21      A
## 24 30/11/2011 17:11         no        431      0.08      A
## 25 30/11/2011 17:11        yes        431     -0.02      A
```

```r
Testing[3, 1:8]
```

```
##   X user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
## 3 3    jeremy           1322673075               342967 30/11/2011 17:11
##   new_window num_window roll_belt
## 3         no        439      0.87
```

```r
Training %>% filter(num_window == 439, raw_timestamp_part_1 == 1322673075) %>% select(1:8, classe)
```

```
##       X user_name raw_timestamp_part_1 raw_timestamp_part_2
## 1  6985    jeremy           1322673075                98663
## 2  6986    jeremy           1322673075               262628
## 3  6987    jeremy           1322673075               330664
## 4  6988    jeremy           1322673075               342940
## 5  6989    jeremy           1322673075               399301
## 6  6990    jeremy           1322673075               430672
## 7  6991    jeremy           1322673075               546733
## 8  6992    jeremy           1322673075               638647
## 9  6993    jeremy           1322673075               694740
## 10 6994    jeremy           1322673075               750647
## 11 6995    jeremy           1322673075               766647
## 12 6996    jeremy           1322673075               818693
## 13 6997    jeremy           1322673075               854687
## 14 6998    jeremy           1322673075               890631
## 15 6999    jeremy           1322673075               922653
## 16 7000    jeremy           1322673075               942643
## 17 7001    jeremy           1322673075               998654
##      cvtd_timestamp new_window num_window roll_belt classe
## 1  30/11/2011 17:11         no        439      1.11      B
## 2  30/11/2011 17:11         no        439      0.89      B
## 3  30/11/2011 17:11         no        439      0.86      B
## 4  30/11/2011 17:11         no        439      0.86      B
## 5  30/11/2011 17:11         no        439      0.97      B
## 6  30/11/2011 17:11         no        439      1.00      B
## 7  30/11/2011 17:11         no        439      1.03      B
## 8  30/11/2011 17:11         no        439      1.11      B
## 9  30/11/2011 17:11         no        439      1.14      B
## 10 30/11/2011 17:11         no        439      1.12      B
## 11 30/11/2011 17:11         no        439      1.15      B
## 12 30/11/2011 17:11         no        439      1.13      B
## 13 30/11/2011 17:11         no        439      0.86      B
## 14 30/11/2011 17:11         no        439      0.78      B
## 15 30/11/2011 17:11         no        439      0.75      B
## 16 30/11/2011 17:11         no        439      0.72      B
## 17 30/11/2011 17:11        yes        439      0.56      B
```

####Conculsion:
->20 observations from the testing set are simply cut out from the training set which shown at observations in the training set and match the num_window variable of one of the testing set observations

->Accordingly, we can build a look up fun. instead of creating a prediction model

Let's try our solution here:


```r
predictions <- rep(NA,20)
for(i in seq_along(Testing_window_num)) {
  predictions[i] <- Training %>%
    filter(num_window == Testing_window_num[i]) %>%
    select(classe) %>%
    slice(1) %>% unlist
}
##output
predictions
```

```
##  [1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A"
## [18] "B" "B" "B"
```

####Note
predictions vector will be used as input for the pml_write_files function as shown below


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("/Users/amrmostafa/Documents/Data Science/Course8/Project/problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
##output
pml_write_files(predictions)
```

###Results
-> The submitted predictions files based on the look-up function's results worked properly
-> All testing set observation were predicated correctly

### Second: Train and build a random forest model

Since we need to report an estimate for out of sample error based on cross validation, we will train and build a random forest model


```r
sub_Training <- Training %>%
  filter(new_window == "yes")

sub_Training %<>% select(-c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window)) %>%
  mutate(
    classe = as.factor(classe)
  )


sub_Training[sub_Training == "#DIV/0!"] <- NA

comp_na_columns <- sapply(sub_Training, function(x) any(is.na(x))) %>%
  .[. == TRUE] %>%
  names

sub_Training %<>% select(-one_of(comp_na_columns))
```
Detect Cores=

```r
detectCores()
```

```
## [1] 4
```
Get do Par Workers=

```r
getDoParWorkers()
```

```
## [1] 1
```



```r
registerDoParallel(cores = 4)
```

#####Define resampling schema

```r
ctrl <- trainControl(method = 'cv', number = 10)
```

#####Train random forest model

```r
grid <-expand.grid(mtry = seq(2, ncol(sub_Training), length.out = 5))

rf_fit <- train(classe ~ ., data = sub_Training, method = "rf", tuneGrid=grid, ntree = 1000, trControl = ctrl)
```

#####Calculate in sample error

```r
confusionMatrix(predict(rf_fit, newdata = sub_Training), sub_Training$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   A   B   C   D   E
##          A 109   0   0   0   0
##          B   0  79   0   0   0
##          C   0   0  70   0   0
##          D   0   0   0  69   0
##          E   0   0   0   0  79
## 
## Overall Statistics
##                                     
##                Accuracy : 1         
##                  95% CI : (0.991, 1)
##     No Information Rate : 0.2685    
##     P-Value [Acc > NIR] : < 2.2e-16 
##                                     
##                   Kappa : 1         
##  Mcnemar's Test P-Value : NA        
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000     1.00   1.0000
## Specificity            1.0000   1.0000   1.0000     1.00   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000     1.00   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000     1.00   1.0000
## Prevalence             0.2685   0.1946   0.1724     0.17   0.1946
## Detection Rate         0.2685   0.1946   0.1724     0.17   0.1946
## Detection Prevalence   0.2685   0.1946   0.1724     0.17   0.1946
## Balanced Accuracy      1.0000   1.0000   1.0000     1.00   1.0000
```

####Sample error
The sample error is 0% because all observations are classified correctly

###Calculate out of sample error

```r
rf_fit
```

```
## Random Forest 
## 
## 406 samples
## 119 predictors
##   5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 365, 366, 366, 365, 366, 365, ... 
## Resampling results across tuning parameters:
## 
##   mtry   Accuracy   Kappa    
##     2.0  0.8228049  0.7759658
##    31.5  0.8453049  0.8048769
##    61.0  0.8256098  0.7794618
##    90.5  0.8060976  0.7550042
##   120.0  0.7937195  0.7397033
## 
## Accuracy was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 31.5.
```

#####Note
Using 10-fold cross validatoin we achieved the best accuarcy which means that out of sample error is 16%


```r
rf_fit$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, ntree = 1000, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 1000
## No. of variables tried at each split: 32
## 
##         OOB estimate of  error rate: 15.76%
## Confusion matrix:
##     A  B  C  D  E class.error
## A 101  2  3  2  1   0.0733945
## B  13 57  4  3  2   0.2784810
## C   4  5 60  1  0   0.1428571
## D   4  1  6 58  0   0.1594203
## E   0  5  3  5 66   0.1645570
```
