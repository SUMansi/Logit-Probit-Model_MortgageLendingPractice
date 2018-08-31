-   [Code File Basics](#code-file-basics)
    -   [Code Header](#code-header)
    -   [Clear Environment and Load packages](#clear-environment-and-load-packages)
-   [Load Data to begin EDA](#load-data-to-begin-eda)
    -   [Analyse the Structure & Summary of data](#analyse-the-structure-summary-of-data)
    -   [Analyse the Visual Summary of data](#analyse-the-visual-summary-of-data)
    -   [Data cleaning and Sample selection](#data-cleaning-and-sample-selection)
    -   [How different variables are Correlated?](#how-different-variables-are-correlated)
    -   [Loan Approval Status by Gender](#loan-approval-status-by-gender)
    -   [Percentage of Approvals by Race](#percentage-of-approvals-by-race)
    -   [Percentage of Approved by Race and Married](#percentage-of-approved-by-race-and-married)
    -   [Approval status wise loan applications](#approval-status-wise-loan-applications)
    -   [Percentage wise distribution of Total Unapproved but qualified mortgages](#percentage-wise-distribution-of-total-unapproved-but-qualified-mortgages)
    -   [Distribution of unapproved applicants meeting guidelines](#distribution-of-unapproved-applicants-meeting-guidelines)

------------------------------------------------------------------------

Code File Basics
----------------

### Code Header

``` r
# Course: ECON 5300
# Title: Labour Market Analysis 
# Purpose: 
# Research Question: 
```

### Clear Environment and Load packages

``` r
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)

# Clear working directory (remove all objects)
rm(list=ls(all=TRUE)) 

# Load packages
# Load packages
library(tidyverse)
library(gridExtra)
library(GGally)
library(knitr)
library(grid)
library(reshape)
library(reshape2)
library(psych)
library(corrplot)
library(lmtest)
library(sandwich)
```

Load Data to begin EDA
----------------------

``` r
# Import LMA data
MLD.Data <- read.csv("MLD Data File-1.csv", header = TRUE)

MLD.Data$LOANPRC <- MLD.Data$LOANPRC * 100

MLD.Data <- MLD.Data %>% mutate(WHITE = as.factor(ifelse(BLACK == 0 & HISPAN == 
    0, 1, 0))) %>% mutate(RACE = as.factor(ifelse(BLACK == 1, "Black", ifelse(HISPAN == 
    1, "Hispanic", ifelse(WHITE == 1, "White", 0))))) %>% filter(MALE %in% c(0, 
    1)) %>% filter(GDLIN %in% c(0, 1)) %>% filter(MARRIED %in% c(0, 1))

MLD.Data.Full <- MLD.Data
```

### Analyse the Structure & Summary of data

``` r
# Structure of the dataset
str(MLD.Data)
```

    ## 'data.frame':    1969 obs. of  10 variables:
    ##  $ MARRIED: Factor w/ 3 levels ".","0","1": 3 2 3 3 2 2 2 3 3 3 ...
    ##  $ GDLIN  : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ OBRAT  : num  34.1 26 37 32.1 33 36 37 30.7 49 37 ...
    ##  $ BLACK  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ HISPAN : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ MALE   : Factor w/ 3 levels ".","0","1": 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ APPROVE: int  0 1 1 1 1 1 1 1 1 1 ...
    ##  $ LOANPRC: num  80 89.5 60 89.6 80.4 ...
    ##  $ WHITE  : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ RACE   : Factor w/ 3 levels "Black","Hispanic",..: 3 3 3 3 3 3 3 3 3 3 ...

``` r
# Summary of the data set ( i.e. Descriptive Statistics of the data)
summary(MLD.Data)
```

    ##  MARRIED      GDLIN            OBRAT           BLACK        
    ##  .:   0   Min.   :0.0000   Min.   : 0.00   Min.   :0.00000  
    ##  0: 671   1st Qu.:1.0000   1st Qu.:28.00   1st Qu.:0.00000  
    ##  1:1298   Median :1.0000   Median :33.00   Median :0.00000  
    ##           Mean   :0.9132   Mean   :32.39   Mean   :0.09903  
    ##           3rd Qu.:1.0000   3rd Qu.:37.00   3rd Qu.:0.00000  
    ##           Max.   :1.0000   Max.   :95.00   Max.   :1.00000  
    ##      HISPAN        MALE        APPROVE          LOANPRC        WHITE   
    ##  Min.   :0.00000   .:   0   Min.   :0.0000   Min.   :  2.105   0: 303  
    ##  1st Qu.:0.00000   0: 368   1st Qu.:1.0000   1st Qu.: 70.000   1:1666  
    ##  Median :0.00000   1:1601   Median :1.0000   Median : 80.000           
    ##  Mean   :0.05485            Mean   :0.8761   Mean   : 77.032           
    ##  3rd Qu.:0.00000            3rd Qu.:1.0000   3rd Qu.: 89.888           
    ##  Max.   :1.00000            Max.   :1.0000   Max.   :257.143           
    ##        RACE     
    ##  Black   : 195  
    ##  Hispanic: 108  
    ##  White   :1666  
    ##                 
    ##                 
    ## 

``` r
# Check the details of different variables

W <- MLD.Data %>% filter(RACE == "White")

H <- MLD.Data %>% filter(RACE == "Hispanic")

B <- MLD.Data %>% filter(RACE == "Black")

summary(W)
```

    ##  MARRIED      GDLIN            OBRAT           BLACK       HISPAN 
    ##  .:   0   Min.   :0.0000   Min.   : 0.00   Min.   :0   Min.   :0  
    ##  0: 565   1st Qu.:1.0000   1st Qu.:27.60   1st Qu.:0   1st Qu.:0  
    ##  1:1101   Median :1.0000   Median :32.55   Median :0   Median :0  
    ##           Mean   :0.9388   Mean   :32.03   Mean   :0   Mean   :0  
    ##           3rd Qu.:1.0000   3rd Qu.:36.60   3rd Qu.:0   3rd Qu.:0  
    ##           Max.   :1.0000   Max.   :95.00   Max.   :0   Max.   :0  
    ##  MALE        APPROVE          LOANPRC        WHITE          RACE     
    ##  .:   0   Min.   :0.0000   Min.   :  2.105   0:   0   Black   :   0  
    ##  0: 295   1st Qu.:1.0000   1st Qu.: 68.149   1:1666   Hispanic:   0  
    ##  1:1371   Median :1.0000   Median : 79.883            White   :1666  
    ##           Mean   :0.9076   Mean   : 75.652                           
    ##           3rd Qu.:1.0000   3rd Qu.: 89.600                           
    ##           Max.   :1.0000   Max.   :257.143

``` r
describe(W)
```

    ##          vars    n  mean    sd median trimmed   mad  min    max  range
    ## MARRIED*    1 1666  2.66  0.47   3.00    2.70  0.00 2.00   3.00   1.00
    ## GDLIN       2 1666  0.94  0.24   1.00    1.00  0.00 0.00   1.00   1.00
    ## OBRAT       3 1666 32.03  8.23  32.55   32.15  6.60 0.00  95.00  95.00
    ## BLACK       4 1666  0.00  0.00   0.00    0.00  0.00 0.00   0.00   0.00
    ## HISPAN      5 1666  0.00  0.00   0.00    0.00  0.00 0.00   0.00   0.00
    ## MALE*       6 1666  2.82  0.38   3.00    2.90  0.00 2.00   3.00   1.00
    ## APPROVE     7 1666  0.91  0.29   1.00    1.00  0.00 0.00   1.00   1.00
    ## LOANPRC     8 1666 75.65 19.01  79.88   77.25 14.83 2.11 257.14 255.04
    ## WHITE*      9 1666  2.00  0.00   2.00    2.00  0.00 2.00   2.00   0.00
    ## RACE*      10 1666  3.00  0.00   3.00    3.00  0.00 3.00   3.00   0.00
    ##           skew kurtosis   se
    ## MARRIED* -0.68    -1.54 0.01
    ## GDLIN    -3.66    11.38 0.01
    ## OBRAT     0.53     5.12 0.20
    ## BLACK      NaN      NaN 0.00
    ## HISPAN     NaN      NaN 0.00
    ## MALE*    -1.69     0.86 0.01
    ## APPROVE  -2.81     5.91 0.01
    ## LOANPRC   0.16     8.66 0.47
    ## WHITE*     NaN      NaN 0.00
    ## RACE*      NaN      NaN 0.00

``` r
1101/(1101 + 565)
```

    ## [1] 0.6608643

``` r
summary(H)
```

    ##  MARRIED     GDLIN            OBRAT           BLACK       HISPAN  MALE  
    ##  .: 0    Min.   :0.0000   Min.   :14.60   Min.   :0   Min.   :1   .: 0  
    ##  0:31    1st Qu.:1.0000   1st Qu.:29.00   1st Qu.:0   1st Qu.:1   0:22  
    ##  1:77    Median :1.0000   Median :33.00   Median :0   Median :1   1:86  
    ##          Mean   :0.8519   Mean   :33.47   Mean   :0   Mean   :1         
    ##          3rd Qu.:1.0000   3rd Qu.:38.33   3rd Qu.:0   3rd Qu.:1         
    ##          Max.   :1.0000   Max.   :62.00   Max.   :0   Max.   :1         
    ##     APPROVE          LOANPRC       WHITE         RACE    
    ##  Min.   :0.0000   Min.   : 40.09   0:108   Black   :  0  
    ##  1st Qu.:1.0000   1st Qu.: 80.00   1:  0   Hispanic:108  
    ##  Median :1.0000   Median : 89.63           White   :  0  
    ##  Mean   :0.7593   Mean   : 85.63                         
    ##  3rd Qu.:1.0000   3rd Qu.: 90.53                         
    ##  Max.   :1.0000   Max.   :162.63

``` r
describe(H)
```

    ##          vars   n  mean    sd median trimmed  mad   min    max  range
    ## MARRIED*    1 108  2.71  0.45   3.00    2.76 0.00  2.00   3.00   1.00
    ## GDLIN       2 108  0.85  0.36   1.00    0.93 0.00  0.00   1.00   1.00
    ## OBRAT       3 108 33.47  8.46  33.00   33.33 6.67 14.60  62.00  47.40
    ## BLACK       4 108  0.00  0.00   0.00    0.00 0.00  0.00   0.00   0.00
    ## HISPAN      5 108  1.00  0.00   1.00    1.00 0.00  1.00   1.00   0.00
    ## MALE*       6 108  2.80  0.40   3.00    2.86 0.00  2.00   3.00   1.00
    ## APPROVE     7 108  0.76  0.43   1.00    0.82 0.00  0.00   1.00   1.00
    ## LOANPRC     8 108 85.63 14.50  89.63   85.89 8.32 40.09 162.63 122.53
    ## WHITE*      9 108  1.00  0.00   1.00    1.00 0.00  1.00   1.00   0.00
    ## RACE*      10 108  2.00  0.00   2.00    2.00 0.00  2.00   2.00   0.00
    ##           skew kurtosis   se
    ## MARRIED* -0.93    -1.15 0.04
    ## GDLIN    -1.95     1.83 0.03
    ## OBRAT     0.35     1.08 0.81
    ## BLACK      NaN      NaN 0.00
    ## HISPAN     NaN      NaN 0.00
    ## MALE*    -1.45     0.11 0.04
    ## APPROVE  -1.20    -0.57 0.04
    ## LOANPRC   1.13     8.13 1.39
    ## WHITE*     NaN      NaN 0.00
    ## RACE*      NaN      NaN 0.00

``` r
77/(77 + 31)
```

    ## [1] 0.712963

``` r
summary(B)
```

    ##  MARRIED     GDLIN            OBRAT           BLACK       HISPAN  MALE   
    ##  .:  0   Min.   :0.0000   Min.   : 5.60   Min.   :1   Min.   :0   .:  0  
    ##  0: 75   1st Qu.:0.0000   1st Qu.:31.00   1st Qu.:1   1st Qu.:0   0: 51  
    ##  1:120   Median :1.0000   Median :35.00   Median :1   Median :0   1:144  
    ##          Mean   :0.7282   Mean   :34.90   Mean   :1   Mean   :0          
    ##          3rd Qu.:1.0000   3rd Qu.:38.85   3rd Qu.:1   3rd Qu.:0          
    ##          Max.   :1.0000   Max.   :63.00   Max.   :1   Max.   :0          
    ##     APPROVE          LOANPRC       WHITE         RACE    
    ##  Min.   :0.0000   Min.   : 28.99   0:195   Black   :195  
    ##  1st Qu.:0.0000   1st Qu.: 80.00   1:  0   Hispanic:  0  
    ##  Median :1.0000   Median : 87.50           White   :  0  
    ##  Mean   :0.6718   Mean   : 84.06                         
    ##  3rd Qu.:1.0000   3rd Qu.: 90.24                         
    ##  Max.   :1.0000   Max.   :255.52

``` r
describe(B)
```

    ##          vars   n  mean    sd median trimmed   mad   min    max  range
    ## MARRIED*    1 195  2.62  0.49    3.0    2.64  0.00  2.00   3.00   1.00
    ## GDLIN       2 195  0.73  0.45    1.0    0.78  0.00  0.00   1.00   1.00
    ## OBRAT       3 195 34.90  8.19   35.0   34.95  5.93  5.60  63.00  57.40
    ## BLACK       4 195  1.00  0.00    1.0    1.00  0.00  1.00   1.00   0.00
    ## HISPAN      5 195  0.00  0.00    0.0    0.00  0.00  0.00   0.00   0.00
    ## MALE*       6 195  2.74  0.44    3.0    2.80  0.00  2.00   3.00   1.00
    ## APPROVE     7 195  0.67  0.47    1.0    0.71  0.00  0.00   1.00   1.00
    ## LOANPRC     8 195 84.06 17.84   87.5   85.07 11.12 28.99 255.52 226.54
    ## WHITE*      9 195  1.00  0.00    1.0    1.00  0.00  1.00   1.00   0.00
    ## RACE*      10 195  1.00  0.00    1.0    1.00  0.00  1.00   1.00   0.00
    ##           skew kurtosis   se
    ## MARRIED* -0.47    -1.79 0.03
    ## GDLIN    -1.02    -0.97 0.03
    ## OBRAT    -0.15     1.92 0.59
    ## BLACK      NaN      NaN 0.00
    ## HISPAN     NaN      NaN 0.00
    ## MALE*    -1.08    -0.84 0.03
    ## APPROVE  -0.73    -1.48 0.03
    ## LOANPRC   3.84    42.98 1.28
    ## WHITE*     NaN      NaN 0.00
    ## RACE*      NaN      NaN 0.00

``` r
120/(120 + 75)
```

    ## [1] 0.6153846

Observations :

1.  Majority of the individuals applied for loan in our sample data are Male.
2.  87% of the total number of loans in our sample data were approved
3.  Majority of the individuals in our sample data are Non-Hispanic White.
4.  Maximum value of LOANPRC = 2.57, i.e.someone applied for loan amount that is 250% of the purchase price

### Analyse the Visual Summary of data

``` r
# Visualize numerical variables

par(mfrow = c(2, 2))
hist(MLD.Data$OBRAT, main = "Histogram of Other Obligations as a percent of total income")
hist(MLD.Data$LOANPRC, main = "Histogram of Loan Amount")
hist(log(MLD.Data$OBRAT), main = "Histogram of Log(OBRAT)")
hist(log(MLD.Data$LOANPRC), main = "Histogram of log(LOANPRC)")
```

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-5-1.png)

Observations :

1.  LoanPRC and other obligation(i.e. OBRAT) have left skewed distribution.

### Data cleaning and Sample selection

``` r
MLD.Data <- MLD.Data %>% filter(LOANPRC <= 120)
# filter(OBRAT < 50)

W <- MLD.Data %>% filter(RACE == "White")

H <- MLD.Data %>% filter(RACE == "Hispanic")

B <- MLD.Data %>% filter(RACE == "Black")

summary(W)
```

    ##  MARRIED      GDLIN           OBRAT           BLACK       HISPAN  MALE    
    ##  .:   0   Min.   :0.000   Min.   : 0.00   Min.   :0   Min.   :0   .:   0  
    ##  0: 562   1st Qu.:1.000   1st Qu.:27.60   1st Qu.:0   1st Qu.:0   0: 293  
    ##  1:1093   Median :1.000   Median :32.50   Median :0   Median :0   1:1362  
    ##           Mean   :0.939   Mean   :32.01   Mean   :0   Mean   :0           
    ##           3rd Qu.:1.000   3rd Qu.:36.50   3rd Qu.:0   3rd Qu.:0           
    ##           Max.   :1.000   Max.   :95.00   Max.   :0   Max.   :0           
    ##     APPROVE          LOANPRC        WHITE          RACE     
    ##  Min.   :0.0000   Min.   :  2.105   0:   0   Black   :   0  
    ##  1st Qu.:1.0000   1st Qu.: 67.815   1:1655   Hispanic:   0  
    ##  Median :1.0000   Median : 79.870            White   :1655  
    ##  Mean   :0.9094   Mean   : 75.076                           
    ##  3rd Qu.:1.0000   3rd Qu.: 89.474                           
    ##  Max.   :1.0000   Max.   :120.000

``` r
describe(W)
```

    ##          vars    n  mean    sd median trimmed   mad  min max  range  skew
    ## MARRIED*    1 1655  2.66  0.47   3.00    2.70  0.00 2.00   3   1.00 -0.68
    ## GDLIN       2 1655  0.94  0.24   1.00    1.00  0.00 0.00   1   1.00 -3.66
    ## OBRAT       3 1655 32.01  8.20  32.50   32.13  6.67 0.00  95  95.00  0.54
    ## BLACK       4 1655  0.00  0.00   0.00    0.00  0.00 0.00   0   0.00   NaN
    ## HISPAN      5 1655  0.00  0.00   0.00    0.00  0.00 0.00   0   0.00   NaN
    ## MALE*       6 1655  2.82  0.38   3.00    2.90  0.00 2.00   3   1.00 -1.69
    ## APPROVE     7 1655  0.91  0.29   1.00    1.00  0.00 0.00   1   1.00 -2.85
    ## LOANPRC     8 1655 75.08 17.40  79.87   77.11 14.82 2.11 120 117.89 -1.15
    ## WHITE*      9 1655  2.00  0.00   2.00    2.00  0.00 2.00   2   0.00   NaN
    ## RACE*      10 1655  3.00  0.00   3.00    3.00  0.00 3.00   3   0.00   NaN
    ##          kurtosis   se
    ## MARRIED*    -1.54 0.01
    ## GDLIN       11.43 0.01
    ## OBRAT        5.21 0.20
    ## BLACK         NaN 0.00
    ## HISPAN        NaN 0.00
    ## MALE*        0.86 0.01
    ## APPROVE      6.12 0.01
    ## LOANPRC      1.49 0.43
    ## WHITE*        NaN 0.00
    ## RACE*         NaN 0.00

``` r
1077/(1077 + 551)
```

    ## [1] 0.6615479

``` r
summary(H)
```

    ##  MARRIED     GDLIN            OBRAT           BLACK       HISPAN  MALE  
    ##  .: 0    Min.   :0.0000   Min.   :14.60   Min.   :0   Min.   :1   .: 0  
    ##  0:30    1st Qu.:1.0000   1st Qu.:29.00   1st Qu.:0   1st Qu.:1   0:20  
    ##  1:76    Median :1.0000   Median :33.00   Median :0   Median :1   1:86  
    ##          Mean   :0.8585   Mean   :33.45   Mean   :0   Mean   :1         
    ##          3rd Qu.:1.0000   3rd Qu.:38.58   3rd Qu.:0   3rd Qu.:1         
    ##          Max.   :1.0000   Max.   :62.00   Max.   :0   Max.   :1         
    ##     APPROVE          LOANPRC       WHITE         RACE    
    ##  Min.   :0.0000   Min.   : 40.09   0:106   Black   :  0  
    ##  1st Qu.:1.0000   1st Qu.: 80.00   1:  0   Hispanic:106  
    ##  Median :1.0000   Median : 89.50           White   :  0  
    ##  Mean   :0.7736   Mean   : 84.41                         
    ##  3rd Qu.:1.0000   3rd Qu.: 90.35                         
    ##  Max.   :1.0000   Max.   :111.43

``` r
describe(H)
```

    ##          vars   n  mean    sd median trimmed  mad   min    max range  skew
    ## MARRIED*    1 106  2.72  0.45    3.0    2.77 0.00  2.00   3.00  1.00 -0.95
    ## GDLIN       2 106  0.86  0.35    1.0    0.94 0.00  0.00   1.00  1.00 -2.03
    ## OBRAT       3 106 33.45  8.54   33.0   33.31 7.12 14.60  62.00 47.40  0.35
    ## BLACK       4 106  0.00  0.00    0.0    0.00 0.00  0.00   0.00  0.00   NaN
    ## HISPAN      5 106  1.00  0.00    1.0    1.00 0.00  1.00   1.00  0.00   NaN
    ## MALE*       6 106  2.81  0.39    3.0    2.88 0.00  2.00   3.00  1.00 -1.57
    ## APPROVE     7 106  0.77  0.42    1.0    0.84 0.00  0.00   1.00  1.00 -1.29
    ## LOANPRC     8 106 84.41 11.41   89.5   85.68 8.47 40.09 111.43 71.34 -1.12
    ## WHITE*      9 106  1.00  0.00    1.0    1.00 0.00  1.00   1.00  0.00   NaN
    ## RACE*      10 106  2.00  0.00    2.0    2.00 0.00  2.00   2.00  0.00   NaN
    ##          kurtosis   se
    ## MARRIED*    -1.11 0.04
    ## GDLIN        2.13 0.03
    ## OBRAT        1.01 0.83
    ## BLACK         NaN 0.00
    ## HISPAN        NaN 0.00
    ## MALE*        0.47 0.04
    ## APPROVE     -0.34 0.04
    ## LOANPRC      2.26 1.11
    ## WHITE*        NaN 0.00
    ## RACE*         NaN 0.00

``` r
74/(29 + 74)
```

    ## [1] 0.7184466

``` r
summary(B)
```

    ##  MARRIED     GDLIN            OBRAT           BLACK       HISPAN  MALE   
    ##  .:  0   Min.   :0.0000   Min.   : 5.60   Min.   :1   Min.   :0   .:  0  
    ##  0: 75   1st Qu.:0.0000   1st Qu.:31.00   1st Qu.:1   1st Qu.:0   0: 50  
    ##  1:118   Median :1.0000   Median :35.00   Median :1   Median :0   1:143  
    ##          Mean   :0.7254   Mean   :34.94   Mean   :1   Mean   :0          
    ##          3rd Qu.:1.0000   3rd Qu.:38.90   3rd Qu.:1   3rd Qu.:0          
    ##          Max.   :1.0000   Max.   :63.00   Max.   :1   Max.   :0          
    ##     APPROVE          LOANPRC       WHITE         RACE    
    ##  Min.   :0.0000   Min.   : 28.99   0:193   Black   :193  
    ##  1st Qu.:0.0000   1st Qu.: 80.00   1:  0   Hispanic:  0  
    ##  Median :1.0000   Median : 87.02           White   :  0  
    ##  Mean   :0.6684   Mean   : 82.98                         
    ##  3rd Qu.:1.0000   3rd Qu.: 90.23                         
    ##  Max.   :1.0000   Max.   :100.35

``` r
describe(B)
```

    ##          vars   n  mean    sd median trimmed   mad   min    max range
    ## MARRIED*    1 193  2.61  0.49   3.00    2.64  0.00  2.00   3.00  1.00
    ## GDLIN       2 193  0.73  0.45   1.00    0.78  0.00  0.00   1.00  1.00
    ## OBRAT       3 193 34.94  8.20  35.00   35.00  5.93  5.60  63.00 57.40
    ## BLACK       4 193  1.00  0.00   1.00    1.00  0.00  1.00   1.00  0.00
    ## HISPAN      5 193  0.00  0.00   0.00    0.00  0.00  0.00   0.00  0.00
    ## MALE*       6 193  2.74  0.44   3.00    2.80  0.00  2.00   3.00  1.00
    ## APPROVE     7 193  0.67  0.47   1.00    0.71  0.00  0.00   1.00  1.00
    ## LOANPRC     8 193 82.98 12.66  87.02   84.94 10.41 28.99 100.35 71.37
    ## WHITE*      9 193  1.00  0.00   1.00    1.00  0.00  1.00   1.00  0.00
    ## RACE*      10 193  1.00  0.00   1.00    1.00  0.00  1.00   1.00  0.00
    ##           skew kurtosis   se
    ## MARRIED* -0.45    -1.80 0.04
    ## GDLIN    -1.00    -1.00 0.03
    ## OBRAT    -0.15     1.94 0.59
    ## BLACK      NaN      NaN 0.00
    ## HISPAN     NaN      NaN 0.00
    ## MALE*    -1.09    -0.81 0.03
    ## APPROVE  -0.71    -1.50 0.03
    ## LOANPRC  -1.89     4.75 0.91
    ## WHITE*     NaN      NaN 0.00
    ## RACE*      NaN      NaN 0.00

``` r
116/(116 + 69)
```

    ## [1] 0.627027

### How different variables are Correlated?

``` r
MLD.Data %>% ggpairs()
```

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-7-1.png)

### Loan Approval Status by Gender

``` r
L1 <- MLD.Data %>% filter(APPROVE == 1) %>% group_by(RACE, MALE) %>% summarise(count = n()) %>% 
    ggplot(aes(x = reorder(RACE, count), y = count, fill = as.factor(MALE))) + 
    geom_bar(stat = "identity", position = "dodge") + geom_text(aes(label = count), 
    size = 5, vjust = -0.1, position = position_dodge(0.9)) + theme_classic() + 
    ggtitle("Loan Approved by Gender") + xlab("Race") + ylab("Count of Individuals") + 
    scale_fill_discrete(name = "Gender", breaks = c("0", "1"), labels = c("Female", 
        "Male"))
theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10))
```

    ## List of 4
    ##  $ axis.text.x:List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 1
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : num 20
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ plot.title :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : chr "bold"
    ##   ..$ colour       : NULL
    ##   ..$ size         : num 13
    ##   ..$ hjust        : num 0.5
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.title :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : num 12
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text  :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : num 10
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

``` r
L2 <- MLD.Data %>% filter(APPROVE == 0) %>% group_by(RACE, MALE) %>% summarise(count = n()) %>% 
    ggplot(aes(x = reorder(RACE, count), y = count, fill = as.factor(MALE))) + 
    geom_bar(stat = "identity", position = "dodge") + geom_text(aes(label = count), 
    size = 5, vjust = -0.1, position = position_dodge(0.9)) + theme_classic() + 
    ggtitle("Loan Not Approved by Gender") + xlab("Race") + ylab("Count of Individuals") + 
    scale_fill_discrete(name = "Gender", breaks = c("0", "1"), labels = c("Female", 
        "Male"))
theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10))
```

    ## List of 4
    ##  $ axis.text.x:List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : NULL
    ##   ..$ hjust        : num 1
    ##   ..$ vjust        : num 1
    ##   ..$ angle        : num 20
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ plot.title :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : chr "bold"
    ##   ..$ colour       : NULL
    ##   ..$ size         : num 13
    ##   ..$ hjust        : num 0.5
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.title :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : num 12
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  $ axis.text  :List of 11
    ##   ..$ family       : NULL
    ##   ..$ face         : NULL
    ##   ..$ colour       : NULL
    ##   ..$ size         : num 10
    ##   ..$ hjust        : NULL
    ##   ..$ vjust        : NULL
    ##   ..$ angle        : NULL
    ##   ..$ lineheight   : NULL
    ##   ..$ margin       : NULL
    ##   ..$ debug        : NULL
    ##   ..$ inherit.blank: logi FALSE
    ##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
    ##  - attr(*, "class")= chr [1:2] "theme" "gg"
    ##  - attr(*, "complete")= logi FALSE
    ##  - attr(*, "validate")= logi TRUE

``` r
grid.arrange(L1, L2, nrow = 1)
```

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-8-1.png)

### Percentage of Approvals by Race

``` r
L3 <- MLD.Data %>% group_by(RACE, APPROVE) %>% summarise(count = n()) %>% group_by(RACE) %>% 
    mutate(Proportion = round(count/sum(count), 3)) %>% ggplot(aes(x = RACE, 
    y = Proportion, fill = as.factor(APPROVE))) + geom_bar(stat = "identity", 
    position = "dodge") + geom_text(aes(label = scales::percent(Proportion)), 
    size = 5, vjust = -0.1, position = position_dodge(0.9)) + theme_classic() + 
    ggtitle("%age of Loans Approved across each race") + xlab("Race") + ylab("%age of Individuals") + 
    scale_fill_discrete(name = "Approval Status", breaks = c("0", "1"), labels = c("Not Approved", 
        "Approved")) + theme(axis.text.x = element_text(angle = 0, hjust = 1, 
    vjust = 1)) + theme(plot.title = element_text(size = 13, face = "bold", 
    hjust = 0.5)) + theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

L4 <- MLD.Data %>% group_by(RACE, APPROVE) %>% summarise(count = n()) %>% ggplot(aes(x = RACE, 
    y = count, fill = as.factor(APPROVE))) + geom_bar(stat = "identity", position = "dodge") + 
    geom_text(aes(label = count), size = 5, vjust = -0.1, position = position_dodge(0.9)) + 
    theme_classic() + ggtitle("Number of Loans Approved across each race") + 
    xlab("Race") + ylab("Count of Individuals") + scale_fill_discrete(name = "Approval Status", 
    breaks = c("0", "1"), labels = c("Not Approved", "Approved")) + theme(axis.text.x = element_text(angle = 0, 
    hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, face = "bold", 
    hjust = 0.5)) + theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))

grid.arrange(L3, L4, nrow = 1)
```

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-9-1.png)

Observations:

1.  92.6% of the loans applied by White individuals were approved. However, only 70.5% of the loans applied by Black individuals were approved.

### Percentage of Approved by Race and Married

``` r
MLD_race_married <- MLD.Data %>% group_by(RACE, MARRIED) %>% summarise(Total_Number_Married = n())

MLD_race <- MLD.Data %>% group_by(RACE) %>% summarise(Total_Number = n(), Percentage = paste((round(((Total_Number/nrow(MLD.Data)) * 
    100), 2)), "%"))


MLD_race_married <- MLD_race_married %>% inner_join((MLD_race %>% select(Total_Number, 
    RACE)), by = "RACE") %>% mutate(Approve_Percentage = round(((Total_Number_Married/Total_Number) * 
    100), 1))

MLD_race_app_mar <- MLD.Data %>% group_by(RACE, MARRIED, APPROVE) %>% summarise(Total_Number_App_Mar = n())

MLD_race_app_mar <- MLD_race_app_mar %>% inner_join((MLD_race_married %>% select(Total_Number_Married, 
    RACE, MARRIED)), by = c("RACE", "MARRIED")) %>% mutate(Approve_Percentage = round(((Total_Number_App_Mar/Total_Number_Married) * 
    100), 1), Key = paste(MARRIED, "_", APPROVE, sep = ""))


# Plot MLD Approve Percentage by Race
MLD_race_app_mar %>% ggplot(aes(x = as.factor(Key), y = Approve_Percentage, 
    fill = RACE)) + geom_bar(stat = "identity", position = "dodge") + ylab("Percentage") + 
    ggtitle("Married Percentage by Race") + theme_classic() + geom_text(aes(label = paste(round(Approve_Percentage, 
    1), "%", sep = "")), vjust = -0.3, colour = "black", position = position_dodge(width = 0.9), 
    size = 5) + theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) + 
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) + 
    theme(axis.title.x = element_blank()) + scale_x_discrete(labels = c(`0_0` = "Single_Not Approved", 
    `0_1` = "Single_Approved", `1_0` = "Married_Not Approved", `1_1` = "Married_Approved"))
```

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-10-1.png)

Observations:

1.  Across all races, more number of loans are approved for married individuals in comparision to un-married individuals.

``` r
L5 <- MLD.Data %>% group_by(RACE, GDLIN, APPROVE) %>% summarise(count = n()) %>% 
    mutate(Status = as.factor(ifelse(GDLIN == 0 & APPROVE == 0, "No_CRworthy_Loan_NotA", 
        ifelse(GDLIN == 0 & APPROVE == 1, "No_CRworthy_Loan_A", ifelse(GDLIN == 
            1 & APPROVE == 0, "CRworthy_Loan_NotA", ifelse(GDLIN == 1 & APPROVE == 
            1, "CRworthy_Loan_A", 0)))))) %>% ggplot(aes(x = RACE, y = count, 
    fill = Status)) + geom_bar(stat = "identity", position = "dodge") + geom_text(aes(label = count), 
    size = 5, vjust = -0.1, position = position_dodge(0.9)) + theme_classic() + 
    ggtitle("Loan Status: Number of Individuals, based on GDLIN & APPROVE") + 
    xlab("Race") + ylab("Count of Individuals") + # scale_fill_discrete(name='Approval Status', breaks=c('0', '1'),
# labels=c('Not Approved', 'Approved')) +
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10))


L6 <- MLD.Data %>% group_by(RACE, GDLIN, APPROVE) %>% mutate(Status = as.factor(ifelse(GDLIN == 
    0 & APPROVE == 0, "No_CRworthy_Loan_NotA", ifelse(GDLIN == 0 & APPROVE == 
    1, "No_CRworthy_Loan_A", ifelse(GDLIN == 1 & APPROVE == 0, "CRworthy_Loan_NotA", 
    ifelse(GDLIN == 1 & APPROVE == 1, "CRworthy_Loan_A", 0)))))) %>% group_by(RACE, 
    Status) %>% summarise(count = n()) %>% group_by(RACE) %>% mutate(Proportion = round(count/sum(count), 
    3)) %>% ggplot(aes(x = RACE, y = Proportion, fill = Status)) + geom_bar(stat = "identity", 
    position = "dodge") + geom_text(aes(label = scales::percent(Proportion)), 
    size = 5, vjust = -0.1, position = position_dodge(0.9)) + theme_classic() + 
    ggtitle("Loan Status: %age of Individuals, based on GDLIN & APPROVE") + 
    xlab("Race") + ylab("%age of Individuals") + # scale_fill_discrete(name='Approval Status', breaks=c('0', '1'),
# labels=c('Not Approved', 'Approved')) +
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10)) + guides(fill = FALSE)

grid.arrange(L5, L6, nrow = 1)
```

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-11-1.png)

Observations:

1.  Loans are highly likely to be approved if individuals are credit worthy.

2.  For Black individuals, 1.9% of the loans are approved even though they are not credit worthy. However, for white individuals only 1.4% of the loans are approved if individuals are not credit worthy.

``` r
L7 <- MLD.Data %>% group_by(APPROVE, MALE) %>% summarise(count = n()) %>% ggplot(aes(x = MALE, 
    y = count, fill = as.factor(APPROVE))) + geom_bar(stat = "identity", position = "dodge") + 
    geom_text(aes(label = count), size = 5, vjust = -0.1, position = position_dodge(0.9)) + 
    theme_classic() + ggtitle("Number of loans approved based on Gender & Race") + 
    xlab("Gender") + ylab("Count of Individuals") + scale_fill_discrete(name = "Approval Status", 
    breaks = c("0", "1"), labels = c("Not Approved", "Approved")) + theme(axis.text.x = element_text(angle = 0, 
    hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, face = "bold", 
    hjust = 0.5)) + theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10))


L8 <- MLD.Data %>% group_by(MALE, APPROVE, RACE) %>% mutate(Status = as.factor(ifelse(MALE == 
    0 & APPROVE == 0, "Female_Loan_NotA", ifelse(MALE == 0 & APPROVE == 1, "Female_Loan_A", 
    ifelse(MALE == 1 & APPROVE == 0, "Male_Loan_NotA", ifelse(MALE == 1 & APPROVE == 
        1, "Male_Loan_A", 0)))))) %>% group_by(RACE, Status) %>% summarise(count = n()) %>% 
    group_by(RACE) %>% mutate(Proportion = round(count/sum(count), 3)) %>% ggplot(aes(x = RACE, 
    y = Proportion, fill = Status)) + geom_bar(stat = "identity", position = "dodge") + 
    geom_text(aes(label = scales::percent(Proportion)), size = 5, vjust = -0.1, 
        position = position_dodge(0.9)) + theme_classic() + ggtitle("%age of loans approved, based on Gender & Race") + 
    xlab("Race") + ylab("%age of Individuals") + # scale_fill_discrete(name='Approval Status', breaks=c('0', '1'),
# labels=c('Not Approved', 'Approved')) +
theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1)) + theme(plot.title = element_text(size = 13, 
    face = "bold", hjust = 0.5)) + theme(axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10))



grid.arrange(L7, L8, nrow = 1)
```

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-12-1.png)

Observations:

1.  Among Whites, 75.8% of the loans that were approved belonged to Males.

### Approval status wise loan applications

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-13-1.png)

### Percentage wise distribution of Total Unapproved but qualified mortgages

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-14-1.png)

### Distribution of unapproved applicants meeting guidelines

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-15-1.png)

As expected,most of the unapproved loan applications have very high loan to price ratio and that might be the reason for rejection of the loans. Also the number of unapproved loans with low loan to price ratio are very less in number.

Strangely though, large number of unapproved loans belong to the category where other obligations are between 20 to 40 % of the total earnings. There are 21 applicants who have other obligations that will consume between 40 to 60% of the applicantws earning and it makes sense to reject those applications.

``` r
LG <- MLD.Data %>% filter(APPROVE == 1) %>% mutate(Loan_Group = cut(LOANPRC, 
    4)) %>% count(Loan_Group, RACE) %>% group_by(RACE) %>% mutate(Proportion = round(n/sum(n), 
    3)) %>% ggplot(aes(x = RACE, y = Proportion, fill = Loan_Group)) + geom_bar(stat = "identity", 
    position = "dodge") + geom_text(aes(label = scales::percent(Proportion)), 
    size = 5, vjust = -0.1, position = position_dodge(0.9)) + theme_classic() + 
    ggtitle("Loan Approved: Among LOANPRC groups") + xlab("Race") + ylab("%age of Individuals")

OBG <- MLD.Data %>% filter(APPROVE == 1) %>% mutate(Other_Obligation_Group = cut(OBRAT, 
    4)) %>% count(Other_Obligation_Group, RACE) %>% group_by(RACE) %>% mutate(Proportion = round(n/sum(n), 
    3)) %>% ggplot(aes(x = RACE, y = Proportion, fill = Other_Obligation_Group)) + 
    geom_bar(stat = "identity", position = "dodge") + geom_text(aes(label = scales::percent(Proportion)), 
    size = 5, vjust = -0.1, position = position_dodge(0.9)) + theme_classic() + 
    ggtitle("Loan Approved:Among Other Obligations groups ") + xlab("Race") + 
    ylab("%age of Individuals")

grid.arrange(LG, OBG, nrow = 1)
```

![alt text]( https://github.com/SUMansi/Logit-Probit-Model_MortgageLendingPractice/blob/master/EDA_Figure/unnamed-chunk-16-1.png)

``` r
# LogitModelw = glm(APPROVE ~ OBRAT + LOANPRC + MARRIED + GDLIN,
# data=subset(MLD.Data, RACE == 'White'), family = 'binomial')
# summary(LogitModelw) LogitModelB = glm(APPROVE ~ OBRAT + LOANPRC + MARRIED
# + GDLIN , data=subset(MLD.Data, RACE == 'Black'), family = 'binomial')
# summary(LogitModelB) LogitModelH = glm(APPROVE ~ OBRAT + LOANPRC + MARRIED
# + GDLIN, data=subset(MLD.Data, RACE == 'Hispanic'), family = 'binomial')
# summary(LogitModelH)
```

``` r
library(aod)
```

    ## Warning: package 'aod' was built under R version 3.4.3

``` r
library(ggplot2)
library(Rcpp)
```

    ## Warning: package 'Rcpp' was built under R version 3.4.3

``` r
LogitModel = glm(MALE ~ OBRAT + LOANPRC + MARRIED + GDLIN + RACE, data = MLD.Data, 
    family = "binomial")

LogitModel = glm(APPROVE ~ OBRAT + LOANPRC + MARRIED + GDLIN + BLACK + HISPAN, 
    data = MLD.Data, family = "binomial")
summary(LogitModel)
```

    ## 
    ## Call:
    ## glm(formula = APPROVE ~ OBRAT + LOANPRC + MARRIED + GDLIN + BLACK + 
    ##     HISPAN, family = "binomial", data = MLD.Data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.8768   0.2456   0.3086   0.3727   2.3402  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  1.17087    0.66655   1.757 0.078981 .  
    ## OBRAT       -0.03313    0.01044  -3.174 0.001501 ** 
    ## LOANPRC     -0.01522    0.00674  -2.258 0.023940 *  
    ## MARRIED1     0.49332    0.18294   2.697 0.007005 ** 
    ## GDLIN        3.73435    0.21830  17.107  < 2e-16 ***
    ## BLACK       -0.87099    0.24071  -3.618 0.000296 ***
    ## HISPAN      -0.85529    0.31948  -2.677 0.007425 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1447.9  on 1953  degrees of freedom
    ## Residual deviance:  941.3  on 1947  degrees of freedom
    ## AIC: 955.3
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Generate Log-Likelihood
logLik(LogitModel)
```

    ## 'log Lik.' -470.6506 (df=7)

``` r
# Generate Odds Ratios
cbind(exp(coef(LogitModel)), 1)
```

    ##                   [,1] [,2]
    ## (Intercept)  3.2248023    1
    ## OBRAT        0.9674094    1
    ## LOANPRC      0.9848955    1
    ## MARRIED1     1.6377387    1
    ## GDLIN       41.8609750    1
    ## BLACK        0.4185370    1
    ## HISPAN       0.4251606    1

``` r
# Define prototypical loan applicants (you will need more than 3)
prototype1 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 1, BLACK = 0, HISPAN = 0)
prototype2 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 1, BLACK = 0, HISPAN = 1)
prototype3 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 1, BLACK = 1, HISPAN = 0)

prototype4 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 0, BLACK = 0, HISPAN = 0)
prototype5 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 0, BLACK = 0, HISPAN = 1)
prototype6 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "1", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 0, BLACK = 1, HISPAN = 0)

prototype7 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 1, BLACK = 0, HISPAN = 0)
prototype8 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 1, BLACK = 0, HISPAN = 1)
prototype9 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "0", LOANPRC = mean(MLD.Data.Full$LOANPRC), 
    GDLIN = 1, BLACK = 1, HISPAN = 0)

prototype10 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "0", 
    LOANPRC = mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 0, HISPAN = 0)
prototype11 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "0", 
    LOANPRC = mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 0, HISPAN = 1)
prototype12 <- data.frame(OBRAT = mean(MLD.Data.Full$OBRAT), MARRIED = "0", 
    LOANPRC = mean(MLD.Data.Full$LOANPRC), GDLIN = 0, BLACK = 1, HISPAN = 0)

# Predict probabilities for prototypical individuals
prototype1$predictedprob <- predict(LogitModel, newdata = prototype1, type = "response")
prototype2$predictedprob <- predict(LogitModel, newdata = prototype2, type = "response")
prototype3$predictedprob <- predict(LogitModel, newdata = prototype3, type = "response")

prototype4$predictedprob <- predict(LogitModel, newdata = prototype4, type = "response")
prototype5$predictedprob <- predict(LogitModel, newdata = prototype5, type = "response")
prototype6$predictedprob <- predict(LogitModel, newdata = prototype6, type = "response")

prototype7$predictedprob <- predict(LogitModel, newdata = prototype7, type = "response")
prototype8$predictedprob <- predict(LogitModel, newdata = prototype8, type = "response")
prototype9$predictedprob <- predict(LogitModel, newdata = prototype9, type = "response")

prototype10$predictedprob <- predict(LogitModel, newdata = prototype10, type = "response")
prototype11$predictedprob <- predict(LogitModel, newdata = prototype11, type = "response")
prototype12$predictedprob <- predict(LogitModel, newdata = prototype12, type = "response")


rbind.data.frame(prototype1, prototype2, prototype3, prototype4, prototype5, 
    prototype6, prototype7, prototype8, prototype9, prototype10, prototype11, 
    prototype12)
```

    ##       OBRAT MARRIED  LOANPRC GDLIN BLACK HISPAN predictedprob
    ## 1  32.39172       1 77.03204     1     0      0     0.9590221
    ## 2  32.39172       1 77.03204     1     0      1     0.9086775
    ## 3  32.39172       1 77.03204     1     1      0     0.9073661
    ## 4  32.39172       1 77.03204     0     0      0     0.3585938
    ## 5  32.39172       1 77.03204     0     0      1     0.1920474
    ## 6  32.39172       1 77.03204     0     1      0     0.1896228
    ## 7  32.39172       0 77.03204     1     0      0     0.9345981
    ## 8  32.39172       0 77.03204     1     0      1     0.8586687
    ## 9  32.39172       0 77.03204     1     1      0     0.8567524
    ## 10 32.39172       0 77.03204     0     0      0     0.2544934
    ## 11 32.39172       0 77.03204     0     0      1     0.1267420
    ## 12 32.39172       0 77.03204     0     1      0     0.1250143

``` r
# Estimate Probit Model

ProbitModel = glm(APPROVE ~ OBRAT + LOANPRC + MARRIED + GDLIN + BLACK + HISPAN, 
    data = MLD.Data, family = binomial(link = "probit"))
summary(ProbitModel)
```

    ## 
    ## Call:
    ## glm(formula = APPROVE ~ OBRAT + LOANPRC + MARRIED + GDLIN + BLACK + 
    ##     HISPAN, family = binomial(link = "probit"), data = MLD.Data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.9327   0.2396   0.3078   0.3741   2.2830  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  0.424859   0.330656   1.285 0.198828    
    ## OBRAT       -0.015967   0.005392  -2.961 0.003065 ** 
    ## LOANPRC     -0.007211   0.003182  -2.267 0.023417 *  
    ## MARRIED1     0.244450   0.091113   2.683 0.007298 ** 
    ## GDLIN        2.151438   0.121809  17.662  < 2e-16 ***
    ## BLACK       -0.450760   0.127257  -3.542 0.000397 ***
    ## HISPAN      -0.442895   0.166795  -2.655 0.007923 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1447.91  on 1953  degrees of freedom
    ## Residual deviance:  940.93  on 1947  degrees of freedom
    ## AIC: 954.93
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Generate Log-Likelihood
logLik(ProbitModel)
```

    ## 'log Lik.' -470.464 (df=7)

``` r
# Predict probabilities for prototypical individuals
prototype1$predictedprob <- predict(ProbitModel, newdata = prototype1, type = "response")
prototype2$predictedprob <- predict(ProbitModel, newdata = prototype2, type = "response")
prototype3$predictedprob <- predict(ProbitModel, newdata = prototype3, type = "response")

prototype4$predictedprob <- predict(ProbitModel, newdata = prototype4, type = "response")
prototype5$predictedprob <- predict(ProbitModel, newdata = prototype5, type = "response")
prototype6$predictedprob <- predict(ProbitModel, newdata = prototype6, type = "response")

prototype7$predictedprob <- predict(ProbitModel, newdata = prototype7, type = "response")
prototype8$predictedprob <- predict(ProbitModel, newdata = prototype8, type = "response")
prototype9$predictedprob <- predict(ProbitModel, newdata = prototype9, type = "response")

prototype10$predictedprob <- predict(ProbitModel, newdata = prototype10, type = "response")
prototype11$predictedprob <- predict(ProbitModel, newdata = prototype11, type = "response")
prototype12$predictedprob <- predict(ProbitModel, newdata = prototype12, type = "response")

rbind.data.frame(prototype1, prototype2, prototype3, prototype4, prototype5, 
    prototype6, prototype7, prototype8, prototype9, prototype10, prototype11, 
    prototype12)
```

    ##       OBRAT MARRIED  LOANPRC GDLIN BLACK HISPAN predictedprob
    ## 1  32.39172       1 77.03204     1     0      0     0.9597732
    ## 2  32.39172       1 77.03204     1     0      1     0.9040817
    ## 3  32.39172       1 77.03204     1     1      0     0.9027361
    ## 4  32.39172       1 77.03204     0     0      0     0.3433351
    ## 5  32.39172       1 77.03204     0     0      1     0.1987002
    ## 6  32.39172       1 77.03204     0     1      0     0.1965142
    ## 7  32.39172       0 77.03204     1     0      0     0.9336592
    ## 8  32.39172       0 77.03204     1     0      1     0.8555904
    ## 9  32.39172       0 77.03204     1     1      0     0.8537953
    ## 10 32.39172       0 77.03204     0     0      0     0.2585482
    ## 11 32.39172       0 77.03204     0     0      1     0.1376974
    ## 12 32.39172       0 77.03204     0     1      0     0.1359740

``` r
# Predited Probability (Logit MOdel)
1/(1 + exp(-(0.980786 - 0.027265 * (mean(MLD.Data.Full$OBRAT)) - 0.016264 * 
    (mean(MLD.Data.Full$LOANPRC)) + 0.487474 + 3.854172)))
```

    ## [1] 0.9603201

``` r
1/(1 + exp(-(0.980786 - 0.027265 * (mean(MLD.Data.Full$OBRAT)) - 0.016264 * 
    (mean(MLD.Data.Full$LOANPRC)) + 0.487474 + 3.854172 - 0.901495)))
```

    ## [1] 0.907621

``` r
1/(1 + exp(-(0.980786 - 0.027265 * (mean(MLD.Data.Full$OBRAT)) - 0.016264 * 
    (mean(MLD.Data.Full$LOANPRC)) + 0.487474 + 3.854172 - 0.720995)))
```

    ## [1] 0.9216824

``` r
# Predited Probability (Probit MOdel)
(0.300495 - 0.012563 * (mean(MLD.Data.Full$OBRAT)) - 0.007673 * (mean(MLD.Data.Full$LOANPRC)) + 
    0.239988 + 2.220218)  #1.762697  0.961
```

    ## [1] 1.762697

``` r
(0.300495 - 0.012563 * (mean(MLD.Data.Full$OBRAT)) - 0.007673 * (mean(MLD.Data.Full$LOANPRC)) + 
    0.239988 + 2.220218 - 0.464631)  #1.298066 0.903
```

    ## [1] 1.298066

``` r
(0.300495 - 0.012563 * (mean(MLD.Data.Full$OBRAT)) - 0.007673 * (mean(MLD.Data.Full$LOANPRC)) + 
    0.239988 + 2.220218 - 0.377086)  #1.385611   0.917
```

    ## [1] 1.385611

``` r
1 - 0.00604 - 10 * (((0.6^4)/4) + ((3 * (0.6^8))/4) - ((2 * (0.6^6))/3) - ((2 * 
    (0.6^10))/5) + ((0.6^12)/12))
```

    ## [1] 0.8774013
