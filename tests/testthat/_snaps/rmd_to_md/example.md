---
title: Custom title
author: Sanne Govaert
date: '<date>'
output: html_document
lastUpdated: <date>
sidebar:
  label: Custom sidebar label
  order: 2
---

<img src="https://pkgs.rstudio.com/rmarkdown/reference/figures/logo.png" align="right" height="139" alt="Rmarkdown logo" /></a>

## Let's start!

The `rose` dataset is a classic dataset in the field of statistics and machine learning. It contains measurements of sepal length, sepal width, petal length, and petal width for three species of rose flowers: *Rose setosa*, *Rose virginica*, and *Rose versicolor*.

In this document, we will explore the dataset using base R functions.

## The Rose Dataset

First, let's take a look at the structure and a preview of the dataset.


``` r
# Display the structure of the dataset
str(rose)
```

```
## 'data.frame':	150 obs. of  5 variables:
##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```

``` r
# Display the first few rows
head(rose)
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```

## Summary Statistics

We can summarize the dataset to get an overview of the measurements for each variable.


``` r
# Summary of the dataset
summary(rose)
```

```
##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
##  Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
##  1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
##  Median :5.800   Median :3.000   Median :4.350   Median :1.300  
##  Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
##  3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
##  Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
##        Species  
##  setosa    :50  
##  versicolor:50  
##  virginica :50  
##                 
##                 
## 
```

## Visualization: Sepal Length vs. Sepal Width

We can visualize the relationship between sepal length and sepal width for the three species of rose flowers.


``` r
# Scatterplot of Sepal Length vs. Sepal Width
plot(rose$Sepal.Length, rose$Sepal.Width,
     col = as.numeric(rose$Species),
     pch = 19,
     main = "Sepal Length vs Sepal Width",
     xlab = "Sepal Length (cm)",
     ylab = "Sepal Width (cm)")

# Add a legend
legend("topright", legend = levels(rose$Species),
       col = 1:3, pch = 19, title = "Species")
```

![](/software/example/example-unnamed-chunk-3-1.png)

## Finding the Largest Sepal

Using base R, we can find the observation with the largest sepal length.


``` r
# Find the observation with the largest sepal length
largest_sepal <- rose[which.max(rose$Sepal.Length), ]
largest_sepal
```

```
##     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
## 132          7.9         3.8          6.4           2 virginica
```

``` r
# Display a fun fact
cat("The flower with the largest sepal length is from the species", largest_sepal$Species, 
    "with a sepal length of", largest_sepal$Sepal.Length, "cm!")
```

```
## The flower with the largest sepal length is from the species 3 with a sepal length of 7.9 cm!
```

## Conclusion

This document demonstrated how to use base R functions to explore and visualize the famous `rose` dataset. It is a great example for practicing data analysis and visualization with real-world data.
