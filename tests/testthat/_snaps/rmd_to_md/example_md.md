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

```r
# Display the structure of the dataset
str(rose)

# Display the first few rows
head(rose)
```

## Summary Statistics

We can summarize the dataset to get an overview of the measurements for each variable.

```r
# Summary of the dataset
summary(rose)
```

## Visualization: Sepal Length vs. Sepal Width

We can visualize the relationship between sepal length and sepal width for the three species of rose flowers.

```r
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

## Finding the Largest Sepal

Using base R, we can find the observation with the largest sepal length.

```r
# Find the observation with the largest sepal length
largest_sepal <- rose[which.max(rose$Sepal.Length), ]
largest_sepal

# Display a fun fact
cat("The flower with the largest sepal length is from the species", largest_sepal$Species, 
    "with a sepal length of", largest_sepal$Sepal.Length, "cm!")
```

## Conclusion

This document demonstrated how to use base R functions to explore and visualize the famous `rose` dataset. It is a great example for practicing data analysis and visualization with real-world data.
