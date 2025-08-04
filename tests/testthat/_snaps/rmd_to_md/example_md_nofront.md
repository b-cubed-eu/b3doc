---
title: Custom title
lastUpdated: <date>
sidebar:
  label: Custom sidebar label
  order: 2
---

<img src="https://pkgs.rstudio.com/rmarkdown/reference/figures/logo.png" align="right" height="139" alt="Rmarkdown logo" /></a>

## Let's start!

The `rosa` dataset is a classic dataset in the field of statistics and machine learning. It contains measurements of sepal length, sepal width, petal length, and petal width for three species of rosa flowers: *Rosa setosa*, *Rosa virginica*, and *Rosa versicolor*.

In this document, we will explore the dataset using base R functions.

## The Rosa Dataset

First, let's take a look at the structure and a preview of the dataset.

```r
# Display the structure of the dataset
str(rosa)

# Display the first few rows
head(rosa)
```

## Summary Statistics

We can summarize the dataset to get an overview of the measurements for each variable.

```r
# Summary of the dataset
summary(rosa)
```

## Visualization: Sepal Length vs. Sepal Width

We can visualize the relationship between sepal length and sepal width for the three species of rosa flowers.

```r
# Scatterplot of Sepal Length vs. Sepal Width
plot(rosa$Sepal.Length, rosa$Sepal.Width,
     col = as.numeric(rosa$Species),
     pch = 19,
     main = "Sepal Length vs Sepal Width",
     xlab = "Sepal Length (cm)",
     ylab = "Sepal Width (cm)")

# Add a legend
legend("topright", legend = levels(rosa$Species),
       col = 1:3, pch = 19, title = "Species")
```

## Finding the Largest Sepal

Using base R, we can find the observation with the largest sepal length.

```r
# Find the observation with the largest sepal length
largest_sepal <- rosa[which.max(rosa$Sepal.Length), ]
largest_sepal

# Display a fun fact
cat("The flower with the largest sepal length is from the species", largest_sepal$Species, 
    "with a sepal length of", largest_sepal$Sepal.Length, "cm!")
```

## Conclusion

This document demonstrated how to use base R functions to explore and visualize the famous `rosa` dataset. It is a great example for practicing data analysis and visualization with real-world data.
