Project Two
================
Mary Brown and Jennifer Relihan
10/21/2021

=======

## Purpose of the Repo

The purpose of a GitHub repo is to act as a interface where you can save projects, track any changes to documents, and work with collaborators. It’s a handy tool to pull and push any saved work so that changes can be made and shared at any point. In our case, it’s where we save, automate, and control any .Rmd/.md files from R Markdown. 

## Required to run this document

``` r
library(tidyverse)  
library(caret)  
library(ggplot2)  
library(ggpubr)
library(knitr)
library(magrittr)  
library(scales)
library(GDAtools)
library(gbm)
library(shiny)
```

## Links to each seperate analysis

- The analysis for [Entertainment articles is available here](Entertainment.html)
- The analysis for [Business articles is available here](https://github.com/Mbrown1994/Project-2-558/blob/main/Business.md)
- The analysis for [Lifestyle articles is available here](https://github.com/Mbrown1994/Project-2-558/blob/main/Lifestyle.html)
- The analysis for [Social Media articles is available here](https://github.com/Mbrown1994/Project-2-558/blob/main/Social%20Media.html)
- The analysis for [Tech articles is available here](https://github.com/Mbrown1994/Project-2-558/blob/main/Tech.html)
- The analysis for [World articles is available here](https://github.com/Mbrown1994/Project-2-558/blob/main/World.html)

## Code used for automation
``` r
# Removing Null values
channelIDs <- c("Entertainment", "Business", "Tech", "Lifestyle", "World", "Social Media")

# Create file names
output_file <- paste0(channelIDs, ".md")

# Commented out for right now
# Create a list for each channel with just the channel name
params= lapply(channelIDs, FUN = function(x){list(channel=x)})  

# Put into a data frame  
reports <- tibble(output_file, params)  

# Rename channelid columns to params  
colnames(reports) <- c("output_file", "params")


# Updated render function for automation of all 6 channel documents
apply(reports, MARGIN=1,
      FUN = function(x){
        render(input = "Project2.Rmd", output_file= x[[1]], params = x[[2]])
      })
```
