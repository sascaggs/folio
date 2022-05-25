---
title: Overdispersion simulation
author: Shane A. Scaggs
date: '2022-01-29'
slug: overdispersion-simulation
categories:
  - Statistics
tags:
  - Bayes
  - Modeling
  - simulation
draft: yes 
---

# Preamble 


```r
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
```




# Introduction

It is no secret data generated from trail camera photographs tends to be *overdispersed*. Most of the time no photographs are taken, and even when they are, many photographs do not contain animals. This *zero-inflation* must be overcome before we can make inferences about the factors that predict the presence of wildlife. 

This is a problem I encounter in my research because I want to estimate the abundance of game species across seral stages in a neotropical rainforest. These seral stages, often called *fallows*, are created by Q'eqchi' swidden cultivation techniques that briefly disturb vegetation communities and facilitate forest succession. To estimate game abundance across these fallows, I am setting 25 cameras in a grid. One axis of the grid is a disturbance gradient that extends from highly disturbed areas near a village to infrequently cultivated regions near a national park. The other axis of the grid extends out from a river. 

The main question is: *how do human disturbances and the fallows created by swidden cultivation influence wildlife abundance?* Before I answer this empirically, I want to simulate some hypothetical answers based on three models: 

1. **Neutral model** -- Fallow classes have no influence on game abundance. Observations appear random.  
2. **Climax model** -- As forests regrow toward climax forest conditions, game abundance linearly increases. 
3. **Intermediate model** -- Intermediate stages of regrowth offer novel foraging and breeding habitat, leading to equal or greater game abundance relative to climax conditions. 

# Coding fallow classes 

Accurately categorizing fallow stages is a topic we could spend decades on. We don't have that kind of time, so how should we code them for a simulation? 

One way to do it would be to generate a uniform distribution ranging from `0`, under cultivation, to `climax`. This would assume an equal distribution of forest ages to what parameter we choose for climax. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />

This approach asserts no strong assumptions about which fallow classes are the most common. Alternatively, we can move the probability density around to create younger or older forests. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />


# The Neutral Model 

Suppose we simulate game abundance over `N_days`. The number of game observed `A` depends on both the probability of detection, `prob_detect`, and the characteristics of the fallows. In the neutral model, we will keep the *effect* of fallows essentially random. 


```r
set.seed(25)
N_days <- 2e3 
prob_detect <- 0.1
```
    
    
    
    
    
    
    
    
