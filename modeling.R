
library(tidyverse)
library(tidybayes)
library(modelr)
library(brms)
teq <- read.csv(file = "C:\\Users\\scagg\\Documents\\Shane's Projects\\Data\\At home\\TequilaTasting.csv")


teq$.Price <- scale(teq$Price)
teq$.Sweetness <- scale(teq$Sweetness)

# formulae
formS <- bf( Smoothness ~ 1 + (.Sweetness | Drinker) + (.Price | Type), family = cumulative() )
formA <- bf( Aftertaste ~ 1 + (.Sweetness | Drinker) + (.Price | Type), family = cumulative() )
formC <- bf( Complexity ~ 1 + (.Sweetness | Drinker) + (.Price | Type), family = cumulative() )




# prior
get_prior(formS, data=teq)

prior1 <- c( prior( exponential(0.1), class = 'sd' ), 
             prior( normal(0, 10),    class = 'Intercept' ), 
             prior( lkj(2),           class = 'cor' ))
prior1

# initial values 
iseq <- seq(-2,2, length=8)

inits <- list(`Intercept[1]` = iseq[1], 
              `Intercept[2]` = iseq[2], 
              `Intercept[3]` = iseq[3], 
              `Intercept[4]` = iseq[4], 
              `Intercept[5]` = iseq[5], 
              `Intercept[6]` = iseq[6], 
              `Intercept[7]` = iseq[7], 
              `Intercept[8]` = iseq[8] )

inits_list <- list(inits, inits)

ppsim <- brm( data = teq, family = cumulative(), 
              Smoothness ~ 1, 
              prior = prior(normal(5.5, 1.5), class='Intercept'),
              iter = 1000, warmup = 1000, cores = 2, chains = 2, 
              inits = inits_list,
              sample_prior = 'only' )
