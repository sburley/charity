# PREDICT 422 Winter 2018
# Course Project
# Scott Burley

# Summary: Read a file 'charity.csv' in the same folder. Transform data and fit
# models to predict DAMT and DONR variables. Export the results as 'result.csv'
# in the same folder.

library(tidyverse)
library(class)  # for knn()

## Import data ##
charity <- read_csv('charity.csv')

## Transform ##
# Combine REG variables into one
charity <- charity %>% 
  mutate(reg = reg1 + 2*reg2 + 3*reg3 + 4*reg4) %>%
  select(-c(reg1, reg2, reg3, reg4))

# Transformations for DONR model, unneeded variables removed
charity.knn <- charity %>% 
  mutate_at(c('tgif', 'incm'), log) %>%
  select(-c(inca, rgif, avhv, genf, agif, lgif, plow, npro)) %>% 
  mutate_at(vars(-ID, -donr, -damt, -part), scale)

# Transformations for DAMT model, unneeded variables removed
charity.ols <- charity %>% 
  mutate_at(c('hinc', 'wrat', 'reg'), factor) %>%
  mutate_at(c('tgif', 'lgif', 'agif', 'rgif', 'incm'), log) %>%
  select(-c(inca, tdon, npro, tlag, avhv, genf))

## Split data ##
# Note that we are combining training and validation datasets so as to refit
# models with as many observations as possible
test.ols <- filter(charity.ols, part == 'test')
train.ols <- filter(charity.ols, part != 'test')
test.knn <- filter(charity.knn, part == 'test')
train.knn <- filter(charity.knn, part != 'test')

## Fit models & score test data ##
# DONR model
mod.donr <- knn(train=train.knn %>% select(-c(ID, donr, damt, part)),
                test=test.knn %>% select(-c(ID, donr, damt, part)),
                cl=train.knn$donr,
                k=16,
                prob=T,
                use.all=T
)

# knn() returns scores as factors with the probability of the winning score as
# an attribute. We need to convert P(winner) into P(DONR == 1).
pred.donr <- 1 - attr(mod.donr, 'prob') +
  (as.integer(mod.donr) - 1)*(2*attr(mod.donr, 'prob') - 1)

# Apply profit-maximizing threshold
pred.donr <- as.numeric(pred.donr > 0.7059)

# DAMT model
mod.damt <- lm(damt ~ . , 
               data=train.ols %>% filter(donr == 1) %>% 
                 select(-c(ID, donr, part))
)

pred.damt <- predict(mod.damt, newdata=test.ols)

## Write scores to file ##
result <- data.frame(ID=test.ols$ID, donr=pred.donr, damt=pred.damt)
write_csv(result, 'result.csv')