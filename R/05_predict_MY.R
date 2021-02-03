# Purpose: Predict probability of tagged individuals migrating during different 
# migration years.
# Author: Ryan N. Kinzer

#load packages
library(tidyverse)
library(caret)
#library(e1071)

# load combined data rec'd from dart
load('./data/allObs.rda')

dat <- mod_dat %>%
  filter(release_site %in% c('JOHTRP', 'SECTRP')) %>%
  mutate(overwinter = relevel(overwinter, ref = '0'),
         release_year = factor(release_year)) %>%
  select(overwinter, release_site, release_year, length, tag_season) %>%
  drop_na() %>%
  rowid_to_column()

dat$overwinter <- droplevels(dat$overwinter)

# split data
train <- dat %>% slice_sample(prop = .7)
test <- dat %>% filter(!(rowid %in% train$rowid))

# check observation balance
percentage <- prop.table(table(train$overwinter))*100
cbind(freq=table(train$overwinter), percentage=percentage)

prop.table(table(test$overwinter))*100

# model set-up using 10-fold cross-validation
control <- trainControl(method="cv", number = 10)
metric <- "Accuracy"

# set seed to ensure comparability across model results
# a) linear algorithms
set.seed(10)
fit.lda <- train(overwinter ~ length + release_site + tag_season, data=train, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(10)
fit.cart <- train(overwinter ~ length + release_site + tag_season, data=train, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(10)
fit.knn <- train(overwinter ~ length + release_site + tag_season, data=train, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(10)
fit.svm <- train(overwinter ~ length + release_site + tag_season, data=train, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(10)
fit.rf <- train(overwinter ~ length + release_site + tag_season, data=train, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# plot results
dotplot(results)

print(fit.lda)

# estimate skill of LDA on the test dataset
test$predictions <- predict(fit.lda, test)
confusionMatrix(test$predictions, test$overwinter)

test %>%
  ggplot(aes(x = length, y = overwinter, colour = predictions)) +
  geom_jitter(size = 2) +
  facet_grid(tag_season ~ release_site) +
  labs(x = 'Fork Length at Tagging',
       y = 'Overwinters from Tagging')


# Predict migration year for tagging data
johtrp <- read_csv('./data/tag_data/johtrp_32W.csv')
sectrp <- read_csv('./data/tag_data/sectrp_32W.csv')

tag_dat <- bind_rows(johtrp, sectrp) %>%
  mutate(release_time = lubridate::mdy_hms(release_time),
         length = as.integer(length),
  tag_season = case_when(
    between(lubridate::month(release_time),1,6) ~ 'Spring',
    between(lubridate::month(release_time),7,8) ~ 'Summer',
    between(lubridate::month(release_time),9,12) ~ 'Fall'),
  tag_season = fct_relevel(tag_season, 'Summer', 'Fall', 'Spring'),
  overwinter = NA,
  release_year = as.factor(lubridate::year(release_time))
  ) %>%
  select(tag_id, overwinter, release_site, release_year, length, tag_season) %>%
  rowid_to_column() %>%
  filter(!is.na(length)) %>%
  filter(!is.na(release_site)) %>%
  filter(!is.na(tag_season))
  
# estimate out-migration year
tag_dat$predictions <- predict(fit.lda, tag_dat)

write_csv(tag_dat, file = './data/tag_data/JCAPE_32W_MY_predictions.csv')


