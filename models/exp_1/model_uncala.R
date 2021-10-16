
rm(list=ls())
library(tidyverse)

tasks = read.csv('models/exp_1/data/tasks.csv') # Experiment 1 task setup
load('models/exp_1/data/behavioral.Rdata')
load('models/exp_1/data/hypos.Rdata')
source('models/exp_1/shared.R') # Helper functions

# No order difference, use the combined mturk data
df.sels = df.sels %>% filter(sequence=='combined')
df.hypos = exp1.hypos
likelis = exp1.likelihoods

# Posterior predictives placeholder
model.uni<-data.frame(
  learningTaskId=character(0), trial=numeric(0),
  object=character(0), prob=numeric(0)
)

# Implementation of the UnCaLa model
# Lines 384-390, p. 12
for (i in seq(6)) {
  cond<-paste0('learn0', i)
  post_col<-paste0('post_l',i)
  for (j in seq(15)) {
    ll<-likelis[[cond]][[j]]
    preds<-lapply(1:nrow(df.hypos), function(x) {
      Map('*', ll[[x]], df.hypos[x,post_col])
    }) %>%
      reduce(function(a,b) Map('+', a, b))
    preds.data<-data.frame(object=names(preds), prob=unlist(preds)) %>%
      mutate(learningTaskId=cond, trial=j) %>%
      select(learningTaskId, trial, object, prob)
    model.uni<-rbind(model.uni, preds.data)
  }
}

# Fit softmax
# Equation 14 and lines 429-433, pp.11 & 13
fit_softmax<-function(par) {
  softed<-data.frame(
    learningTaskId=character(0),
    trial=numeric(0),
    object=character(0),
    prob=numeric(0),
    soft=numeric(0)
  )
  for (i in seq(6)) {
    for (j in seq(15)) {
      dt<-model.uni %>% filter(learningTaskId==paste0('learn0',i), trial==j)
      dt$prob_s<-softmax(dt$prob, par)
      softed<-rbind(softed, dt)
    }
  }
  ppt_data<-df.sels %>%
    filter(sequence=='combined') %>%
    select(learningTaskId, trial, object=selection, n)
  softed<-softed %>% 
    mutate(object=as.character(object)) %>%
    left_join(ppt_data, by=c('learningTaskId', 'trial', 'object')) %>%
    filter(n>0)
  return(-sum(softed$n*log(softed$prob_s)))
}

out<-optim(par=0, fn=fit_softmax, method='Brent', lower=0, upper=100)
out$par # 6.95571
out$value # 2760.672
exp1.uncala = model.uni

