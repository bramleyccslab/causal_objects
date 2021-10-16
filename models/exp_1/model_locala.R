
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

# Create LoCaLa model
get_model_preds<-function(alpha, beta, t='', gamma) {
  model.cat<-data.frame(
    learningTaskId=character(0), trial=numeric(0),
    object=character(0), prob=numeric(0), prob_s=numeric(0)
  )
  # The two lines of code blow implement CRP priors
  # See Equation 6, lines 225-230 p.6 in the manuscript
  crp_join<-alpha/(alpha+1)
  crp_new<-1/(alpha+1)
  # Predictions
  for (i in seq(6)) {
    cond<-paste0('learn0', i)
    post_col<-paste0('post_l',i)
    learn_task<-tasks %>% filter(phase=='learn', learningTaskId==cond) %>%
      select(agent, recipient, result) %>% as.list()
    for (j in seq(15)) {
      task_data<-tasks %>% 
        filter(phase=='gen', learningTaskId==cond, trial==j) %>%
        select(agent, recipient) %>% paste0(., collapse=',')
      # The five lines of code below implements feature similarity following a 
      # Dirichlet prior and multinomial distribution
      # See lines 235-248, pp.6-7
      feats<-read_data_feature(task_data, gamma)
      cat_join<-Map('+', init_feat_dist(beta), read_data_feature(learn_task, gamma))
      cat_new<-Map('+', init_feat_dist(beta), feats)
      dir_join<-Reduce('+', Map('*', feats, cat_join))/Reduce('+',cat_join)
      dir_new<-Reduce('+', Map('*', feats, cat_new))/Reduce('+',cat_new)
      # The lines of code below forms latent causal categories, pp.7-8
      mix<-normalize(c(crp_join*dir_join, crp_new*dir_new)) # Equation 8, p.7
      ll<-likelis[[cond]][[j]]
      preds<-lapply(1:nrow(df.hypos), function(x) {
        # Make use of the hypo posterior likelihood lookup table to facilitate computation
        Map('+',
            Map('*', ll[[x]], (df.hypos[x,post_col]*mix[1])),
            Map('*', ll[[x]], (df.hypos[x,'prior']*mix[2]))
        )
      }) %>%
        reduce(function(a,b) Map('+', a, b)) 
      # THe three lines of code below implement posterior predictive for the LoCaLa model
      # Equation 11-12, pp.7-8
      preds.data<-data.frame(object=names(preds), prob=unlist(preds)) %>%
        mutate(learningTaskId=cond, trial=j, prob_s=NA) %>%
        select(learningTaskId, trial, object, prob, prob_s)
      # Add softmax, lines 377-383, pp.11-12
      if (typeof(t)=='double') preds.data$prob_s<-softmax(preds.data$prob, t)
      model.cat<-rbind(model.cat, preds.data)
    }
  }
  return(model.cat)
}

# Fit parameters: c(alpha=,beta=,t=, g=)
# See Experiment 1 Model Fix section, lines 391-403, p.12
fit_me<-function(par) {
  gamma<-exp(par[4])/(1+exp(par[4]))
  preds<-get_model_preds(par[1],par[2],par[3], gamma)
  ppt<-df.sels %>%
    filter(sequence=='combined') %>%
    select(learningTaskId, trial, object=selection, n)
  data<-preds%>%
    mutate(object=as.character(object)) %>%
    left_join(ppt, by=c('learningTaskId', 'trial', 'object'))
  return(-sum(data$n*log(data$prob_s)))
}

# Fix Gamma = 0.5, line 401-403, p.12
out<-optim(par=c(1, 0.1, 1), fit_me, method="L-BFGS-B", lower=c(0, 0, 0))
exp1.locala = model.cat


