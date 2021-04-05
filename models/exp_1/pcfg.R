
rm(list=ls())
library(tidyverse)

tasks = read.csv('tasks.csv')
source('shared.R')

# Enumerate hypos
single_feat_hypos<-function(feat) {
  hypo_vec<-c()
  for (e in c('equal', 'neq')) {
    hypo_vec<-c(hypo_vec, paste0(e,'(',feat,'(M),',feat,'(A))'))
    hypo_vec<-c(hypo_vec, paste0(e,'(',feat,'(M),',feat,'(R))'))
    for (v in feature_setting[[feat]]) {
      hypo_vec<-c(hypo_vec, paste0(e,'(',feat,'(M),',"'",v,"'",')'))
    }
  }
  return(hypo_vec)
}
color_hypos<-single_feat_hypos('color')
shape_hypos<-single_feat_hypos('shape')
hypos<-c(color_hypos, shape_hypos)
for (ch in color_hypos) {
  for (sh in shape_hypos) {
    hypos<-c(hypos, paste0('and(',ch,',',sh,')'))
  }
}
df.hypos<-data.frame(hypo=hypos, stringsAsFactors = F)

# Priors
get_prior<-function(hypo) {
  hasAnd<-grepl('and',hypo)
  relative_infs<-length(grep('A|R',strsplit(hypo,'')[[1]]))
  absolute_infs<-(hasAnd+1)-relative_infs
  prior<-(1/2)^(2*3)*(1/2)^relative_infs*(1/3)^absolute_infs
  return(prior)
}
df.hypos$prior<-mapply(get_prior, df.hypos$hypo)
df.hypos$prior<-normalize(df.hypos$prior)

# Posteriors
for (i in seq(6)) {
  cond<-paste0('learn0',i)
  data<-tasks %>% filter(phase=='learn', learningTaskId==cond) %>% 
    select(agent, recipient, result) %>% 
    paste0(.,collapse=',')
  ll_col<-paste0('ll_l',i)
  post_col<-paste0('post_l',i)
  df.hypos[,ll_col]<-mapply(causal_mechanism, df.hypos$hypo, rep(data,nrow(df.hypos)))
  df.hypos[,post_col]<-df.hypos[,'prior']*df.hypos[,ll_col]
  df.hypos[,post_col]<-normalize(df.hypos[,post_col])
}
df.hypos<-df.hypos %>%
  select(hypo, prior, starts_with('post_'))

# Predictions likelihoods
likelis<-list()
for (i in seq(6)) {
  cond<-paste0('learn0', i)
  likelis[[cond]]<-list()
  for (j in seq(15)) {
    task_data<-tasks %>% 
      filter(phase=='gen', learningTaskId==cond, trial==j) %>%
      select(agent, recipient) %>% paste0(., collapse=',')
    preds<-lapply(1:nrow(df.hypos), function(x) {
      causal_mechanism(df.hypos$hypo[x], task_data)
    })
    likelis[[cond]][[j]]<-preds
  }
}

exp1.hypos = df.hypos
exp1.likehoods = likelis
save(exp1.hypos, exp1.likehoods, file='models/exp_1/hypos.Rdata')

