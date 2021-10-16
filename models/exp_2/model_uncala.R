
# Import libraries, helpers, and data
library(tidyverse)
source('models/exp_2/functions/shared.R')
load('models/exp_2/data/hypos.Rdata')
tasks<-read.csv('models/exp_2/data/tasks.csv')

################################################################
# More helper functions
fetch_task<-function(group_name, phase_name, trial_id, type='list', source=tasks) {
  task_data<-source%>%
    filter(condition==group_name&phase==phase_name&task==trial_id)%>%
    select(agent, recipient, result)
  if (type=='s') return(paste(task_data, collapse=',')) else return(as.list(task_data))
}

listify_task<-function(task_str) {
  if (typeof(task_str)=='list') return(task_str) else {
    task_els<-strsplit(task_str, ',')[[1]]
    if (task_els[3]==0) task_els[3]<-NA
    return(list(agent=task_els[1], recipient=task_els[2], result=task_els[3]))
  }
}

get_one_gen_pred<-function(group_name, trial_id, learn_post) {
  data<-fetch_task(group_name, 'gen', trial_id)
  post_col<-paste0('post_', group_name)
  df<-learn_post[(learn_post[,post_col]>0),]
  result<-unlist(init_dist())
  for (i in 1:nrow(df)) {
    result<-result+(unlist(causal_mechanism(df$hypo[i], data)))*df[i, post_col]
  }
  return(data.frame(group=group_name, phase='gen', trial=trial_id, 
                    object=all_objects, pred=normalize(result)))
}


################################################################
# Get generalization predictions from learning posteriors
# Same idea as in lines 384-390, p.12, but with Experiment 2 learning posterior (of causal functions/hypos)
ce_preds<-data.frame(group=character(0), phase=character(0), trial=numeric(0), pred=character(0), pred=numeric(0))
for (i in 2:4) {
  group_name=paste0('A', i)
  learned<-df.hypos[,c('hypo', paste0('post_', group_name))]
  prediction<-get_one_gen_pred(group_name, 1, learned)
  for (j in 2:16) {
    prediction<-rbind(prediction, get_one_gen_pred(group_name, j, learned))
  }
  ce_preds<-rbind(ce_preds, prediction)
}
ce_preds<-ce_preds%>%mutate(source='causal_grouped')%>%select(group, trial, object, prob=pred, source)
ce_preds$object<-as.character(ce_preds$object)
exp2.uncala<-ce_preds

# Fit a softmax and check likelihood, lines 337-383, pp.11-12
baseline_ll<-nrow(df.sw)*16*log(1/20)

counts<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(
    group=condition,
    trial=as.numeric(substr(sid,8,9)), 
    object=as.character(result)) %>%
  group_by(group, trial, object) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(ce_preds, by=c('group', 'trial', 'object')) %>%
  mutate(count=ifelse(is.na(count), 0, count)) %>%
  select(group, trial, object, count, prob=pred)

fit_ll<-function(b, data, type='fit') {
  softed<-filter(data, group=='A1', trial==1)%>%mutate(soft=softmax(prob, b))
  for (c in 1:4) {
    for (i in 1:16) {
      if (!(c==1 & i==1)) {
        softed<-rbind(
          softed, 
          filter(data, group==paste0('A',c), trial==i)%>%mutate(soft=softmax(prob, b)))
      }
    }
  }
  if (type=='fit') {
    return(-sum(softed$count*log(softed$soft)))
  } else {
    return(softed)
  }
}
fit_ll(1, counts)

out<-optim(par=0, fn=fit_ll, data=counts, method='Brent', lower=0, upper=100)
out$par #3.19
out$value #3706.359

# Random baseline
101*16*log(1/20) # -4841.103











