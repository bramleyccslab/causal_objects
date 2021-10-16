
rm(list=ls())
set.seed(231)

library(tidyverse)
source('models/exp_1/shared.R') # Helper functions
tasks = read.csv('models/exp_1/data/tasks.csv') # Experiment 1 task setup

load('models/exp_1/data/hypos.Rdata')
df.hypos = exp1.hypos
likelis = exp1.likelihoods

load('models/exp_1/data/behavioral.Rdata')
ppt_data<-rbind(
  (df.sels%>%filter(sequence=='default')%>%mutate(condition='near')%>%select(learningTaskId, condition, trial, selection, n)),
  (df.sels%>%filter(sequence=='reverse')%>%mutate(condition='far')%>%select(learningTaskId, condition, trial, selection, n))
) %>%
  select(learningTaskId, sequence=condition, trial, object=selection, n)
df.hypos$hid<-seq(1:nrow(df.hypos))
N=10000 # Number of Gibbs iterations


# Build up the process
# The code below in the sim_preds function implements the Gibbs sampler for our LoCaLaPro model
# See lines 404-427, pp.12-13
sim_preds<-function(lid, seq, alpha, beta, gamma) {
  preds<-vector("list", 15) # store prediction for each trial
  cats<-list() # list of categories
  cat_unit<-if (gamma==1|gamma==0) 1 else 2
  cat_funcs<-list() # causal functions assigned to categories
  cond<-paste0('learn0',lid) # lid: learning condition ID
  postcol<-paste0('post_l', lid)
  ld<-tasks %>% # learn data
    filter(phase=='learn', learningTaskId==cond) %>% 
    select(agent, recipient, result) %>% 
    as.list()
  lls<-likelis[[cond]]
  # Assign ld to the first cat, line 2 in Algorithm 1, p.12
  cats[[1]]<-Map('+', read_data_feature(ld, gamma), init_feat_dist(beta))
  # Sample a function for it from posterior, line 3 in Algorithm 1, p.12
  func_id<-sample(df.hypos$hid, 1, prob=df.hypos[,postcol])
  # Record this category, line 4 in Algorithm 1, p.12
  cat_funcs[[1]]<-c(func_id, df.hypos[func_id, 'hypo'])
  # Greedily assign categories, lines 5-15 in in Algorithm 1, p.12
  for (i in seq(15)) {
    # Read the correct trial data given generalization task sequence condition
    tid<-if (seq=='near') i else 16-i
    td<-tasks %>% filter(phase=='gen', learningTaskId==cond, trial==tid) %>% select(agent, recipient) %>% as.list()
    td_feats<-read_data_feature(td, gamma)
    th_preds<-lls[[tid]]
    # Check the probability of belonging to each existing category
    # Line 6 in Algorithm 1, p.12
    unnorm_probs<-vector()
    for (ci in (1:length(cats))) {
      cat<-cats[[ci]]
      # Feature similarity, Equation 7, p.6
      dir_join<-Reduce('+', Map('*', td_feats, cat))/Reduce('+',cat)
      # CRP for group size prior, Equation 6, p.6
      cat_size<-(Reduce('+', cat)-beta*6)/cat_unit
      crp_join<-cat_size/(i+alpha) 
      # Equation 5, p.6
      unnorm_probs<-c(unnorm_probs, dir_join*crp_join)
    }
    # Or creating a new category, lines 11-14 in Algorithm 1, p.12
    cat_new<-Map('+', init_feat_dist(beta), td_feats)
    dir_new<-Reduce('+', Map('*', td_feats, cat_new))/Reduce('+',cat_new)
    crp_new<-alpha/(i+alpha)
    unnorm_probs<-c(unnorm_probs, dir_new*crp_new)
    # Assign categories accordingly
    assigned_ci<-sample(1:length(unnorm_probs), 1, prob=unnorm_probs)
    if (assigned_ci>length(cats)) {
      cats[[assigned_ci]]<-cat_new
      # For newly-created cat, sample a function from the prior
      # Line 12 in Algorithm 1, p.12
      func_id<-sample(df.hypos$hid, 1, prob=df.hypos$prior)
      cat_funcs[[assigned_ci]]<-c(func_id, df.hypos[func_id, 'hypo'])
    } else {
      cats[[assigned_ci]]<-Map('+', cats[[assigned_ci]], td_feats)
    }
    # Get predictions
    hid<-cat_funcs[[assigned_ci]][1] %>% as.numeric()
    preds[[tid]]<-sample(names(th_preds[[hid]]), 1, prob=unlist(th_preds[[hid]]))
  }
  return(preds)
}

# Get simulation results
# Equation 11-12, pp.7-8
sim_all<-function(alpha, beta, gamma, N) {
  model.proc<-expand.grid(
    learningTaskId=paste0('learn0',seq(6)), 
    sequence=c('near', 'far'),
    trial=seq(15), 
    object=all_objects, 
    sim=0, stringsAsFactors = F
  ) %>%
    arrange(learningTaskId, desc(sequence), trial, object)
  
  counter=0
  
  i=1
  s='near'
  cond<-paste0('learn0', i)
  while (counter<N) {
    # Run Gibbs sampler
    sims<-sim_preds(i, s, alpha, beta, gamma)
    for (x in 1:length(sims)) {
      rownum<-which(
        model.proc$learningTaskId==cond & model.proc$sequence==s &
        model.proc$trial==x & model.proc$object==sims[[x]]
      )
      model.proc[rownum, 'sim']<-model.proc[rownum, 'sim']+1        
    }
    counter<-counter+1
  }

  return(model.proc)
}

# Fit softmax
# Lines 322-329, p.9
full_model<-function(par, mdata) {
  for (i in seq(6)) {
    for (s in c('near', 'far')) {
      for (j in seq(15)) {
        data<-mdata %>%
          filter(learningTaskId==paste0('learn0',i), sequence==s, trial==j) %>%
          select(learningTaskId, sequence, trial, object, o_prob=prob)
        data$softmaxed<-softmax(data$o_prob, par)
        mdata<-mdata %>%
          left_join(data, by=c('learningTaskId', 'sequence', 'trial', 'object')) %>%
          mutate(prob_s=ifelse(is.na(softmaxed), prob_s, softmaxed)) %>%
          select(learningTaskId, sequence, trial, object, prob, n, prob_s)
      }
    }
  }
  return(-sum(mdata$n*log(mdata$prob_s)))
}


# Grid search
# Lines 441-453, p.13
alphas<-c(seq(from=1,to=10,by=.5), 2^(4:10))
betas<-c(seq(0,1,.1), 2^(1:10))
gammas<-c(0,.25,.5,.75,1)

grid.fits<-expand.grid(
  alpha=alphas, beta=betas, gamma=gammas, raw_ll=0, t=0, fitted_ll=0
)
grid.fits$pid<-seq(nrow(grid.fits))
grid.fits<-grid.fits %>% select(pid, alpha, beta, gamma, raw_ll, t, fitted_ll)
grid.preds<-list()

params<-list(alpha=0, beta=0.1, gamma=0.5)
for (i in 1:nrow(grid.fits)) {
  params<-grid.fits[i,] %>% as.list()
  model.proc<-sim_all(params[['alpha']], params[['beta']], params[['gamma']], N)
  grid.preds[[i]]<-model.proc
  
  model_data<-model.proc %>%
    mutate(prob=sim/N, prob_s=NA) %>%
    left_join(ppt_data, by=c('learningTaskId', 'sequence', 'trial', 'object'))
  raw_ll_val<-model_data %>%
    filter(n>0) %>%
    summarise(ll=sum(n*log(prob)))
  
  out<-optim(par=1, fn=full_model, mdata=model_data, method='Brent', lower=0, upper=100)
  grid.fits[i,'raw_ll']<-raw_ll_val[1,'ll']
  grid.fits[i,'t']<-out$par
  grid.fits[i,'fitted_ll']<--out$value
  
  log<-as.list(grid.fits[i,])
  log[['perc']]<-paste0(round(100*i/nrow(grid.fits),2),'%')
  write.csv(as.data.frame(log), file='_process')
}
exp1.localapro = model.proc

