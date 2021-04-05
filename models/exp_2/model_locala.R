
gamma=0
alphas<-c(1:10, 2^(4:10))
betas<-c(seq(0,1,.1), 2^(1:10))
set.seed(231)

drop<-500
slice<-1
iter<-10000
softmax_base<-''

source('shared.R')
source('gibbs.R')
source('preds.R')
load('hypos.Rdata')
load('mturk_main.Rdata')
tasks<-read.csv('tasks.csv')
n_learn_obs<-length(unique((tasks%>%filter(phase=='learn'))$task))
n_gen_obs<-length(unique((tasks%>%filter(phase=='gen'))$task))

default<-expand.grid(condition=paste0('A',1:4), trial=1:16, result=all_objects, stringsAsFactors=F)
counts<-df.tw %>%
  filter(phase=='gen'&grepl('gen', sid)) %>%
  mutate(trial=as.numeric(substr(sid,8,9)), result=as.character(result)) %>%
  select(ix, condition, trial, result) %>%
  arrange(condition, trial) %>%
  group_by(condition, trial, result) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  right_join(default, by=c('condition', 'trial', 'result')) %>%
  mutate(count=ifelse(is.na(count), 0, count)) %>%
  select(group=condition, trial, object=result, count)

# Set up result objects
dp_raw_preds<-list()
dp_grid<-data.frame(
  id=numeric(0), alpha=numeric(0), beta=numeric(0), gamma=numeric(0), 
  raw_ll=numeric(0), fitted_base=numeric(0), fitted_ll=numeric(0)
)

full_grid<-data.frame(id=numeric(0), alpha=numeric(0), beta=numeric(0))
n=1
for (a in alphas) {
  for (b in betas) {
    full_grid[n,'id']<-n
    full_grid[n,'alpha']<-a
    full_grid[n,'beta']<-b
    n=n+1
  }
}

for (n in 1:nrow(full_grid)) {
  a = full_grid[n, 'alpha']
  b = full_grid[n, 'beta']

  dp_grid[n, 'id']<-n
  dp_grid[n, 'alpha']<-a
  dp_grid[n, 'beta']<-b
  dp_grid[n, 'gamma']<-gamma
  
  log<-data.frame(nth=n, alpha=a, beta=b)
  write.csv(log, file='dp_current')

  # Run Gibbs sampler with given parameters
  preds<-data.frame(
    group=character(0), trial=numeric(0), object=numeric(0),
    prob=numeric(0), type=character(0)
  )
  # Get categories & make predictions
  for (c in 1:4) {
    cond<-paste0('A', c)
    x<-run_gibbs_sampler(cond, gamma, a, b, iter, F)
    cats<-read_cats(x[[1]], base=softmax_base, burn_in=drop, thinning=slice)
    func_preds<-prep_preds(x[[2]], cond)
    preds<-rbind(preds,
                 get_cond_preds(cond, cats, func_preds, a, b, gamma))
  }
  dp_raw_preds[[n]]<-preds

  # Compute (log) likelihoods
  data<-preds %>% left_join(counts, by=c('group', 'trial', 'object'))
  # Log likelihood
  raw_ll<-data %>%
    filter(count > 0) %>%
    summarise(sum(log(prob)*count))
  dp_grid[n,'raw_ll']<-raw_ll

  # Fit log-likelihood
  data_likeli<-function(b) {
    ds<-filter(data, group=='A1', trial==1) %>% mutate(softmaxed=softmax(prob, b))
    for (c in 1:4) {
      for (i in 1:16) {
        if (!(c==1 & i==1)) {
          ds<-rbind(ds,
                    filter(data, group==paste0('A', c), trial==i) %>%
                      mutate(softmaxed=softmax(prob, b)))
        }
      }
    }
    ds<-ds%>%filter(count>0)
    ll<-sum(ds$count*log(ds$softmaxed))
    return(-ll)
  }

  out<-optim(par=0, fn=data_likeli, method='Brent', lower=0, upper=100)
  dp_grid[n, 'fitted_base']<-out$par
  dp_grid[n, 'fitted_ll']<-out$value

  save(dp_grid, dp_raw_preds, file='../results/dp.Rdata')
}

# Put model fit results together
model_refs<-expand.grid(alpha=alphas, beta=betas, gamma=gammas)
model_refs$id=1:nrow(model_refs)
model_refs<-model_refs%>%select(id, alpha, beta, gamma)

# Put together fit results
get_model_fits<-function(df, g) {
  data<-model_refs %>%
    filter(gamma==g) %>%
    left_join((df %>% mutate(gamma=g, oid=id) %>% select(-id)), 
              by=c('alpha', 'beta', 'gamma'))
  return(data)
}

model_fits<-get_model_fits(dpa_grid_2, 1)
model_fits<-rbind(model_fits, get_model_fits(dpr_grid_2, 0))
model_fits<-rbind(model_fits, get_model_fits(dp25_grid, .25))
model_fits<-rbind(model_fits, get_model_fits(dp50_grid, .5))
model_fits<-rbind(model_fits, get_model_fits(dp75_grid, .75))
model_fits<-model_fits %>% arrange(id)

# Put together raw predictions
model_preds<-vector('list', length=nrow(model_refs))

for (i in 1:length(dpa_raw_preds_2)) {
  id<-model_fits%>%filter(gamma==1, oid==i) %>% pull(id)
  model_preds[[id]]<-dpa_raw_preds_2[[i]]
}
for (i in 1:length(dpr_raw_preds_2)) {
  id<-model_fits%>%filter(gamma==0, oid==i) %>% pull(id)
  model_preds[[id]]<-dpr_raw_preds_2[[i]]
}
for (i in 1:length(dp25_raw_preds)) {
  dp25_ref_id<-model_fits%>%filter(gamma==.25, oid==i) %>% pull(id)
  dp50_ref_id<-model_fits%>%filter(gamma==.5, oid==i) %>% pull(id)
  dp75_ref_id<-model_fits%>%filter(gamma==.75, oid==i) %>% pull(id)

  model_preds[[dp25_ref_id]]<-dp25_raw_preds[[i]]
  model_preds[[dp50_ref_id]]<-dp50_raw_preds[[i]]
  model_preds[[dp75_ref_id]]<-dp25_raw_preds[[i]]
}

# Save data
save(model_refs, model_fits, model_preds, file='model_locala.Rdata')
