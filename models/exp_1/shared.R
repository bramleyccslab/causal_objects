
#### libraries ####
library(tidyverse)
options(scipen=10)

#### Glabal variables ####
# configs
feature_setting<-list()
feature_setting[['color']]<-c('r','b','y')
feature_setting[['shape']]<-c('c','s','d')

# defined according to configs
all_objects<-vector()
for (c in feature_setting[[1]]) {
  for (s in feature_setting[[2]]) {
    all_objects<-c(all_objects, paste0(c,s))
  }
}

init_dist<-function() {
  dist<-list()
  for (o in all_objects) dist[[o]]<-0
  return(dist)
}
dist<-init_dist()


all_data_str<-vector()
for (a in all_objects) {
  for (r in all_objects) {
    all_data_str<-c(all_data_str, paste(c(a,r),collapse=',')) 
  }
}

#### Evaluation ####
color<-function(x) substr(x,1,1)
shape<-function(x) substr(x,2,2)
and<-function(x, y) {
  if (missing(x)) x<-NA
  if (missing(y)) y<-NA
  return(x&y)
}
equal<-function(x,y) x==y
neq<-function(x,y) x!=y

normalize<-function(raw) {
  if (typeof(raw)=='list') {
    sum<-Reduce('+', raw)
    if (sum==0) return(raw) else {
      for (r in names(raw)) raw[[r]]<-raw[[r]]/sum; return(raw)
    }
  } else return(raw/sum(raw))
}

softmax<-function(vec, base=0, type='') {
  if (type!='log') {
    v_exp<-exp(vec*base); sum<-sum(v_exp)
  } else {
    v_exp<-exp(log(vec)*base); sum<-sum(v_exp)
  }
  return(v_exp/sum)
}


listify_data<-function(data_str) {
  if (typeof(data_str)=='list') return(data_str) else {
    obs<-strsplit(data_str, ',')[[1]]
    return(list(agent=obs[1], recipient=obs[2], result=obs[3]))
  }
}

causal_mechanism<-function(hypo, data) {
  data<-listify_data(data)
  
  # If effect clause only spec one feature, set the missing one to default
  has_color<-grepl('color', hypo)
  has_shape<-grepl('shape', hypo)
  if (!(has_color&has_shape)) {
    default<-if (!has_color) 'color' else 'shape'
    hypo<-paste0("and(",hypo,",equal(",default,"(M),",default,"(R)))")
  }
  
  # Replace values
  hypo<-gsub('A', paste0("'",data$agent,"'"), hypo)
  hypo<-gsub('R', paste0("'",data$recipient,"'"), hypo)

  dist<-init_dist()
  for (d in names(dist)) {
    hypo_text<-gsub('M', paste0("'",d,"'"), hypo)
    dist[[d]]<-as.numeric(eval(parse(text=hypo_text)))
  }
  dist<-normalize(dist)
  
  if (is.na(data$result)) {
    return(dist)
  } else {
    return(dist[[data$result]])
  }
}
 

# for category resemblance
init_feat_dist<-function(beta=0) {
  feat_dist<-list()
  for (e in feature_setting[['color']]) feat_dist[[paste0('c',e)]]<-beta
  for (s in feature_setting[['shape']]) feat_dist[[paste0('s',s)]]<-beta
  return(feat_dist)
}

read_feature<-function(data, type='A') {
  if (typeof(data)!='list') data<-listify_data(data)
  feat_dist<-init_feat_dist(0)
  if (type=='A') data<-data[c('agent')] else
    if (type=='AR') data<-data[c('agent', 'recipient')] else
      if (type=='R') data<-data[c('recipient')]
  # read obs feature value
  for (i in 1:length(data)) {
    color_val<-paste0('c',color(data[[i]]))
    feat_dist[[color_val]]<-feat_dist[[color_val]]+1

    shape_val<-paste0('s',shape(data[[i]]))
    feat_dist[[shape_val]]<-feat_dist[[shape_val]]+1
  }
  return(feat_dist)
}

# Gamma is constraint to 0 to 1
# Use sigmoid function if fit this to unconstrained values
read_data_feature<-function(data, gamma) {
  feat_a<-lapply(read_feature(data, 'A'), function(x) x * gamma)
  feat_r<-lapply(read_feature(data, 'R'), function(x) x * (1-gamma))
  return(mapply(sum, feat_a, feat_r, SIMPLIFY=F))
}

















