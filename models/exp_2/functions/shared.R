
#### libraries ####
library(dplyr)
options(scipen=10)

#### Glabal variables ####
# configs
feature_setting<-list()
feature_setting[['edges']]<-seq(3,7)
feature_setting[['shades']]<-seq(4)

# defined according to configs
all_objects<-vector()
for (e in feature_setting$edges) {
  for (s in feature_setting$shades) {
    all_objects<-c(all_objects, paste(c(e,s), collapse=''))
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
edges<-function(x) floor(x/10)
shades<-function(x) x%%10
and<-function(x, y) {
  if (missing(x)) x<-NA
  if (missing(y)) y<-NA
  return(x&y)
}

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
  
  hypo<-gsub('A', data$agent, hypo)
  hypo<-gsub('R', data$recipient, hypo)
  
  input<-eval(parse(text=hypo))
  effect<-input$effect
  dist<-init_dist()
  
  if (input$cause=='') {
    set_default<-if (effect=='') T else F
  } else {
    cause_cond<-eval(parse(text=input$cause))
    set_default<-if (is.na(cause_cond)|!cause_cond) T else F
  }
  
  if (!set_default) {
    # If effect clause only spec one feature, set the missing one to default
    has_shades<-grepl('shades', effect)
    has_edges<-grepl('edges', effect)
    if (!(has_edges&has_shades)) {
      d_text<- if (has_edges) 'shades(M)==shades(R)' else 'edges(M)==edges(R)'
      effect<-paste0('and(', effect, ', ', d_text, ')')
      effect<-gsub('R', data$recipient, effect)
    }
    
    for (d in names(dist)) {
      effect_text<-gsub('M', d, effect)
      cond<-eval(parse(text=effect_text))
      if (is.na(cond)|is.null(cond)) {
        set_default<-T; break;
      } else {
        dist[[d]]<-as.numeric(cond)
      }
    }
  }
  
  if (set_default) {
    dist[[as.character(data$recipient)]]<-1
  } else {
    dist<-normalize(dist)
  }
  
  return(dist)
}

get_likeli<-function(hypo, data) {
  data<-listify_data(data)
  return(causal_mechanism(hypo, data)[[as.character(data$result)]])
}

# for category resemblance
init_feat_dist<-function(beta=0) {
  feat_dist<-list()
  for (e in feature_setting[['edges']]) feat_dist[[paste0('e',e)]]<-beta
  for (s in feature_setting[['shades']]) feat_dist[[paste0('s',s)]]<-beta
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
    edge_val<-paste0('e',edges(data[[i]]))
    feat_dist[[edge_val]]<-feat_dist[[edge_val]]+1
    
    shade_val<-paste0('s',shades(data[[i]]))
    feat_dist[[shade_val]]<-feat_dist[[shade_val]]+1
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

















