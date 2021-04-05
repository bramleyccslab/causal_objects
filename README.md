# Supplementary Data and Code

## Project info

### Manuscript
How do people generalize causal relations over objects? A non-parametric Bayesian account (submitting soon)

### Authors
[Bonan Zhao](http://bramleylab.ppls.ed.ac.uk/member/bonan/),
[Chris Lucas](https://homepages.inf.ed.ac.uk/clucas2/),
[Neil Bramley](http://bramleylab.ppls.ed.ac.uk/member/neil/)

### Abstract
How do people decide how general a causal relationship is, in terms of the entities or situations it applies to? What features do people use to decide whether a new situation is governed by a new causal law or an old one? How can people make these difficult judgments in a fast, efficient way? We address these questions in two experiments that ask participants to generalize from one (Experiment 1) or several (Experiment 2) causal interactions between pairs of objects. In each case, participants see an agent object act on a recipient object, causing some changes to the recipient. In line with the human capacity for few-shot concept learning, we find systematic patterns of causal generalizations favoring simpler causal laws that extend over categories of similar objects. In Experiment 1, we find that participants’ inferences are shaped by the order of the generalization questions they are asked. In both experiments, we find an asymmetry in the formation of causal categories: participants preferentially identify causal laws with features of the agent objects rather than recipients. To explain this, we develop a computational model that combines program induction (about the hidden causal laws) with non-parametric category inference (about their domains of influence). We demonstrate that our modeling approach can both explain the order effect in Experiment 1 and the causal asymmetry, and outperforms a na ̈ıve Bayesian account while providing a computationally plausible mechanism for real world causal generalization.

### Keywords
Causal reasoning · Generalization · Bayesian models · Inductive bias · Program induction · Dirichlet process

## Repo structure
```
.
├── behavioral_data
│   ├── exp1_subjects.csv
│   ├── exp1_trials.csv
│   ├── exp2_responses.csv
│   ├── exp2_subjects.csv
│   └── exp2_trials.csv
├── models
│   ├── exp_1
│   │   ├── data
│   │   │   ├── behavioral.Rdata
│   │   │   ├── hypos.Rdata
│   │   │   ├── model_data.Rdata
│   │   │   └── tasks.csv
│   │   ├── model_locala.R
│   │   ├── model_localapro.R
│   │   ├── model_uncala.R
│   │   ├── pcfg.R
│   │   └── shared.R
│   └── exp_2
│       ├── data
│       │   ├── hypos.Rdata
│       │   ├── model_locala.Rdata
│       │   ├── mturk_main.Rdata
│       │   └── tasks.csv
│       ├── functions
│       │   ├── gibbs.R
│       │   ├── preds.R
│       │   └── shared.R
│       ├── model_locala.R
│       ├── model_uncala.R
│       └── pcfg.R
└── README.md
```
**Caution**: Some load/read/write paths in the R scripts may be outdated because of restructuing. Please check with your local version and adjust accordingly.

## Useful links
- [Experiment 1 demo](http://bramleylab.ppls.ed.ac.uk/experiments/bnz/magic_stones/index.html)
- [Experiment 2 demo](http://bramleylab.ppls.ed.ac.uk/experiments/bnz/myst/p/welcome.html)
- [Experiment 1 working repository](https://github.com/zhaobn/magic-stones) (contains experiment dev code, all raw data and model dev history; can be messy)
- [Experiment 2 working repository](https://github.com/zhaobn/mysterious-stones) (same as above)
