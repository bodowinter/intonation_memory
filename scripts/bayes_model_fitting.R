## Bodo Winter
## August 8, 2019
## Bayesian models

## This is the only model that does not use relative paths (generic script file, no markdown)
## There were issues with fitting multiple Bayesian models in Markdown scripts

## Load packages:

library(tidyverse)
library(brms)

## Set working directory:

setwd('/Users/winterb/Research/intonation_memory/analysis/data/')

## Load data:

recall <- read_csv('recall_cleaned.csv')

## Set cores for parallel processing:

options(mc.cores = parallel::detectCores())

## Set priors:

my_priors <- c(prior(normal(0, 1), class = 'b'),
               prior(cauchy(0, 2), class = 'sd'))

## Set folder for saving models:

setwd('/Users/winterb/Research/intonation_memory/analysis/models/')

## Main model:

mdl_main <- brm(Correct | trials(15) ~ 1 +
	DigitSpan_c + Pos_c + Pos_c2 +
	  mo(Triplet) + Modality + Condition +
	  mo(Triplet):Condition + Modality:Condition +
	(1 + Pos_c + Pos_c2 + mo(Triplet) +
	   Condition + mo(Triplet):Condition|ID),
		data = recall, family = binomial,
	control = list(adapt_delta = 0.99,
				max_treedepth = 13),
	seed = 42, init = 0,
	warmup = 2000,
	prior = my_priors,
	iter = 4000, chains = 4, save_all_pars = FALSE)
save(mdl_main, file = 'mdl_main.RData')

## No triplet * condition interaction:

mdl_no_triplet_int <- brm(Correct | trials(15) ~ 1 +
                  DigitSpan_c + Pos_c + Pos_c2 +
                  mo(Triplet) + Modality + Condition +
                   Modality:Condition +
                  (1 + Pos_c + Pos_c2 + mo(Triplet) +
                     Condition|ID),
                data = recall, family = binomial,
                control = list(adapt_delta = 0.99,
                               max_treedepth = 13),
                seed = 42, init = 0,
                warmup = 2000,
                prior = my_priors,
                iter = 4000, chains = 4, save_all_pars = FALSE)
save(mdl_no_triplet_int, file = 'mdl_no_triplet_int.RData')

## No modality * condition interaction:

mdl_no_mod_int <- brm(Correct | trials(15) ~ 1 +
                  DigitSpan_c + Pos_c + Pos_c2 +
                  mo(Triplet) + Modality + Condition +
                  mo(Triplet):Condition + 
                  (1 + Pos_c + Pos_c2 + mo(Triplet) +
                     Condition + mo(Triplet):Condition|ID),
                data = recall, family = binomial,
                control = list(adapt_delta = 0.99,
                               max_treedepth = 13),
                seed = 42, init = 0,
                warmup = 2000,
                prior = my_priors,
                iter = 4000, chains = 4, save_all_pars = FALSE)
save(mdl_no_mod_int, file = 'mdl_no_mod_int.RData')

## Model, no interactions whatsoever:

mdl_no_int <- brm(Correct | trials(15) ~ 1 +
                  DigitSpan_c + Pos_c + Pos_c2 +
                  mo(Triplet) + Modality + Condition +
                  (1 + Pos_c + Pos_c2 + mo(Triplet) +
                     Condition|ID),
                data = recall, family = binomial,
                control = list(adapt_delta = 0.99,
                               max_treedepth = 13),
                seed = 42, init = 0,
                warmup = 2000,
                prior = my_priors,
                iter = 4000, chains = 4, save_all_pars = FALSE)
save(mdl_no_int, file = 'mdl_no_int.RData')

## Model, no condition effect:

mdl_no_cond <- brm(Correct | trials(15) ~ 1 +
                    DigitSpan_c + Pos_c + Pos_c2 +
                    mo(Triplet) + Modality + 
                    (1 + Pos_c + Pos_c2 + mo(Triplet) +
                       Condition|ID),
                  data = recall, family = binomial,
                  control = list(adapt_delta = 0.99,
                                 max_treedepth = 13),
                  seed = 42, init = 0,
                  warmup = 2000,
                  prior = my_priors,
                  iter = 4000, chains = 4, save_all_pars = FALSE)
save(mdl_no_cond, file = 'mdl_no_cond.RData')

## Model, no modality whatsoever:

mdl_no_mod <- brm(Correct | trials(15) ~ 1 +
                    DigitSpan_c + Pos_c + Pos_c2 +
                    mo(Triplet) + Condition +
                    (1 + Pos_c + Pos_c2 + mo(Triplet) +
                       Condition|ID),
                  data = recall, family = binomial,
                  control = list(adapt_delta = 0.99,
                                 max_treedepth = 13),
                  seed = 42, init = 0,
                  warmup = 2000,
                  prior = my_priors,
                  iter = 4000, chains = 4, save_all_pars = FALSE)
save(mdl_no_mod, file = 'mdl_no_mod.RData')

## Model, no triplet whatsoever:

mdl_no_triplet <- brm(Correct | trials(15) ~ 1 +
                    DigitSpan_c + Pos_c + Pos_c2 +
                    Modality + Condition +
                    (1 + Pos_c + Pos_c2 + mo(Triplet) +
                       Condition|ID),
                  data = recall, family = binomial,
                  control = list(adapt_delta = 0.99,
                                 max_treedepth = 13),
                  seed = 42, init = 0,
                  warmup = 2000,
                  prior = my_priors,
                  iter = 4000, chains = 4, save_all_pars = FALSE)
save(mdl_no_triplet, file = 'mdl_no_triplet.RData')

# Due to LOO-CV having some observations with pareto_k > 0.7, k-fold cross-validations were perfomed:

kfold_no_triplet <- kfold(mdl_no_triplet, K = 10)
kfold_no_mod_int <- kfold(mdl_no_mod_int, K = 10)
kfold_no_triplet_int <- kfold(mdl_no_triplet_int, K = 10)
kfold_main <- kfold(mdl_main, K = 10)
kfold_no_int <- kfold(mdl_no_int, K = 10)
kfold_no_cond <- kfold(mdl_no_cond, K = 10)
kfold_no_mod <- kfold(mdl_no_mod, K = 10)

# Save these objects:

save(kfold_no_triplet, file = 'kfold_no_triplet.RData')
save(kfold_no_mod_int, file = 'kfold_no_mod_int.RData')
save(kfold_no_triplet_int, file = 'kfold_no_triplet_int.RData')
save(kfold_main, file = 'kfold_main.RData')
save(kfold_no_int, file = 'kfold_no_int.RData')
save(kfold_no_cond, file = 'kfold_no_cond.RData')
save(kfold_no_mod, file = 'kfold_no_mod.RData')
