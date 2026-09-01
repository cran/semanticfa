## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## -----------------------------------------------------------------------------
# library(semanticfa)
# 
# fit <- sfa(big5$items, nfactors = 5)
# labels <- sfa_name(fit)
# labels
# #> Factor labels (Qwen/Qwen3-Embedding-0.6B)
# #>
# #>   F1   emotional instability  [emotional instability, neuroticism]
# #>   F2   conscientiousness
# #>   ...

## -----------------------------------------------------------------------------
# fit <- sfa(big5$items, nfactors = 5, label_factors = TRUE)
# fit$labels

## -----------------------------------------------------------------------------
# labels <- sfa_name(fit, model = "microsoft/harrier-oss-v1-27b")

## -----------------------------------------------------------------------------
# # rotate an existing fit
# rot <- sfa_leximax(fit)
# rot$labels
# 
# # or fit and rotate in one call
# fit_lex <- sfa(big5$items, nfactors = 5, rotate = "leximax")

