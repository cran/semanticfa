## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----quickstart---------------------------------------------------------------
library(semanticfa)
data(big5)

fit <- sfa(
  big5$items,
  nfactors    = 5,
  embeddings  = big5$embeddings,
  scoring     = big5$scoring
)
print(fit)

## ----retention----------------------------------------------------------------
fit_auto <- sfa(
  big5$items,
  embeddings = big5$embeddings,
  scoring    = big5$scoring
)
cat("Auto-detected factors:", fit_auto$factors, "\n")

## ----nfactors-----------------------------------------------------------------
sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                      scoring = big5$scoring)
nf <- sfa_nfactors(sim, big5$embeddings,
                   methods = c("parallel", "kaiser"),
                   parallel_iter = 50)
print(nf)

## ----encoding-----------------------------------------------------------------
sim_ar  <- sfa_similarity(big5$embeddings, "atomic_reversed", big5$scoring)
sim_sq  <- sfa_similarity(big5$embeddings, "squid", big5$scoring)
sim_mcp <- sfa_similarity(big5$embeddings, "mean_centered_pearson", big5$scoring)

cat("atomic_reversed range:", range(sim_ar[lower.tri(sim_ar)]), "\n")
cat("squid range:          ", range(sim_sq[lower.tri(sim_sq)]), "\n")
cat("mean_centered_pearson:", range(sim_mcp[lower.tri(sim_mcp)]), "\n")

## ----scree, fig.cap="Scree plot with parallel analysis threshold"-------------
plot(fit, type = "scree")

## ----loadings, fig.cap="Factor loading heatmap"-------------------------------
plot(fit, type = "loadings")

## ----psych-compat, eval=FALSE-------------------------------------------------
# # Run human-data EFA (not run — requires response data)
# human_fit <- psych::fa(response_data, nfactors = 5, rotate = "oblimin")
# 
# # Compare
# psych::factor.congruence(fit$loadings, human_fit$loadings)

## ----congruence---------------------------------------------------------------
cong <- sfa_congruence(fit, big5$factors, metrics = c("nmi", "ari"))
print(cong)

## ----custom, eval=FALSE-------------------------------------------------------
# # With sentence-transformers (requires reticulate + Python).
# # The default model is "Qwen/Qwen3-Embedding-0.6B"; larger models such as
# # "Qwen/Qwen3-Embedding-4B" (8 GB RAM) or "Qwen/Qwen3-Embedding-8B" (16 GB RAM)
# # recover factor structure more accurately.
# emb <- sfa_embed(my_items, embed = "sbert", model = "Qwen/Qwen3-Embedding-0.6B")
# fit <- sfa(my_items, embeddings = emb, scoring = my_scoring)
# 
# # Or bring your own function
# my_embedder <- function(texts) {
#   # ... your embedding logic ...
#   # must return a numeric matrix (n_items x dim)
# }
# fit <- sfa(my_items, embed = my_embedder, scoring = my_scoring)

