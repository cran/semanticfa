# Lexical target rotation and nameability. No encoder or pool download here:
# the lexmap is assembled from deterministic synthetic vectors with the same
# structure sfa_lexmap() produces (S = pool x items, Q = items x items,
# q_unit = items x dim, pool$words = data.frame(word, family, tier1)).

set.seed(42)
.dim <- 16L
.npool <- 300L
.nitem <- 20L
.k <- 3L

.unit <- function(m) m / pmax(sqrt(rowSums(m^2)), 1e-12)

.fake_lexmap <- function(n_items = .nitem, dim = .dim, npool = .npool) {
  q <- .unit(matrix(stats::rnorm(n_items * dim), n_items))
  pe <- .unit(matrix(stats::rnorm(npool * dim), npool))
  words <- data.frame(
    word = paste0("w", seq_len(npool)),
    family = paste0("f", ceiling(seq_len(npool) / 3)),
    tier1 = rep(c(TRUE, FALSE), length.out = npool),
    stringsAsFactors = FALSE)
  structure(list(S = pe %*% t(q), Q = q %*% t(q), q_unit = q,
                 n_items = n_items,
                 pool = list(words = words, emb = pe, dim = dim,
                             model = "synthetic", precision = "fp16"),
                 model = "synthetic", instruction = "synthetic",
                 items = paste0("item", seq_len(n_items)),
                 block_size = 50000L),
            class = "sfa_lexmap")
}

.fake_loadings <- function(n_items = .nitem, k = .k) {
  L <- matrix(stats::rnorm(n_items * k, sd = 0.15), n_items, k)
  blk <- rep(seq_len(k), length.out = n_items)
  for (j in seq_len(k)) L[blk == j, j] <- L[blk == j, j] + 0.75
  colnames(L) <- paste0("MR", seq_len(k))
  L
}

test_that("sfa_leximax returns a valid oblique solution", {
  lm <- .fake_lexmap(); L <- .fake_loadings()
  o <- suppressWarnings(sfa_leximax(x = L, Phi = diag(.k), lexmap = lm,
                                    n_random = 2L, max_iter = 5L, seed = 42L))

  expect_s3_class(o, "sfa_leximax")
  expect_equal(dim(o$loadings), c(.nitem, .k))
  # Phi must be a legitimate factor-correlation matrix
  expect_true(isSymmetric(unname(o$Phi), tol = 1e-8))
  expect_lt(max(abs(diag(o$Phi) - 1)), 1e-8)
  expect_gt(min(eigen(o$Phi, only.values = TRUE)$values), -1e-8)
  expect_true(all(is.finite(o$loadings)))
})

test_that("leximax rotation preserves communalities", {
  lm <- .fake_lexmap(); L <- .fake_loadings()
  o <- suppressWarnings(sfa_leximax(x = L, Phi = diag(.k), lexmap = lm,
                                    n_random = 2L, max_iter = 5L, seed = 7L))
  # a rotation redistributes variance across factors but cannot change how
  # much of each item's variance the common factors explain
  expect_equal(rowSums((o$loadings %*% o$Phi) * o$loadings),
               rowSums(L^2), tolerance = 1e-6)
})

test_that("leximax is deterministic under a fixed seed", {
  lm <- .fake_lexmap(); L <- .fake_loadings()
  a <- suppressWarnings(sfa_leximax(x = L, Phi = diag(.k), lexmap = lm,
                                    n_random = 2L, max_iter = 5L, seed = 99L))
  b <- suppressWarnings(sfa_leximax(x = L, Phi = diag(.k), lexmap = lm,
                                    n_random = 2L, max_iter = 5L, seed = 99L))
  expect_equal(a$loadings, b$loadings)
  expect_equal(a$Phi, b$Phi)
})

test_that("orthogonal rotation leaves factors uncorrelated", {
  lm <- .fake_lexmap(); L <- .fake_loadings()
  o <- suppressWarnings(sfa_leximax(x = L, Phi = diag(.k), lexmap = lm,
                                    n_random = 2L, max_iter = 5L, seed = 3L,
                                    rotation = "orthogonal"))
  expect_lt(max(abs(o$Phi[lower.tri(o$Phi)])), 1e-8)
})

test_that("leximax rejects a single-factor solution", {
  lm <- .fake_lexmap(); L <- .fake_loadings()
  expect_error(
    sfa_leximax(x = L[, 1, drop = FALSE], Phi = diag(1), lexmap = lm),
    "at least 2 factors")
})

test_that("sfa_nameability labels every factor from the pool", {
  lm <- .fake_lexmap(); L <- .fake_loadings()
  nm <- suppressWarnings(sfa_nameability(lm, L, n_candidates = 3L))

  expect_named(nm, c("labels", "candidates", "criterion", "per_factor",
                     "coefs"))
  expect_s3_class(nm$labels, "data.frame")
  expect_equal(nrow(nm$labels), .k)
  expect_true(all(c("factor", "label", "rule") %in% names(nm$labels)))
  # every label must be a real pool word, not a factor id
  expect_true(all(nm$labels$label %in% lm$pool$words$word))
  expect_length(nm$per_factor, .k)
  expect_true(all(is.finite(nm$criterion)))
})

test_that("nameability surprise scoring accepts a baseline", {
  lm <- .fake_lexmap(); L <- .fake_loadings()
  base <- rowMeans(lm$S)
  plain <- suppressWarnings(sfa_nameability(lm, L, n_candidates = 3L))
  surp <- suppressWarnings(sfa_nameability(lm, L, n_candidates = 3L,
                                           baseline = base))
  expect_equal(nrow(surp$labels), .k)
  expect_true(all(surp$labels$label %in% lm$pool$words$word))
  # centering changes the score scale, so criteria need not match
  expect_true(is.finite(surp$criterion))
  # and with a standard deviation supplied as well
  sds <- pmax(apply(lm$S, 1, stats::sd), 1e-8)
  z <- suppressWarnings(sfa_nameability(lm, L, n_candidates = 3L,
                                        baseline = base, baseline_sd = sds))
  expect_true(is.finite(z$criterion))
  expect_equal(nrow(z$labels), .k)
  expect_false(identical(plain$per_factor, z$per_factor))
})

test_that("sfa_naming_instruction returns a single usable string", {
  s <- sfa_naming_instruction()
  expect_type(s, "character")
  expect_length(s, 1L)
  expect_gt(nchar(s), 0L)
})

test_that("sfa_pool refuses to download implicitly when non-interactive", {
  # download defaults to interactive(); under R CMD check that is FALSE, so
  # an uncached pool must error with instructions rather than fetch silently
  skip_if(interactive())
  # the message names the explicit opt-in (download = TRUE for a hosted pool,
  # build = TRUE for a model without one); either way nothing is fetched
  expect_error(
    sfa_pool("definitely/not-a-real-model", download = FALSE, build = FALSE),
    "download = TRUE|build = TRUE")
})
