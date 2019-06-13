## An R implementation of Navid Dianati's MLF algorithm
library(igraph)
library(data.table)

# find or generate da standard hairball to prune

pval <- function(mode = "undirected", params) {
  if (mode == "undirected") {
    .pval_undirected(params)
  } else if (mode == "directed") {
    .pval_directed(params)
  } else {
    stop("Mode must be either 'directed' or 'undirected'")
  }
}

.pval_undirected <- function(params) {
  
  w <- params[1]
  ku <- params[2]
  kv <- params[3]
  q <- params[4]
  
  p <- ku * kv * 1 / q / q / 2
  
  binom.test(x = w, n = q, p = p, alternative = "greater")[5] #why does W have to be integer?
}

.pval_directed <- function(params) {
  
  w <- params[1]
  ku_out <- params[2]
  kv_in <- params[3]
  q <- params[4]
  
  p <- ku_out * kv_in * 1 / q / q / 1
  
  binom.test(x = w, n = q, p = p, alternative = "greater")[5]
}


prune <- function(G, df, pct = NULL, num_remove = NULL) {
  if (!is.null(pct)) {
    deathrow <- c()
    n <- ecount(G)
    threshold_index <- n - n * pct / 100
    threshold_value <- sort(E(G)$significance)[threshold_index]
    idx <- E(G)$significance <= threshold_value
    G <- delete.edges(G, which(idx))
    G # fix all this stuff
  } else if (!is.null(num_remove)) {
    n <- ecount(G)
    ind <- sort(E(G)$significance, partial = (num_remove))[num_remove]
    idx <- which(E(G)$significance <= ind)
    G <- delete_edges(G, idx)
    G # fix all this stuff
  } else {
    stop("Something went wrong")
  }
}

compute_sig <- function(G) {
  if (is_directed(G)) {
    .compute_sig_directed(G)
  } else {
    compute_sig_undirected(G)
  }
}

.compute_sig_directed <- function(G) {
  # unimplemented?
}

.compute_sig_undirected <- function(G) {
  G <- rg ##
  ks <- graph.strength(G) # weights used by default
  total_degree <- sum(ks)
  
  all_ends <- as.data.table(ends(G, E(G)))
  strengths <- all_ends[, .(weight = E(G)$weight,
                            s1 = ks[V1],
                            s2 = ks[V2],
                            norm_factor = total_degree / 2)]
  strengths[, I := .I
            ][, pval := pval(params = c(weight, s1, s2, norm_factor)), by = I
              ][, minus_log_p := -log(pval)]
  
  E(G)$significance <- strengths[, minus_log_p]
  
  max_sig <- max(E(G)$significance)
  E(G)$significance[is.null(E(G)$significance)] <- max_sig
  G
}

# generate random graphs to test
rg <- erdos.renyi.game(50, 50 * 3, type = "gnm", directed = F)
rg$layout <- layout.circle
V(rg)$size <- 3
plot(rg)
E(rg)$weight <- sample(seq_len(20), length(E(rg)), replace = T)

test <- compute_sig(rg) #works!
test2 <- prune(test, pct = 30)

