## An R implementation of Navid Dianati's MLF algorithm
library(igraph)

# find or generate da standard hairball to prune

pval = function(mode = "undirected", params){
  if(mode == "undirected"){
    return(pval_undirected(params))
  }else if (mode == "directed"){
    return(pval_directed(params))
  } else{
    stop("Mode must be either 'directed' or 'undirected'")
  }
}

pval_undirected = function(params){
  
  w = params[1]
  ku = params[2]
  kv = params[3]
  q = params[4]
  
  p = ku * kv * 1 / q / q / 2
  
  return(binom.test(x = w, n = q, p=p, alternative = "greater")[5]) #why does W have to be integer?
}

pval_directed = function(params){
  
  w = params[1]
  ku_out = params[2]
  kv_in = params[3]
  q = params[4]
  
  p = ku_out * kv_in * 1 / q / q / 1
  
  return(binom.test(x = w, n = q, p=p, alternative = "greater")[5])
}


prune = function(G, df, pct = NULL, num_remove=NULL){
  if(!is.null(pct)){
    deathrow = c()
    n = ecount(G)
    threshold_index = n - n*pct/100
    threshold_value = sort(E(G)$significance)[threshold_index]
    idx = E(G)$significance <= threshold_value
    G = delete.edges(G, which(idx))
    return(G)# fix all this stuff
  } else if(!is.null(num_remove)){
    n = ecount(G)
    ind = sort(E(G)$significance,partial=(num_remove))[num_remove]
    idx = which(E(G)$significance <= ind)
    G = delete_edges(G, idx)
    return(G)# fix all this stuff
  } else {
    stop("Something went wrong")
  }
}

compute_sig = function(G){
  if(is_directed(G)){
    return(compute_sig_directed(G))
  } else{
    return(compute_sig_dunirected(G))
  }
}

compute_sig_directed = function(G){
  # unimplemented?
}


compute_sig_undirected = function(G){
  ks = graph.strength(G) # weights used by default
  total_degree = sum(ks)
  sig = c()
  for(e in 1:ecount(G)){
    i0 = ends(G, E(G)[e])[1] # fix this: e should be the Eth edge
    i1 = ends(G, E(G)[e])[2] # fix this: e should be the Eth edge
    v0 = V(G)[i0]
    v1 = V(G)[i1]
    
    p = pval(params=c(E(G)$weight[e], ks[i0], ks[i1], total_degree/2)) #resume testing here
    sig[e] = -log(p$estimate)
  }
  E(G)$significance =  sig
  max_sig = max(E(G)$significance)
  for(e in 1:ecount(G)){
    if(is.na(E(G)$significance[e])){ # is na the right test here?
       E(G)$significance[e] = max_sig
    }
  }
  return(G)
}

# generate random graphs to test
rg <- erdos.renyi.game(50, 50*3, type="gnm", directed=F)
rg$layout <- layout.circle
V(rg)$size <- 3
plot(rg)
E(rg)$weight = sample(1:20, length(E(rg)), replace=T)

test = compute_sig_undirected(rg) #works!
test2 = prune(test, pct = 30)

