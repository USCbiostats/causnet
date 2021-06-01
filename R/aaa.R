# Compute index r for some set T, which is a subset of a set with n elements
# watch out for rounding errors!!!
subsetr = function(n, T){
  r = 0L
  for(i in seq_len(n)){
    if( is.element(i, T)) r = r + 2L ^ (n - i)
  }
  return(r)
}

# Find a subset set T of a set with n elements, using index r
# watch out for rounding errors!!!
subsetur = function(n, r){
  #print("r=====")
  #print(r)
  T = NULL
  for(i in n:1){
    if( r %% 2 == 1 ) T = c(i,T)
    r = r %/% 2
  }
  return( T )
}


# function to find possible parents

#' @importFrom stats cor.test
mypp = function(mydata, alpha, n.var){
  pp = vector('list', n.var)
  for(v in 1:n.var){
    for(p in 1:n.var){
      if(p != v){
        #if(p == n.var){}else{ # last node phenotype outout- not a parent
          p.value = cor.test(mydata[, v], mydata[, p])$p.value
          #p.value = cor.test(mydata[, v], mydata[, p])$estimate
          if(p.value < alpha) pp[[v]] = c(pp[[v]], p)
       # }
      }
    }
  }
  return(pp)
} # end mypp

#phenotype driven pp non-surv
mypp1 = function(mydata, alpha1, alpha2, n.var, y){
  pp = vector('list', n.var)
  nSubset1 = NULL
  nSubset = y
    for(p in 1:n.var){
      if(p != y){
        #p.value = cor.test(mydata[, y], mydata[, p])$p.value
        p.value = cor(mydata[, y], mydata[, p])

        #if(p.value < alpha1){
          if(p.value > alpha1){
          pp[[y]] = c(pp[[y]], p)
          nSubset1 = c(nSubset1,p)
        }
      }
    }
  #print("nSubset1")
  #print(nSubset1)
  for(q in 1:(n.var-1)){# not including outcome variable
    for(r in nSubset1){
      if(q != r){
        #p.value = cor.test(mydata[, q], mydata[, r])$p.value
        p.value = cor(mydata[, r], mydata[, q])
        #if(p.value < alpha2){
        if(p.value > alpha2){
          pp[[r]] = c(pp[[r]], q)

        }
      }
    }
  }
  return(pp)
} # end mypp

#phenotype driven pp surv
mypp_surv = function(mydata, genes, alpha1, alpha2){
  n.var = ncol(mydata)
  n.var = n.var -1 # last col survival time
  pval = NULL
  i=0

  for (g in genes){
    i=i+1
    #print(i)
    f = coxph(Surv(as.numeric(unlist(mydata["Time"])),
                   as.numeric(unlist((mydata["Status"]))))
              ~  as.matrix(mydata["Stage"])
              + as.matrix(mydata["Age"]) +as.matrix(mydata[g])
              + strata(mydata["Site"]),
              mydata)
    an = anova(f)
    pval = c(pval,an$`Pr(>|Chi|)`[4])
  }
  pval1=p.adjust(pval, "BH")
  ppDis = NULL
  i=0
  for (k in pval1){ # fixed pval1
    i = i+1
    if (k < alpha1) ppDis = c(ppDis,i) # 0.0001 gives 195 genes, all top 5 in
    # 1e-20,22 --18,14(3 top5), 1e-23- 11, 1e-25 -7(1 in top5 ZFHX4)
    # 1e-24 -5(1 in top5 ZFHX4), 1e-30 -2(0 in top5)
  }
  sGenes = genes[ppDis]
  parentsDis = ppDis # col numbers in genes

  mydata1 = mydata[genes]
  mydata=cbind(mydata1,surdata)
  cnames = colnames(mydata)
  n.var = ncol(mydata)
  surv = 0
  if(!is.null(surdata)){
    n.var = n.var -1 # last col survival time
    surv = 1
  }

  pp = vector('list', n.var)
  nSubset1 = parentsDis
  y = n.var
  pp[[y]] = nSubset1
  alpha2 = 0.85 # 0.85 -- 9,3,7 parents, 5 of dis
  lengs = NULL
  nSubset2 = NULL
  for(r in nSubset1){
    for(q in 1:(n.var-1)){# not including outcome variable
      if(q != r){
        #p.value = cor.test(mydata[, q], mydata[, r])$p.value
        p.value = cor(mydata[, r], mydata[, q])

        if(p.value > alpha2){
          pp[[r]] = c(pp[[r]], q)
          nSubset2 = c(nSubset2,q)
        }
      }

    }
    if(!is.null(pp[[r]])){
      #print(r)
      lengs = c(lengs,length(pp[[r]]))
    }
  }
  nSubset3 = NULL
  for(r1 in nSubset2){
    for(q1 in 1:(n.var-1)){# not including outcome variable
      if(q1 != r1 && !(q1 %in% nSubset1)){
        #p.value = cor.test(mydata[, q], mydata[, r])$p.value
        p.value = cor(mydata[, r1], mydata[, q1])

        if(p.value > alpha2){
          pp[[r1]] = c(pp[[r1]], q1)
          nSubset3 = c(nSubset3,q1)
        }
      }

    }
    if(!is.null(pp[[r1]])){
      #print(r1)
      lengs = c(lengs,length(pp[[r1]]))
    }
  }


  return( pp )

}

# function to reformat a list of possible parents, pp, into a list of possible offspring, po
pofun = function(pp){
  po = vector('list', length(pp))
  for(i in 1:length(pp)){
    for(j in 1:length(pp)){
      if( is.element(i, pp[[ j ]]) ) po[[i]] = c( po[[i]], j )
    }
  }
  return( po )
}

# create list object with all combinations of vec

#' @importFrom utils combn
comb1 = function(vec, maxSubsetSize) {
  n = length(vec)
  out = vector('list', min( maxSubsetSize, n))
  for (j in 1:min( maxSubsetSize, n)) { # only upto 2 elements

    if( length(vec) > 1 ) out[[ j ]] = combn(vec, j) else out[[ j ]] = matrix(vec, nrow=1, ncol=1)
  }
  return(out)
}

## create all sets of possible parents for each node
pp.sets = function(pp){
  n = length(pp)
  pps = vector('list', n)
  for(j in 1:n){
    if (!is.null(pp[[ j ]]))
    pps[[ j ]] = comb1( pp[[ j ]],n )
  }
  return(pps)
}

# Bigger is better score for node y and parents x

#' @importFrom stats BIC lm
score.bic.lm = function(y, x, mydat) {
  y.nm = colnames(mydat)[ y ]
  if( is.element(x[1], 1:ncol(mydat)) ) x.nms = colnames(mydat)[ x ] else x.nms = "1"
  fit = lm(paste0(y.nm, ' ~ ', paste(x.nms, collapse=' + ')), data = mydat)
  bic = -(1/2)*BIC(fit)
  return(bic)
}


#' @importFrom stats BIC survival
score.bic.surv = function(y, x, mydat) {
  n = ncol(mydat)
  y.nm = colnames(mydat)[ y ]
  delta = colnames(mydat)[ n ]
  X = NULL
  if(!is.na(x) ) {
    #print("*****Notnull SURV*********")
    x.nms = colnames(mydat)[ x ]
    #print("x.nms===")
    #print(x.nms)
    for(v in x.nms){
      X = c(X,v)
    }

    l = as.matrix(mydat[X])
    #l = as.matrix(mydat[x])
    #print(l)
    #print(dim(l))

    fit = coxph(Surv(as.numeric(unlist(mydat[unlist(y)])), unlist(mydat[delta])) ~ l)

  }

  else{
    x.nms = "1"
    #print("*****nullSurv*********")
    fit = coxph(Surv(as.numeric(unlist(mydat[unlist(y)])), unlist(mydat[delta])) ~ 1)
      }

  #print("*****BICsurv*********")
  bic = -(1/2)*BIC(fit)
  #print(bic)
  return(bic)
}

## create object with scores for all sets of possible parents for each node
pp.sets.s = function(mydata, pps, surv, score){
  n = length(pps)
  ppss = vector('list', n) # possible parent set scores
  ppss1 = vector('list', n) # possible parent best sets
  ppss2 = vector('list', n) # possible parent best set scores
  for(v in 1:n){
    n.pp = ncol(pps[[v]][[1]])
    #print("%%%%%%%% n.pp=")
    #print(n.pp)
    if (!is.null(n.pp)){
      #print("22222222")
    for(set.size in 1:n.pp){
      ppss[[v]][[set.size]] = rep(NA, ncol(pps[[v]][[set.size]]) )
      ppss1[[v]][[set.size]] = rep(NA, ncol(pps[[v]][[set.size]]) )
      ppss2[[v]][[set.size]] = rep(NA, ncol(pps[[v]][[set.size]]) )
      for(pset.i in 1:ncol(pps[[v]][[set.size]])){
        v.pset = pps[[v]][[set.size]][ , pset.i ]
        if(v==n & surv ==1){
            ppss[[v]][[set.size]][ pset.i ] = score.bic.surv( v, v.pset, mydata )
            }
          else{

            if(score=="bic"){
              #print(v)
              #print("BIC")
              ppss[[v]][[set.size]][ pset.i ] = score.bic.lm( v, v.pset, mydata )
              }
            else{
              #print(v)
              #print("Bge")
              ppss[[v]][[set.size]][ pset.i ] = score_bge( v, v.pset, mydata )

            }

          }

        }
    }
    }
  }
  return(ppss)
}

# compute scores for nodes with no parents
mscores = function(vset, mydata, surv = 0, score){
  n = ncol(mydata)
  mscore = NULL
  for(v in vset){
    if(v==n-1 & surv ==1){
      #print("v====")
      #print(v)
      mscore = c(mscore, score.bic.surv( v, NA, mydata ))
    }
    else{
      #print("v====")

      if(score=="bic"){
        #print(v)
        #print("BIC")
        mscore = c(mscore, score.bic.lm( v, NA, mydata ))
      }
      else{
        #print(v)
        #print("Bge")
        mscore = c(mscore, score_bge( v, NA, mydata ))

      }

    }
  }
  return(mscore)
}

# get a score, given a node, its parent set, and parent set scores
# ms = scores of nodes w/ no parents
get.score = function(v, pset, pps, ppss, ms){
  if(is.null(pset)){ # FIX check for null NOT length<1
    myscore = ms[v]
  } else {
    l = length(pset)
    aa = apply(pps[[v]][[l]],2,setequal,y=pset)
    myscore = ppss[[v]][[l]][ aa ]
  }
  return(myscore)
} # end get.score

# function to find best parent set of v, for parent set pset, given pps, ppss, ms
bestps = function(v, pset, pps, ppss, ms, max_parents, surv){
  # all subsets of pset
  best.score = get.score(v, NULL, pps, ppss, ms)
  best.set = NULL
  # print("pset===")
  # print(pset)
  sb = comb1( pset, max_parents ) # max_parents -maxSubsetSize
  # print(sb)
  # print("length(sb)====")
  # print(length(sb))
  for(j in 1:length(sb)){
    for(q in 1:ncol(sb[[j]])){
      tmp.set = sb[[j]][, q]
      tmp.score = get.score(v, tmp.set, pps, ppss, ms)
      if(surv == 1){
        if(q == 1) {
          best.score = tmp.score
          best.set = tmp.set
        }
      } # FIX against disconnected being best for survival outcome
      # print("tmp.score====")
      # print(tmp.score)
      if( tmp.score > best.score ){
        best.set = tmp.set
        best.score  = tmp.score
      }
    }
  }
  outp = vector('list', 2)
  # print("best.set====")
  # print(best.set)
  outp[[1]] = best.set
  outp[[2]] = best.score
  return(outp)
} # end function bestps

# Get BEST parent sets and scores for all sets of possible parents for each node
pp.sets.bs = function(pps, ppss, ms, max_parents, surv){
  bps = ppss # best parent sets
  bpss = ppss # best parent set scores
  for(v in 1:length(pps)){ # length(pps) - no. of vars
    # for each possible parent set, compare score w/ all subsets to identify
    # the best possible parent sets
    n.pp = ncol(pps[[v]][[1]]) #n.pp number of pp for v
    #print("n.pp")
    # print(n.pp)
    if (!is.null(n.pp)){ # for each v, n.pp number of pp .. for each of
      # pps for v, need to find all subsets, find their score and then choose
      # the best score and best parents for that pps
      for(set.size in 1:n.pp){ # n.pp number of pp
      #print("#######")
      #for(set.size in 1:min(n.pp,max_parents)){ # max parents NOT here
        n.sets = ncol(pps[[v]][[set.size]])
        # n.sets- no of sets of size n.sets
        #print(n.sets)
        if (!is.null(n.sets)){
          bps[[v]][[set.size]] = vector('list', n.sets)
          for(k in 1:n.sets){
              tmp.set = pps[[v]][[set.size]][, k]
              # tmp.set one set of size set.size at a time

              #print(tmp.set)
              tmp = bestps(v, tmp.set, pps, ppss, ms, max_parents,surv)
              # print("tmp")
              #print(tmp)
              bps[[v]][[set.size]][[k]] = tmp[[1]]
              bpss[[v]][[set.size]][k] = tmp[[2]]
            }
          }
        }
      }
  }
  outp = vector('list', 2)
  outp[[ 1 ]] = bps
  outp[[ 2 ]] = bpss
  return( outp )
} # end function pp.sets.bs

# given a node s, and a set w, compute score for s with its best parent set in w
# argument bps is generated by pp.sets.bs
swscore = function(s, w, pp, pps, bps){
  pset = w[ is.element( w, pp[[s]] ) ] # find possible parents of s in w
  l = length(pset)
  #print("pps[[s]][[l]]===" )
  #print(pps[[s]][[l]] )
    aa = apply(pps[[s]][[l]],2,setequal,y=pset)
    best.ps = bps[[1]][[s]][[l]][aa]
    #print("best.ps===" )
    #print(best.ps )
    best.pss = bps[[2]][[s]][[l]][aa]
    #print("best.pss===" )
    #print(best.pss )
  #}
  outp = vector('list', 2)
  outp[[ 1 ]] = best.ps
  outp[[ 2 ]] = best.pss
  return(outp)
} # end swscore

# Given a set w of nodes, compute network scores for all possible
# sinks of sets, w1 = {w union v},
# where v is a possible offspring of at least one of the nodes in w.
# W.networkscore is the score of the best network for w.
wsink.scores = function(w, w.networkscore, pp, po, pps, bps, m){

  # Find possible offspring for w not already in w
  wpo = NULL
  for(v in w){
    wpo = unique( c(wpo, po[[v]][ !is.element( po[[v]], w ) ]) )
  }

  if( length(wpo) > 0 ){
    windx <- k <- sink <- wscore <- numeric(length(wpo))
    # Expand w by one po node, a possible sink, and compute score
    rowno = 1
    for(s in wpo){
      s.score = swscore(s, w, pp, pps, bps)[[2]]
      # print("s.score==")
      # print(s.score)
      # print("w.networkscore==")
      # print(w.networkscore)
      wscore[rowno] = s.score + w.networkscore
      windx[rowno] = subsetr(m, c(w, s))
      k[rowno] = length(w) + 1
      sink[rowno] = s
      rowno = rowno + 1
    }
  } else {
    w1sinks = NULL    # end if length(wpo)
    return( w1sinks )
  }

  w1sinks <- list(wscore = wscore,
                  windx = windx,
                  k = k,
                  sink = sink)
  return( w1sinks )
} # end wsink.scores


bestnet = function(bsinks, m){
  nms = c("windx", "k", "sink","subset","wscore", "component") # adding subset
  bestnets = as.data.frame(matrix(NA, nrow=0, ncol=length(nms)))
  names(bestnets) = nms

  ## Order best sinks, removing a sink at each step
  # Loop over connected components
  mycomp = 1
  rowno = 1
  while(nrow(bsinks) > 0){

    ks = unique( bsinks$k )
    ks = ks[order(ks, decreasing=TRUE)]
    q = max(ks) ###### change index k to q DONE
    aa = match(q, bsinks$k) ########!!!!!!! match gets only one row
    ######## aa gets only one top row which may not have max score
    tmp.s = bsinks[ aa, ]
    bestnets[rowno, c("windx", "k", "subset","sink","wscore")] = tmp.s[1, c("windx", "k", "subset","sink","wscore")]
    bestnets[rowno, "component"] = mycomp
    # print("tmp.s[1, windx]")
    # print(tmp.s[1, "windx"])
    myw = subsetur(m, tmp.s[1, "windx"]) # just pick 1st row
    w1 = myw[ !is.element(myw, tmp.s[1, "sink"]) ] # pick sink in the top row
    w1indx = subsetr(m, w1)

    rowno = rowno + 1
    wlen = length(w1)
    #print("wlen===")
    #print(wlen)
    if(wlen >1){
      for(d in wlen:2){
        aa = match(w1indx, bsinks$windx)
        tmp.s = bsinks[ aa, ]
        ######## aa gets only one top row which may not have max score
        ######## so inside this loop you just get the top rows for each subset
        bestnets[rowno, c("windx", "k","subset", "sink","wscore")] = tmp.s[1, c("windx", "k","subset", "sink","wscore")]
        bestnets[rowno, "component"] = mycomp
        w = subsetur(m, tmp.s[1, "windx"])
        w1 = w[ !is.element(w, tmp.s[1, "sink"]) ]
        w1indx = subsetr(m, w1)
        rowno = rowno + 1
      }
    }


    # remove all rows in bsinks with sets that have elements of the largest w in bestnets
    aa = NULL
    for(j in 1:nrow(bsinks)){
      wj = subsetur(m, bsinks[j, "windx"])
      aa = c(aa, sum( is.element(wj, myw) ) == 0)
    }
    bsinks = bsinks[aa, ]
    break
  } # end while loop

  return(bestnets)
} # end bestnet

# get edges of best network connected components from ordered sinks
sink2net = function(bnets, pp, pps, bps){
  m = length(bps[[1]])
  nms = c("node.source", "node.sink", "component")
  mynets = as.data.frame(matrix(NA, nrow=0, ncol=length(nms)))
  names(mynets) = nms
  n.comp = length(unique(bnets$component))
  rowno = 1
  for(c in 1:n.comp){
    tmpn = bnets[ is.element(bnets$component, c), ]
    for(q in max(tmpn$k):2){ # REPLCED k by q
      bp.set = NULL
      tmp = tmpn[ is.element(tmpn$k, q), ]
      s = tmp[ 1, "sink"]
      w = subsetur(m, tmp[ 1, "windx"])
      bp.set1 = swscore(s, w, pp, pps, bps) ## BUG FIX [[1]]
      bp.set = swscore(s, w, pp, pps, bps)[[1]] # ??????? OK???
      if(!is.null(bp.set[[1]])){
        src = bp.set[[1]]
        #print(length(bp.set[[1]]))
        snk = rep(s, length(bp.set[[1]]))
        cmp = rep(c, length(bp.set[[1]]))
        mynets[ c(rowno:(rowno+length(bp.set[[1]])-1)),] = cbind(src, snk, cmp)

      }
      rowno = rowno+length(bp.set[[1]])
    }


  }
  return(mynets)
} # end sink2net

#' BGE scoring function
#'
#' @param y y
#' @param x x
#' @param mydat data
#'
#' @return numeric score
#' @export
score_bge <- function(y, x, mydat) {
  #print("score bge")
  names <- colnames(mydat)

  if (any(is.na(x))) {
    model <- paste0("[",
                    names[y],
                    "]")
    model.net <- bnlearn::model2network(model)
    model_data <- mydat[y]
    bnlearn::modelstring(model.net) <- model
    res <- bnlearn::score(model.net, model_data, type = "bge")

  } else {
    names1 = names[y]
    names2 = names[x]
    names3 <- c(names1,names2)
    namesLeft = names[!(names %in% names3)]
    namesUse = c(namesLeft,names2)
    model <- paste0(paste0("[", paste0(names[x], collapse = "]["), "]"),
                    "[",
                    names[y],
                    "|",
                    paste0(names[x], collapse = ":"),
                    "]")

    model.net <- bnlearn::model2network(model)
    model_data <- mydat[c(y, x)]
    bnlearn::modelstring(model.net) <- model

    model1 <- paste0("[", paste0(names[x], collapse = "]["), "]")
    #print(model1)
    model.net1 <- bnlearn::model2network(model1)
    model_data1 <- mydat[names[x]]
    #print(model.net1)

    bnlearn::modelstring(model.net1) <- model1
    bnlearn::modelstring(model.net) <- model
    #print(model)
    networkScore <- bnlearn::score(model.net, model_data, type = "bge")
    sourceScore <- bnlearn::score(model.net1, model_data1, type = "bge")
    # print(networkScore)
    # print(sourceScore)
    res = networkScore - sourceScore
  }
  #print(res)
  res
}



