# function to loop through offspring to find best sinks
bestSinks = function(pp, ms, po, pps, ppss, bps, mydata){
  m = length(pp)
  nms = c("windx", "k", "sink", "wscore")
  sinks.tmp = as.data.frame(matrix(NA, nrow=0, ncol=length(nms)))
  names(sinks.tmp) = nms

  # best sinks and scores for subnetworks of one node, which is the node itself and its score
  for(s in 1:m){
    sinks.tmp[s, "windx"] = subsetr(m, s)
    sinks.tmp[s, "k"] = 1
    sinks.tmp[s, "sink"] = s
    sinks.tmp[s, "wscore"] = ms[s]
  }
  mysinks = sinks.tmp
  bsinks = sinks.tmp[0, ]

  # best sinks and scores for subnetworks of size 2:m
  for(k in 2:m){

    sinks.tmp1 = list()

    ws <- numeric(nrow(sinks.tmp))
    for(j in 1:nrow(sinks.tmp)){
      ws[j] = length(subsetur(m, sinks.tmp[j, "windx"]))
    }

    total_length <- sum(ws)
    wscore <- windx <- k <- sink <- numeric(m*m) #find better upper bound

    index <- 1
    for(j in seq_len(nrow(sinks.tmp))) {
      w = subsetur(m, sinks.tmp[j, "windx"])
      w.networkscore = sinks.tmp[j, "wscore"]
      w1sinks = wsink.scores(w, w.networkscore, pp, po, pps, bps, m)
      index_subset <- seq_along(w1sinks$wscore)-1+index
      wscore[index_subset] <- w1sinks$wscore
      windx[index_subset] <- w1sinks$windx
      k[index_subset] <- w1sinks$k
      sink[index_subset] <- w1sinks$sink
      index <- index + length(index_subset)
    }
    sinks.tmp1  <- data.frame(wscore = wscore[seq_len(index-1)],
                              windx = windx[seq_len(index-1)],
                              k = k[seq_len(index-1)],
                              sink = sink[seq_len(index-1)])

    # break k loop if there are no more offspring for any sets
    if( nrow(sinks.tmp1) == 0 ) break

    # for each w, find the best sinks
    myws = unique( sinks.tmp1$windx )
    for(wind in 1:length(myws)){
      myw = myws[ wind ]
      tmp = sinks.tmp1[ is.element( sinks.tmp1$windx, myw ), ]
      tmp1 = tmp[ tmp$wscore >= max(tmp$wscore), ]
      bsinks = rbind( bsinks, tmp1 )
    }
    bsinks = unique(bsinks)
    sinks.tmp = bsinks[ is.element( bsinks$k, k ), ]
  }

  return(bsinks)
} # end bestSinks
