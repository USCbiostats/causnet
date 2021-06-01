# function to loop through offsprings to find best sinks
bestSinks = function(pp, ms, po, pps, ppss, bps, mydata, surv){
  print("inside bestSinks")
  m = ncol(mydata) # number of nodes
  if(surv == 1){
    m = m-1
  }
  nms = c("windx", "k", "sink", "wscore") #subset
  sinks.tmp = as.data.frame(matrix(NA, nrow=0, ncol=length(nms)))
  rownames(sinks.tmp) <- NULL
  names(sinks.tmp) = nms

  # best sinks and scores for subnetworks of one node, which is the node itself and its score
  for(s in 1:m){
    sinks.tmp[s, "windx"] = subsetr(m, s)
    sinks.tmp[s, "k"] = 1
    sinks.tmp[s, "sink"] = s
    sinks.tmp[s, "wscore"] = ms[s]
    }

  mysinks = sinks.tmp
  #print(mysinks)
  #print(mysinks,quote = TRUE, row.names = FALSE)
  bsinks = sinks.tmp[0, ] # names row
  #print(bsinks,quote = TRUE, row.names = FALSE)

  # best sinks and scores for subnetworks of size 2:m
  for(q in 2:m){
    sinks.tmp1 = list()
    wscore <- windx <- k <- sink <- numeric(m*m)
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
    # break q loop if there are no more offspring for any sets
    if( nrow(sinks.tmp1) == 0 ) break

    # for each subset w, find the best sinks
    # keep the row/rows with max score
    myws = unique( sinks.tmp1$windx )
    for(wind in 1:length(myws)){
      myw = myws[ wind ]
      tmp = sinks.tmp1[ is.element( sinks.tmp1$windx, myw ), ]
      tmp1 = tmp[ tmp$wscore >= max(tmp$wscore), ]
      bsinks = rbind( bsinks, tmp1 )
    }
    bsinks = unique(bsinks)
    sinks.tmp = bsinks[ is.element( bsinks$k, q ), ]
    # NEW Sep 29 -- remove duplicates in sinks.tmp - sink
    sinks.tmp = sinks.tmp[ , -which(names(sinks.tmp) %in% c("sink"))]
    sinks.tmp = unique(sinks.tmp)
    # NEW Sep 29 --keep only the row/rows with max score
    # for each subset with card q in sinks.tmp
  } # end q for loop

  print(bsinks,quote = TRUE, row.names = FALSE)
  bsinks11 = bsinks
  wsubCol = NULL
  if(nrow(bsinks)>0){
    for(i in 1:nrow(bsinks)){
      if(is.na(bsinks[i,"windx"])) wsubset = NA
      else wsubset <- paste0(subsetur(m,bsinks[i,"windx"]), collapse = ",")
      wsubCol = c(wsubCol,wsubset)
    }

  }

  bsinks["subset"] = wsubCol
  #print(bsinks,quote = TRUE, row.names = FALSE)

  return(bsinks)
} # end bestSinks
