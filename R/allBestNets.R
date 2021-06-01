
# Find all best networks
findAllBestNets = function(bsinks, n.var){
  print("inside findAllBestNets")
  m = n.var
  bnets = list()
  r = 1
  ks = unique( bsinks$k )
  ks = ks[order(ks, decreasing=TRUE)]
  q = max(ks)
  d = bsinks[(bsinks$k==q),]
  #print(d,quote = TRUE, row.names = FALSE)

    for (i in 1:nrow(d)) { # loop over rows set 4
      print(i)
      mycomp = 1
      rowno = 1
        nms = c("windx", "k", "sink","subset","wscore", "component") # adding subset
        b1 = as.data.frame(matrix(NA, nrow=0, ncol=length(nms)))
        names(b1) = nms
        # add rows to df b1
        b1[rowno, c("windx", "k", "subset","sink","wscore")] = d[i, c("windx", "k", "subset","sink","wscore")]
        b1[rowno, "component"] = mycomp
        # get lower subet
        myw = subsetur(m, d[i, "windx"]) # just pick 1st row
        w1 = myw[ !is.element(myw, d[i, "sink"]) ] # next set w/o sink
        w1indx = subsetr(m, w1)
        wlen = length(w1)

        # get # rows for k-1 set
        d1 = bsinks[(bsinks$windx==w1indx),]

        for (j in 1:nrow(d1)) { # loop over rows set 3
          print(j)
          rowno1 = 1
          # new copy of previous df
          tmp1 = b1
          # add rows to new tmp df
          tmp1[rowno+rowno1, c("windx", "k", "subset","sink","wscore")] = d1[j, c("windx", "k", "subset","sink","wscore")]
          tmp1[rowno+rowno1, "component"] = mycomp
          # get lower subet
          myw = subsetur(m, d1[j, "windx"]) # just pick 1st row
          w1 = myw[ !is.element(myw, d1[j, "sink"]) ] # next set w/o sink
          w1indx = subsetr(m, w1)
          wlen = length(w1)

          # get # rows for k-1 set
          d2 = bsinks[(bsinks$windx==w1indx),]

          for (l in 1:nrow(d2)) { # loop over rows set 3
            print(l)
            rowno2 =1
            # new copy of previous df
            tmp2 = tmp1
            # # add rows to new tmp df
            tmp2[rowno+rowno1+rowno2, c("windx", "k", "subset","sink","wscore")] = d2[l, c("windx", "k", "subset","sink","wscore")]
            tmp2[rowno+rowno1+rowno2, "component"] = mycomp
            # get lower subet
            myw = subsetur(m, d2[l, "windx"]) # just pick 1st row
            w1 = myw[ !is.element(myw, d2[l, "sink"]) ] # next set w/o sink
            w1indx = subsetr(m, w1)
            wlen = length(w1)

            # get # rows for k-1 set
            rn = rowno+rowno1+rowno2 +1
            if(wlen >1){
              for(n in wlen:2){
                print(n)
                d3 = bsinks[(bsinks$windx==w1indx),]
                tmp2[rn, c("windx", "k","subset", "sink","wscore")] = d3[1, c("windx", "k","subset", "sink","wscore")]
                tmp2[rn, "component"] = mycomp
                w = subsetur(m, d3[1, "windx"])
                w1 = w[ !is.element(w, d3[1, "sink"]) ]
                w1indx = subsetr(m, w1)
                rn = rn + 1
              }
            } # if(wlen >1)
            # add bestnets to global list bnets
            bnets[[r]] = tmp2
            r=r+1

          } #for (i in 1:nrow(d2))


        } #for (i in 1:nrow(d1))

      }#for (i in 1:nrow(d))

    print(bnets)
    return(bnets)

  } # end function

