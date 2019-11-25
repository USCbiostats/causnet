#include <Rcpp.h>
#include <stdio.h>
#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>
using namespace std;
using namespace Rcpp;

//Compute index r for some set T, which is a subset of a set with n elements
//watch out for rounding errors!!!
double getIndex (int m,const std::vector<double> & T)
{
  Rcpp::Environment base("package:causnet");
  Rcpp::Function f = base["subsetr"];
  return as<double>(f(Named("n")=m, Named("set")=T));
}

std::vector<int> getSubsetFromIndex (int n,int r)
{
  Rcpp::Environment base("package:causnet");
  Rcpp::Function f = base["subsetur"];
  vector<int> v = as<std::vector<int>>(f(Named("n")=n, Named("r")=r));
  return v;
}

Rcpp::List getwsinkscores(std::vector<int> w,
                          double w_networkscore,
                          Rcpp::List& pp,
                          Rcpp::List& po,
                          Rcpp::List& pps,
                          Rcpp::List& bps,
                          int m)
{
  Rcpp::Environment base("package:causnet");
  Rcpp::Function f = base["wsink_scores"];
  Rcpp::List v = (f(w, w_networkscore, pp, po, pps, bps, m));
  return v;
}

//return ith column vector of vec of vectors
std::vector<double> getIthColumn(const std::vector<std::vector<double> > & vy, int i)
{
  vector<double> wsc;
  for (int j1 = 0; j1 < vy.size(); j1++)
  {
    wsc.push_back(vy[j1][i]);
  }
  return wsc;
}

std::vector<int> uniq(const std::vector<int> & input)
{
  std::unordered_set<int> set;

  for (const int &i: input) {
    set.insert(i);
  }
  std::vector<int> v(set.begin(), set.end());
  return v;
}

//' Internal function to find the Best Sink
//'
//' @export
//' @param pp list
//' @param ms list
//' @param po list
//' @param pps list
//' @param ppss list
//' @param bps list
//' @param mydata list
// [[Rcpp::export]]
List bestSinksCnew(Rcpp::List  & pp,   Rcpp::List & ms,
                   Rcpp::List & po,  Rcpp::List & pps,
                   Rcpp::List & ppss, Rcpp::List & bps,
                   Rcpp::List & mydata)
{
  int m = pp.size();
  vector<double> windx ;
  vector<double> card ;
  vector<double> sink ;
  vector<double> wscore ;

  vector<vector<double> > sinkstmp;
  //best sinks and scores for subnetworks of one node, which is the node itself and its score
  for (int s = 1 ; s <= m ; s++){
    vector<double> v;
    v.push_back(s);
    double a = getIndex(m,v);
    vector<double> vect{ a, 1, (double)s, ms[s-1] };
    sinkstmp.push_back(vect);
  }

  vector<vector<double> > bsinks;

  for (int k = 2 ; k <= m ; k++){
    vector<double> sink1;
    vector<double> k1; //k also var of the loop
    vector<double> windx1;
    vector<double> wscore1;

    vector<double> sinkstmp1 = getIthColumn(sinkstmp,0);
    vector<double> sinkstmp2 = getIthColumn(sinkstmp,1);
    vector<double> sinkstmp3 = getIthColumn(sinkstmp,2);
    vector<double> sinkstmp4 = getIthColumn(sinkstmp,3);

    for(int j=0; j < sinkstmp.size(); j++)
    {
      vector<int> w = getSubsetFromIndex(m, sinkstmp1[j]);
      double w_networkscore = sinkstmp4[j];
      Rcpp::List w1sinks = getwsinkscores(w, w_networkscore, pp, po, pps, bps, m); //function in aaa

      NumericVector w1sinkstemp = w1sinks["wscore"];
      wscore1.insert( wscore1.end(), w1sinkstemp.begin(), w1sinkstemp.end() );

      NumericVector w1sinkstemp1 = w1sinks["windx"];
      windx1.insert( windx1.end(), w1sinkstemp1.begin(), w1sinkstemp1.end() );

      NumericVector w1sinkstemp2 = w1sinks["k"];
      k1.insert( k1.end(), w1sinkstemp2.begin(), w1sinkstemp2.end() );

      NumericVector w1sinkstemp3 = w1sinks["sink"];
      sink1.insert( sink1.end(), w1sinkstemp3.begin(), w1sinkstemp3.end() );
    }

    DataFrame sinkstmpNew = DataFrame::create(Named("windx")=windx1,
                                              Named("k")=k1,
                                              Named("sink")=sink1,
                                              Named("wscore")=wscore1);

    vector<int> v1 = sinkstmpNew[0]; //sinks.tmp1 == sinkstmpNew here
    vector<int> myws = uniq(v1);

    for(int wind=0; wind < myws.size(); wind++)
    {
      int myw = myws[wind];
      vector<vector<double> > tmp;
      NumericVector sinkstemp11 = sinkstmpNew["windx"];
      for(int wind1=0; wind1 < sinkstemp11.size(); wind1++)
      {
        if (sinkstemp11[wind1] == myw){
          int nCols=sinkstmpNew.size();
          vector <double> y;
          for (int j2=0; j2<nCols;j2++) {
            vector <double> column = sinkstmpNew[j2] ;
            y.push_back(column[wind1]) ;

          }
          tmp.push_back(y);

        }

      }

      vector<double> wsc;
      vector< vector <double>> tmp1;
      for (int j1 = 0; j1 < tmp.size(); j1++)
      {
        wsc.push_back(tmp[j1][3]);
      }

      vector<double>::iterator it;
      it=max_element(wsc.begin(),wsc.end());

      //add this row to bsinks
      for (int j1 = 0; j1 < tmp.size(); j1++)
      {
        if (tmp[j1][3] == *it){
          tmp1.push_back(tmp[j1]);
        }
      }

      for (int j1 = 0; j1 < tmp1.size(); j1++)
      {
        bsinks.push_back(tmp1[j1]);

      }

    }//end wind

    vector< vector <double>> tmpSinksNew;
    for (int j11 = 0; j11 < bsinks.size(); j11++)
    {
      if (bsinks[j11][1] == k){
        tmpSinksNew.push_back(bsinks[j11]);
      }
    }

    sinkstmp = tmpSinksNew;

  }//end k


  vector<double> x1 = getIthColumn(bsinks,0);
  vector<double> x2 = getIthColumn(bsinks,1);
  vector<double> x3 = getIthColumn(bsinks,2);
  vector<double> x4 = getIthColumn(bsinks,3);

  DataFrame bsinks1 = DataFrame::create(Named("windx")=x1,
                                        Named("k")=x2,
                                        Named("sink")=x3,
                                        Named("wscore")=x4);

  return bsinks1;
}
