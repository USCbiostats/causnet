
# Compute index r for some set T, which is a subset of a set with n elements
# watch out for rounding errors!!!
#' @export
subsetr = function(n, T){
  r = 0L
  for(i in seq_len(n)){
    if( is.element(i, T)) r = r + 2L ^ (n - i)
  }
  return(r)
}

# Find a subset set T of a set with n elements, using index r
# watch out for rounding errors!!!
#' @export
subsetur = function(n, r){
  T = integer()
  for(i in n:1){
    if( (r %% 2) == 1 ) T = c(i,T)
    r = r %/% 2
  }
  return( T )
}
