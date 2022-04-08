poly_comp <- function(x0, ...) {
  coefs <- unlist(list(...))
  function(x) {
    result = 0
    for (k in length(coefs):2) {
      result <- (x-x0)*(coefs[k] + result)
    }
    return(coefs[1] + result)
  }
}
