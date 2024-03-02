calculate_cor_sign <- function(ars, corrcoef) {
  CorSign <- rep("+", nrow(ars))
  for (b in 1:nrow(ars)) {
    CorSign[b] <- ifelse(corrcoef[match(ars[b,1], colnames(corrcoef)), match(ars[b,2], colnames(corrcoef))] > 0, "+", "-")
  }
  return(CorSign)
}
