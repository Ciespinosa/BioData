
aggregate.plurals <- function (v) {
                  aggr_fn <- function(v, singular, plural) {
                  if (! is.na(v[plural])) {
                  v[singular] <- v[singular] + v[plural]
                  v <- v[-which(names(v) == plural)]
                  }
                  return(v)
                  }
                  for (n in names(v)) {
                  n_pl <- paste(n, 's', sep='')
                  v <- aggr_fn(v, n, n_pl)
                  n_pl <- paste(n, 'es', sep='')
                  v <- aggr_fn(v, n, n_pl)
                  }
                  return(v)
}


