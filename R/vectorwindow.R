
vector_window <- function(parent, start, len) {
  ## no sneaky integer vectors allowed
  stopifnot(mode(parent) == "numeric")
  start = as.numeric(start)
  len = as.numeric(len)
  start_len = c(start - 1, # C is 0 indexed
                len)
  .Call("make_window_real", 
          parent, 
        start_len,
        PACKAGE = "vectorwindow")
}