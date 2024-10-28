martingale_wager <- function(
    previous_wager
    , previous_outcome
    , max_wager
    , current_budget
){
  if(previous_outcome == 1) return(1)
  min(2*previous_wager, max_wager, current_budget)
}


profit <- function(l1) l1[nrow(l1),4] - l1[1,1]

one_series <- function(ng, sb, wt, mw){
  # ng = max number of spins
  # sb = starting budget
  # wt = winning threshold
  # mw = max wager
  l1 <- matrix(NA_integer_,nrow=ng,ncol=4)
  l1[,3] <- rbinom(ng,1,18/38)
  l1[1,1] <- sb
  l1[1,2] <- 1
  l1[1,4] <- ifelse(l1[1,3]==1, sb + 1, sb - 1)
  
  for(i in 2:ng){
    w <- martingale_wager(l1[i-1,2], l1[i-1,3], mw, l1[i-1,4])
    e <- ifelse(l1[i,3]==1, l1[i-1,4] + w, l1[i-1,4] - w)
    s <- e <= 0 | e >= wt
    l1[i,c(1,2,4)] <- c(l1[i-1,4], w, e)
    if(s) break
  }
  l1[1:i,]
}
