#--- Session 4: Case Study- FCA-PSA merge case ---#

df17m <-  dfc %>% filter(year == 2017) 

# 4-1: Derivative matrix
derivative_simple <- matrix(0, nrow(df17m), nrow(df17m))

derivative_simple <- -alpha * df17m$share %*% t(df17m$share)

diag(derivative_simple) <- alpha * (1 - df17m$share) * df17m$share

# 4-2: Ownership matrix
omega_m4 <- outer(df17m$firm, df17m$firm, FUN = "==") * 1

# 4-3: Markup
markup_pre <- 0
markup_pre <- c(markup_pre, c(-solve(t(derivative_simple) * omega_m4) %*% df17m$share))
markup_pre <- markup_pre[-1]

mc_pre <- df17m$price - markup_pre

# 4-4: Merge iteration process
df17m_post <- df17m
df17m_post$firm[df17m_post$firm %in% c("PSA", "FCA")] <- "Stellantis"
omega_m4_post <- outer(df17m_post$firm, df17m_post$firm, FUN = "==") * 1

price_in  <- df17m$price
repeat{
  
  #Step 1: new share after merge
  delta_new <- df17m$lhs + alpha * (price_in - df17m$price)
  share_new <- exp(delta_new) /  (1 + sum(exp(delta_new)))
  
  #Step 2: new derivative matrix 
  derivative_merger <- -alpha * outer(share_new, share_new)
  diag(derivative_merger) <- alpha * (1 - share_new) * share_new
  
  #Step 3: renew markup 
  markup_merger <- as.numeric(-solve(t(derivative_merger) * omega_m4_post) %*% share_new)
  
  #Step 4: new price
  price_out <- mc_pre + markup_merger
  
  #Step 5: convergence
  if (max(abs(price_in - price_out)) < 1e-6) break
  
  price_in <- price_out
  
}

CS_simple_logit_merge <- (1/abs(alpha)) * log(1 + sum(exp(df17m$lhs + alpha * (price_in - df17m$price)))) * 1000