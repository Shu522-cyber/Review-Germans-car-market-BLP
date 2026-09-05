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
lerner_pre_simple <- markup_pre/ df17m$price
CS_simple_logit_pre <- (1/abs(alpha)) * log(1 + sum(exp(dfc$lhs[dfc$year == 2017]))) * 1000

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
  lerner_simple_merger <- markup_merger / price_in
  
}

CS_simple_logit_merge <- (1/abs(alpha)) * log(1 + sum(exp(df17m$lhs + alpha * (price_in - df17m$price)))) * 1000

# 4-5: Conclude the results into a table
Stellantis_idx <- df17m$firm %in% c("FCA", "PSA")

table_simple_merger <- data.frame(
  
  Variables = c( 
    "Mean price for every products", "Mean price, Stellantis (FCA+PSA) ", "Mean price, outsiders of Stellantis", 
    "Mean markup for every products", "Mean markup, Stellantis (FCA+PSA) ", "Mean markup, outsiders of Stellantis", 
    "lerner index", "Consumer Surplus"
    ),
  
  Ante_merge = c(
    mean(df17m$price)*1000, mean(df17m$price[Stellantis_idx])*1000, mean(df17m$price[!Stellantis_idx])*1000,
    mean(markup_pre)*1000, mean(markup_pre[Stellantis_idx])*1000, mean(markup_pre[!Stellantis_idx])*1000, 
    mean(lerner_pre_simple), CS_simple_logit_pre
  ), 
  
  Post_merge = c(
    mean(price_in)*1000, mean(price_in[Stellantis_idx])*1000, mean(price_in[!Stellantis_idx])*1000, 
    mean(markup_merger)*1000, mean(markup_merger[Stellantis_idx])*1000, mean(markup_merger[!Stellantis_idx])*1000,
    mean(lerner_simple_merger), CS_simple_logit_merge
  ), 
  
  Diff_in_percentage = c(
    ((mean(price_in) - mean(df17m$price)) / mean(df17m$price))*100, ((mean(price_in[Stellantis_idx]) - mean(df17m$price[Stellantis_idx])) / mean(df17m$price[Stellantis_idx]))*100, 
    ((mean(price_in[!Stellantis_idx]) - mean(df17m$price[!Stellantis_idx])) / mean(df17m$price[!Stellantis_idx]))*100, 
    
    ((mean(markup_merger) - mean(markup_pre)) / mean(markup_pre))*100, ((mean(markup_merger[Stellantis_idx]) - mean(markup_pre[Stellantis_idx])) / mean(markup_pre[Stellantis_idx]))*100,
    ((mean(markup_merger[!Stellantis_idx]) - mean(markup_pre[!Stellantis_idx])) / mean(markup_pre[!Stellantis_idx]))*100,
    
    ((mean(lerner_simple_merger) - mean(lerner_pre_simple)) / mean(lerner_pre_simple))*100,
    ((CS_simple_logit_merge - CS_simple_logit_pre) / CS_simple_logit_pre)*100
    
  )
)

kable(table_simple_merger, caption = "[Simple Logit] The evaluation of hypothetical mergering case: Stellantis in 2017")

stargazer(table_simple_merger, 
          type = "text", 
          out = "output/Tables/table_merger_simulation_simple_logit.tex", 
          summary = FALSE, 
          rownames = FALSE)
