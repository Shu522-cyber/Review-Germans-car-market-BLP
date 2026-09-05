# 07: Nested merge case for PSA-FCA

# 7-1: Pre-requisition setups
# Same-nest matrix（for derivative）
same_nest_pre <- outer(df17$class, df17$class, FUN = "==") * 1

# Ownership matrix（for markup）
own_nest_pre <- outer(df17$firm, df17$firm, FUN = "==") * 1

# Derivative matrix
n17 <- nrow(df17)
derivative_nest_pre <- matrix(0, n17, n17)

for (j in 1:n17){
  
  for (k in 1:n17){
    
    if (j == k){
      
      derivative_nest_pre[j,j] <- (alpha_nl / (1 - sigma_nl)) * 
        (1 - sigma_nl * df17$group_share[j] - (1 - sigma_nl) * df17$share[j]) * df17$share[j]
      
    } else if (df17$class[j] == df17$class[k]){
      
      derivative_nest_pre[j,k] <- -alpha_nl * (sigma_nl/(1-sigma_nl) * df17$group_share[k] + df17$share[k]) * df17$share[j]
      
    } else {
      
      derivative_nest_pre[j,k] <- -alpha_nl * df17$share[j] * df17$share[k]
      
    }
  }
}

# Markup (Generate variables: mc_pre)
markup_nest_pre17 <- as.numeric(-solve(derivative_nest_pre * own_nest_pre) %*% df17$share)
mc_nest_pre17 <- df17$price - markup_nest_pre17
lerner_nest_pre <- markup_nest_pre17 / df17$price

# 7-2: Consumer Surplus
delta_nl <- df17$lhs - sigma_nl * df17$lwgshare
exp_nl_preM <- exp(delta_nl / (1 - sigma_nl))
nest_sum_preM <- tapply(exp_nl_preM, df17$class, sum)

eta_nest_preM <- (1 - sigma_nl) * log(nest_sum_preM)

CS_pre_nest <- (1 / abs(alpha_nl)) * log(1 + sum(exp(eta_nest_preM))) * 1000

# 7-3: Merge, establishing the function for processes

simulate_M_nl <- function(df17, mc_pre, alpha_nl, sigma_nl, delta_nl_17, lambda = 0){
  
  prices_in_nest <- df17$price
  
  own_m <- outer(df17$firm, df17$firm, FUN = "==") * 1
  
  # Coordinated effects
  if (lambda > 0) { 
    own_m <- own_m + lambda * (1 - own_m)  # If lambda > 0, to simulate coordinated effect
  }
  
  # Same nest matrix
  same_nest_m <- outer(df17$class, df17$class, FUN = "==") * 1
  
  repeat{
    
    # Step 1: update shares
    delta_new  <- delta_nl_17 + alpha_nl * (prices_in_nest - df17$price) 
    exp_delta      <- exp(delta_new / (1 - sigma_nl))  
    nest_sum   <- tapply(exp_delta, df17$class, sum) 
    s_within_new <- exp_delta / nest_sum[df17$class] 
    nest_inclusive <- nest_sum^(1 - sigma_nl)
    
    nest_inclusive_values <- tapply(exp_delta, df17$class, sum)^(1 - sigma_nl) 
    s_new <- (exp_delta * nest_sum[df17$class]^(-sigma_nl)) / (1 + sum(nest_inclusive_values))
    
    # Step 2: Derivative matrix
    dsdp_m <- -alpha_nl * outer(s_new, s_new) 
    
    dsdp_m <- dsdp_m - same_nest_m *          
      (alpha_nl * sigma_nl / (1 - sigma_nl)) *
      outer(s_new, s_within_new)
    
    diag(dsdp_m) <- (alpha_nl / (1 - sigma_nl)) * s_new * 
      (1 - sigma_nl * s_within_new - (1 - sigma_nl) * s_new) # Overwrite the diagonal with the nested logit own-price derivative
    
    # Step 3: Markup
    markup_new <- as.numeric(solve(-(dsdp_m * own_m)) %*% s_new)  # Solve the post-merger BLP FOC
    
    # Step 4: New price
    prices_out <- mc_pre + markup_new # Compute new equilibrium candidate prices
    
    # Step 5: Convergence verification
    if (max(abs(prices_out - prices_in_nest)) < 1e-6) break # Check convergence: if the maximum price change across all products is below 0.001
    
    prices_in_nest <- prices_out # Update the price vector for the next iteration 
  }
  
  # CS
  delta_final    <- delta_nl_17 + alpha_nl * (prices_out - df17$price) # Compute final mean utilities at the converged post-merger prices.
  
  exp_d_final    <- exp(delta_final / (1 - sigma_nl)) # Compute exponentiated scaled utilities at the converged prices.
  
  nest_sum_final <- tapply(exp_d_final, df17$class, sum) # Compute nest-level sums of exponentiated scaled utilities at the converged prices.
  
  eta_g <- (1 - sigma_nl) * log(nest_sum_final) # Compute post-merger nest-level inclusive values.
  
  CS_nl_merge <- (1/abs(alpha_nl)) * log(1 + sum(exp(eta_g))) * 1000 # Compute post-merger consumer surplus
  
  list(price  = prices_out,   # Compile what we got with a list for the preparation of the later format.
       markup = markup_new, 
       share  = s_new, 
       CS     = CS_nl_merge, 
       lerner = mean(markup_new / prices_out))
}

# 7-4: Apply the function for the merge cases: FCA-PSA
# Pre-requisition setting: 
delta_nl_17 <- df17$lhs - sigma_nl * df17$lwgshare

dfM_nl <- df17
dfM_nl$firm[dfM_nl$firm %in% c("FCA", "PSA")] <- "Stellantis"
res_Stellantis <- simulate_M_nl(dfM_nl, mc_nest_pre17, alpha_nl, sigma_nl, delta_nl_17)

# 7-5: Comparison / Verification

Stellantis_idx <- df17m$firm %in% c("FCA", "PSA")

table_nl_merger <- data.frame(
  
  Variables = c(
    "Mean price for every products", "Mean price, Stellantis (FCA + PSA)", "Mean price, outsiders of Stellantis", 
    "Mean markup for every products", "Mean markup, Stellantis (FCA + PSA)", "Mean markup, outsiders of Stellantis", 
    
    "Lerner Index", "Consumer Index"
  ), 
  
  Ante_merge = c(
    mean(df17$price)*1000, mean(df17$price[Stellantis_idx])*1000, mean(df17$price[!Stellantis_idx])*1000,
    mean(markup_nest_pre17)*1000, mean(markup_nest_pre17[Stellantis_idx])*1000, mean(markup_nest_pre17[!Stellantis_idx])*1000, 
    mean(lerner_nest_pre), CS
  ),
  
  Post_merge = c(
    mean(res_Stellantis[["price"]])*1000, mean(res_Stellantis[["price"]][Stellantis_idx])*1000, mean(res_Stellantis[["price"]][!Stellantis_idx])*1000, 
    mean(res_Stellantis[["markup"]])*1000, mean(res_Stellantis[["markup"]][Stellantis_idx])*1000, mean(res_Stellantis[["markup"]][!Stellantis_idx])*1000,
    mean(res_Stellantis[["lerner"]]), res_Stellantis[["CS"]]
  ), 
  
  Diff_in_percentage = c(
    ((mean(res_Stellantis[["price"]]) - mean(df17$price)) / mean(df17$price))*100, ((mean(res_Stellantis[["price"]][Stellantis_idx]) - mean(df17$price[Stellantis_idx])) / mean(df17$price[Stellantis_idx]))*100,
    ((mean(res_Stellantis[["price"]][!Stellantis_idx]) - mean(df17$price[!Stellantis_idx])) / mean(df17$price[!Stellantis_idx]))*100, 
    
    ((mean(res_Stellantis[["markup"]]) - mean(markup_nest_pre17)) / mean(markup_nest_pre17))*100, ((mean(res_Stellantis[["markup"]][Stellantis_idx]) - mean(markup_nest_pre17[Stellantis_idx])) / mean(markup_nest_pre17[Stellantis_idx]))*100, 
    ((mean(res_Stellantis[["markup"]][!Stellantis_idx]) - mean(markup_nest_pre17[!Stellantis_idx])) / mean(markup_nest_pre17[!Stellantis_idx]))*100, 
    
    ((mean(res_Stellantis[["lerner"]]) - mean(lerner_nest_pre)) / mean(lerner_nest_pre))*100, ((res_Stellantis[["CS"]] - CS_pre_nest) / CS_pre_nest)*100
  )
)

kable(table_nl_merger, caption = "[Nested Logit] The evaluation of hypothetical mergering case: Stellantis in 2017")

stargazer(table_nl_merger, 
          type = "text", 
          out = "output/Tables/table_merger_simulation_nested_logit.tex", 
          summary = FALSE, 
          rownames = FALSE)

View(table_nl_merger)

# 7-6: Extra: Check the validity of the results
comparison <- data.frame(
  id            = paste(df17$brand, df17$model, df17$fueltype, df17$kw, sep = "_"),
  firm          = df17$firm,
  class         = df17$class,
  price_pre     = df17$price,          
  price_post    = res_Stellantis$price
) %>%
  mutate(
    Difference  = price_post - price_pre,
    pct_change  = Difference / price_pre * 100,
    is_merging  = firm %in% c("FCA", "PSA")
  )

comparison %>%
  group_by(is_merging) %>%
  summarise(
    mean_pct = mean(pct_change),
    median_pct = median(pct_change),
    max_pct  = max(pct_change),
    n = n()
  )

merging_classes <- df17$class[df17$firm %in% c("FCA","PSA")] %>% unique()

comparison_nl %>%
  filter(!is_merging) %>%
  mutate(same_class_as_merger = class %in% merging_classes) %>%
  group_by(same_class_as_merger) %>%
  summarise(mean_pct = mean(pct_change), median_pct = median(pct_change), n = n())
