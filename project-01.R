library(AER)
library(tidyverse)
library(data.table)
library(ggplot2)

dfc <- data.frame(fread("/Users/ishimare/Desktop/cardata.csv")) # Copy the pathway of the file
                                                                # 正式版會置換成瀏覽者的裝置路徑
# dfc$class <- str_squish(dfc$class)
#--- Session 1: Modify the database ---#
dfc <- dfc %>%
  
  group_by(year) %>% 
  
  mutate(market_size = hh/4) %>% # mutate(): Create or transform variables:market_size while preserving the original data structure.
  
  mutate(share = quantity / market_size) %>% # Create variabls: share while preserving the original data structure.
  
  mutate(outshare = 1 - sum(share)) %>% # mutate(): Create new variable outshare as the share of consumers who bought no car (outside good share = 1 minus total inside shares).
  
  mutate(lhs = log(share / outshare))  %>% # muatae(): Create new variable "lhs" as the relative utility between targeted good and outside options of it.
  
  ungroup() # ungroup(): Remove grouping metadata to ensure subsequent operations apply to the entire dataset

# Establishing 8 instrument variables: 
dfc <- dfc %>% 
  
  group_by(firm, year) %>%  
  
  mutate(firm_fueleff = sum(fueleff),   
         firm_kw = sum(kw), 
         firm_cylinders = sum(cylinders), 
         firm_footprint = sum(footprint)) %>%
  
  ungroup() %>% 
  
  group_by(year) %>% 
  
  mutate(all_fueleff = sum(fueleff), # Calculate total market-wide fuel efficiency for each year
         all_kw = sum(kw),  # Calculate total market-wide kilowatts for each year
         all_cylinders = sum(cylinders), # Calculate total market-wide cylinders for each year
         all_footprint = sum(footprint)) # Calculate total market-wide footprint for each year

dfc$IV_fueleff_intra <- (dfc$firm_fueleff - dfc$fueleff) / 10000 # Create now column: Intra-firm IV for fuel efficiency
dfc$IV_kw_intra <- (dfc$firm_kw - dfc$kw) / 10000 # Create now column: Intra-firm IV for kilowatts
dfc$IV_cylinders_intra <- (dfc$firm_cylinders - dfc$cylinders) / 10000 # Create now column: Intra-firm IV for cyklinders
dfc$IV_footprint_intra <- (dfc$firm_footprint - dfc$footprint) / 10000 # Create now column: Intra-firm IV for footprints

dfc$IV_fueleff_rival <- (dfc$all_fueleff - dfc$firm_fueleff) / 10000 # Create now column: Inter-firm IV for fuel efficiency
dfc$IV_kw_rival <- (dfc$all_kw - dfc$firm_kw) / 10000 # Create now column: Inter-firm IV for kilowatts
dfc$IV_cylinders_rival <- (dfc$all_cylinders - dfc$firm_cylinders) / 10000 # Create now column: Inter-firm IV for cylinders
dfc$IV_footprint_rival <- (dfc$all_footprint - dfc$firm_footprint) / 10000 # Create now column: Inter-firm IV for footprint

#--- Session 2: 2SLS  ---#

summary(lm(price ~ fueleff + kw + cylinders + weight + footprint + 
             factor(brand) + factor(fueltype) + factor(class) + factor(body)  + 
             IV_fueleff_intra + IV_fueleff_rival +IV_kw_intra + IV_kw_rival + 
             IV_cylinders_intra + IV_cylinders_rival + IV_footprint_intra + IV_footprint_rival, 
           data = dfc)) 


BLP <- ivreg(lhs ~ price + fueleff + kw + cylinders + weight + footprint + 
               factor(brand) + factor(fueltype) + factor(class) + factor(body) + factor(year)| 
               fueleff + kw + cylinders + weight + footprint + 
               factor(brand) + factor(fueltype) + factor(class) + factor(body) + factor(year) + 
               IV_fueleff_intra + IV_fueleff_rival +  
               IV_kw_intra + IV_kw_rival + 
               IV_cylinders_intra + IV_cylinders_rival + 
               IV_footprint_intra + IV_footprint_rival, 
             data = dfc) 

summary(BLP, diagnostics = TRUE)

alpha <- as.numeric(coefficients(BLP)[2])

#--- Session 3:  ---#

# 3-1  : Ownership matrix
owner_year <- matrix(NA,
                     nrow = nrow(dfc),
                     ncol = length(unique(dfc$firm)))

for (j in 1:ncol(owner_year)){
  
  owner_year[,j] <- as.numeric(dfc$firm == unique(dfc$firm)[j])
  
}

# 3-2: List Derivative matrix and Markup
market_vec <- unique(dfc$year)
mc <- rep(NA, nrow(dfc))

for (j in 1:length(market_vec)) {
  
  # Mark segregating by year
  idx <- market_vec[j] == dfc$year
  
  # Ownership matrix for each year
  own_year_j <- owner_year[idx, ]
  omega <- own_year_j %*% t(own_year_j)
  
  # Derivative matrix
  share_j <- dfc$share[idx]
  derivative <- matrix(0, nrow(own_year_j), nrow(own_year_j))
  derivative <- -alpha * share_j %*% t(share_j)
  diag(derivative) <- alpha * (1 - share_j) * share_j
  
  # Markup 
  markup_j <- as.numeric(-solve(t(derivative) * omega) %*% share_j)
  mc[idx] <- dfc$price[idx] - markup_j
}

# 3-3: Supply side analysis

mc_ols <- lm(mc ~ price + weight + fueleff + kw + footprint + cylinders +
               factor(model) + factor(class) + factor(brand) + factor(year)  + factor(body), 
             data = dfc)

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

price_in <- df17m$price
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

#--- Session 5: Nested Logit ---#

# 5-1: Pre-requisite for nested logit
dfc <- dfc %>%
  
  group_by(year, firm, class) %>%
  
  mutate(firm_nestFE = sum(fueleff),
         firm_nestKW = sum(kw),
         firm_nestCL = sum(cylinders),
         firm_nestFP = sum(footprint)) %>%
  
  ungroup() %>%
  
  group_by(year, class) %>%
  
  mutate(nest_share = sum(share),
         
         all_nestFE = sum(fueleff),
         all_nestKW = sum(kw),
         all_nestCL = sum(cylinders),
         all_nestFP = sum(footprint)) %>%
  
  mutate(group_share = share / nest_share, 
         lwgshare = log(group_share)) %>%
  
  ungroup() %>%
  
  # Nested Instrument variables
  mutate(IVnest_IntraFE = (firm_nestFE - fueleff) / 10000,
         IVnest_IntraKW = (firm_nestKW - kw) / 10000, 
         IVnest_IntraFP = (firm_nestFP - footprint) / 10000, 
         IVnest_IntraCL = (firm_nestCL - cylinders) / 10000, 
         
         IVnest_RivalFE = (all_nestFE - firm_nestFE) / 10000,
         IVnest_RivalKW = (all_nestKW - firm_nestKW) / 10000, 
         IVnest_RivalFP = (all_nestFP - firm_nestFP) / 10000, 
         IVnest_RivalCL = (all_nestCL - firm_nestCL) / 10000)

BLP_NL <- ivreg(lhs ~ price + lwgshare + fueleff + kw + cylinders + weight + footprint + 
                factor(brand) + factor(fueltype) + factor(year)|
                  
                fueleff + kw + cylinders + footprint + weight + 
                factor(brand)  + factor(fueltype) + factor(year) + 
                
                  IV_fueleff_intra   + IV_fueleff_rival +      # BLP instruments
                  IV_footprint_intra + IV_footprint_rival +  # BLP instruments
                  IVnest_IntraFE + IVnest_RivalFE, data = dfc)
  


summary(BLP_NL, diagnostics = TRUE)

alpha_nl <- as.numeric(coefficients(BLP_NL)[2])
sigma_nl <- as.numeric(coefficients(BLP_NL)[3])

# 5-2: Nested Logit - Markup and Marginal Cost

markup_nl <- rep(NA, nrow(dfc))
elasticities_list <- list()
data_list <- list()

for (t in unique(dfc$year)){
  
  idx_nest <- dfc$year == t
  
  n_nest_t <- sum(idx_nest)
  p_nest_t <- dfc$price[idx_nest]
  s_nest_t <- dfc$share[idx_nest]
  s_nest_group <- dfc$group_share[idx_nest]
  class_nest_t <- dfc$class[idx_nest]
  firm_nest_t <- dfc$firm[idx_nest]
  
  # Ownership matrix per annual market
  Omega_nest <- outer(firm_nest_t, firm_nest_t, FUN = "==") * 1
  
  # Derivative matrix for the nested logit
  derivative_nl <- matrix(0, n_nest_t, n_nest_t)
  
  for (j in 1:n_nest_t){  # Outer loop: iterate over each product j as the "affected" product.
    
    for (k in 1:n_nest_t){# Inner loop: iterate over each product k whose price is changing.
      
      if (j == k){
        # From the same group, and the own-price derivative
        derivative_nl[j,j] <- (alpha_nl / (1 - sigma_nl)) * (1 - sigma_nl * s_nest_group[j] - (1 - sigma_nl) * s_nest_t[j]) * s_nest_t[j]
        
      } else if (class_nest_t[j] == class_nest_t[k]){
        # Cross, from the same group's cross derivative
        derivative_nl[j,k] <- -alpha_nl * (sigma_nl / (1 - sigma_nl) * s_nest_group[k] + s_nest_t[k]) * s_nest_t[j]
      } else {
        # For products in different nest
        derivative_nl[j,k] <- -alpha_nl * s_nest_t[j] * s_nest_t[k]
      }
    }
  }
  
  # Markup and Marginal Cost
  markup_nl[idx_nest] <- as.numeric(-solve(derivative_nl * Omega_nest) %*% s_nest_t)
  
  # Elasticities:
  scale_nl <- matrix(1 / s_nest_t) %*% matrix(p_nest_t, nrow = 1)
  elasticities <- scale_nl * derivative_nl
  
  id_t <- paste(dfc$brand[idx_nest], dfc$model[idx_nest], dfc$fueltype[idx_nest], dfc$kw[idx_nest], sep = "_")
  rownames(elasticities) <- colnames(elasticities) <- id_t
  
  elasticities_list[[as.character(t)]] <- elasticities
  data_list[[as.character(t)]] <- dfc[idx_nest, ]
}

mc_nl <- dfc$price - markup_nl

# 5-3: Fetch targeting products

df17 <- data_list[["2017"]]
elas17 <- elasticities_list[["2017"]]

target_idx <- which(
  (df17$brand == "Fiat"           & df17$model == "500C"      & df17$fueltype == "gasoline" & df17$kw == 5.1) |
    (df17$brand == "Fiat"           & df17$model == "Panda"     & df17$fueltype == "gasoline" & df17$kw == 5.1) |
    (df17$brand == "Abarth"         & df17$model == "595C/695C" & df17$fueltype == "gasoline" & df17$kw == 13.2) |
    (df17$brand == "Peugeot"        & df17$model == "208"       & df17$fueltype == "gasoline" & df17$kw == 6.0) |
    (df17$brand == "Citroen"        & df17$model == "C3"        & df17$fueltype == "gasoline" & df17$kw == 6.0) |
    (df17$brand == "DS Automobiles" & df17$model == "DS 3"      & df17$fueltype == "gasoline" & df17$kw == 8.1) |
    (df17$brand == "Alfa Romeo"     & df17$model == "Giulietta" & df17$fueltype == "gasoline" & df17$kw == 8.8) |
    (df17$brand == "Peugeot"        & df17$model == "308"       & df17$fueltype == "gasoline" & df17$kw == 9.6) |
    (df17$brand == "Citroen"        & df17$model == "C4"        & df17$fueltype == "gasoline" & df17$kw == 9.6) |
    (df17$brand == "Jeep"           & df17$model == "Renegade"  & df17$fueltype == "gasoline" & df17$kw == 10.3) |
    (df17$brand == "Peugeot"        & df17$model == "3008"      & df17$fueltype == "gasoline" & df17$kw == 12.1) |
    (df17$brand == "Fiat"           & df17$model == "500X"      & df17$fueltype == "gasoline" & df17$kw == 8.1)
)

elasticities_17 <- elas17[target_idx, target_idx]

# 5-4: Visualization 

# 步驟1：先算出車款對應的class資訊，這是elas_df排序時需要用到的參照表
car_class_lookup <- data_list[["2017"]] %>%
  filter(paste(brand, model, fueltype, kw, sep = "_") %in% rownames(elasticities_17)) %>%
  transmute(id_t = paste(brand, model, fueltype, kw, sep = "_"), class) %>%
  distinct()

# 步驟2：依照class排序，讓同nest的車彼此相鄰
ordered_ids <- car_class_lookup %>%
  arrange(class, id_t) %>%
  pull(id_t)

# 步驟3：把矩陣轉成long format，並套用上面決定好的排序
elas_df <- as.data.frame(elasticities_17) %>%
  rownames_to_column("row_car") %>%
  pivot_longer(-row_car, names_to = "col_car", values_to = "elasticity") %>%
  mutate(
    row_car = factor(row_car, levels = ordered_ids),
    col_car = factor(col_car, levels = ordered_ids),
    # log-modulus transform：保留正負號，同時壓縮尺度差異，讓小數值的cross-elasticity也看得出顏色深淺
    elasticity_trans = sign(elasticity) * log1p(abs(elasticity))
  )

# 步驟4：畫圖
ggplot(elas_df, aes(x = col_car, y = row_car, fill = elasticity_trans)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", elasticity)), size = 2.3, color = "black") +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "elasticity\n(log-scaled)"
  ) +
  scale_y_discrete(limits = rev) +   # 讓row順序由上到下對應矩陣視覺習慣
  labs(
    title = "2017 FCA-PSA Case：Cross-Price Elasticity Matrix basing on nested classess ordering",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("elasticity_heatmap_2017.png", width = 10, height = 8, dpi = 300)

# 5-5: Nested merge case for PSA-FCA

  # 5-5-1: Pre-requisition setups
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
  ##########################
  
  # Markup (Generate variables: mc_pre)
  markup_nest_pre17 <- as.numeric(-solve(derivative_nest_pre * own_nest_pre) %*% df17$share)
  mc_nest_pre17 <- df17$price - markup_nest_pre17
  lerner_nest <- markup_nest_pre17 / df17$price

  # 5-5-2: Consumer Surplus
  delta_nl <- df17$lhs - sigma_nl * df17$lwgshare
  exp_nl_preM <- exp(delta_nl / (1 - sigma_nl))
  nest_sum_preM <- tapply(exp_nl_preM, df17$class, sum)
  
  eta_nest_preM <- (1 - sigma_nl) * log(nest_sum_preM)
  
  CS <- (1 / abs(alpha_nl)) * log(1 + sum(exp(eta_nest_preM))) * 1000
  
  # 5-5-3: Merge, establishing the function for processes
  
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

  # 5-5-4: Apply the function for the merge cases: FCA-PSA
  # Pre-requisition setting: 
  delta_nl_17 <- df17$lhs - sigma_nl * df17$lwgshare
  
  dfM_nl <- df17
  dfM_nl$firm[dfM_nl$firm %in% c("FCA", "PSA")] <- "Stellantis"
  res_Stellantis <- simulate_M_nl(dfM_nl, mc_nest_pre17, alpha_nl, sigma_nl, delta_nl_17)
  
  # 5-5-5: Comparison / Verification
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

  
  
# EXTRA: Verification for the correctness of the model

  # 1:  
  dfc %>%
    group_by(year) %>%
    summarise(total_inside_share = sum(share), outshare_check = 1 - sum(share)) %>%
    print()
  # outshare_check 應該全部是正值，且不會離0太近（不會有市場幾乎被inside goods佔滿的情況）
  
  # 2:
  which(mc_pre < 0)
  df17m[which(mc_pre < 0), c("brand","model","fueltype","kw","share","price")]
  
  # 3: 
  comparison_nl <- data.frame(
    id         = paste(df17$brand, df17$model, df17$fueltype, df17$kw, sep = "_"),
    firm       = df17$firm,
    class      = df17$class,
    price_pre  = df17$price,
    price_post = res_Stellantis$price
  ) %>%
    mutate(pct_change = (price_post - price_pre) / price_pre * 100,
           is_merging = firm %in% c("FCA", "PSA"))
  
  comparison_nl %>%
    group_by(is_merging) %>%
    summarise(mean_pct = mean(pct_change), median_pct = median(pct_change),
              max_pct = max(pct_change), min_pct = min(pct_change), n = n())

              
  