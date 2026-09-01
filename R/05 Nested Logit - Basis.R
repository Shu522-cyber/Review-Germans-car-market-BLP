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