#--- Session 3: Supply Side ---#

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

stargazer(
  BLP, mc_ols, 
  type = "text",
  dep.var.labels = c("delta", "marginal cost"), 
  omit = c("brand", "class", "body", "model"),           
  digits = 3,                                   
  header = FALSE,
  out = "output/Tables/table_D&S_analysis_simple_logit.tex"
)
