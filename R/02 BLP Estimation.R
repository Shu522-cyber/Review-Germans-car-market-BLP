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
