# FCA–PSA Merger Simulation in the German Car Market

## Logit and nested logit demand estimation with BLP-style instruments, and a Nash-Bertrand merger simulation (R)

+ The original dataset is not included due to data redistribution restrictions. 
+ The analysis code and replication workflow are provided for transparency.
+ The dataset will be upload once the author ensure the credibility of usage.

This project estimates demand for differentiated car products in Germany (2012–2017), recovers marginal costs from a multi-product Bertrand-Nash pricing model, and simulates the unilateral price effects of a hypothetical FCA–PSA ("Stellantis") merger in 2017. It compares two demand specifications to show how the independence of irrelevant alternatives (IIA) property of the simple logit model distorts both pre-merger markups and predicted merger effects, and how a nested logit with vehicle-class nests corrects this.

**Key findings**

Hypothetical FCA–PSA merger, German market, 2017. Prices and markups in euros, simple (unweighted) means across products. Merger effects hold marginal costs fixed and solve for the new Nash-Bertrand equilibrium prices.

| 2017, % change after merger | Simple logit | Nested logit |
|---|---:|---:|
| Mean price, merging firms' products | +0.55% | +0.98% |
| Mean markup, merging firms' products | +1.37% | +3.48% |
| Consumer surplus | −0.13% | −0.27% |
| Pre-merger mean markup | 10,381 | 7,430 |
| Pre-merger mean Lerner index | 0.369 | 0.263 | 

1. **The simple logit overstates market power and understates merger harm.** Because IIA forces substitution to be proportional to market shares, it produces implausibly high pre-merger markups (Lerner index 0.37 vs 0.26 under nested logit).
2. **Allowing within-class substitution roughly doubles the predicted effect on the merging firms' prices and markups**, because FCA and PSA products are closer substitutes within segments than the logit model allows.
3. Spillovers to non-merging firms are negligible in both models.

Estimated parameters (2SLS, standard errors in parentheses): price coefficient \alpha = −0.101 (0.041) in the simple logit and \alpha = −0.093 (0.025) in the nested logit; nesting parameter \sigma = 0.380 (0.105), inside the theoretically consistent range [0, 1).

＊Consumer-surplus levels differ across the two models by construction, so only the percentage changes should be compared.*

**Substitution patterns (nested logit)**
Cross-price elasticity heatmap, 2017

Cross-price elasticities for 12 FCA and PSA products in 2017, ordered by vehicle class. The block structure along the diagonal is the nested logit at work: substitution inside a class (e.g. 0.013–0.061) is one to two orders of magnitude larger than across classes (≈0.000–0.004). Under the simple logit, every column would be constant off the diagonal. Colors use a signed log transform so that small cross-price effects remain visible next to large own-price elasticities; cell labels show the raw values.

---
## Research question

How would a horizontal merger between two firms selling differentiated products change equilibrium prices, markups and consumer welfare, and how sensitive is that answer to the substitution patterns imposed by the demand model?

## Data
The underlying dataset is **not included** in this repository because of data redistribution restrictions. The code expects a file data/cardata.csv with product-level observations (one row per model–fuel type–engine variant per year) containing:

| Variable | Description |
|---|:---|
| year | Market(year), 2012-2017 |
| firm, brand, model |	Producer, brand, model name |
| class, body, fueltype | 	Vehicle class (used as nest), body style, fuel type |
| price | produce price |
| quantity | united sold |
| hh | the number of household | 
| fueleff, kw, cylinders, weight, footprint | product characteristics | 

* Source: (provided by the University of Mannheim, the department of economics)
* Size: 6,846 product-year observations, 19 firms (17 active in 2017)
* **Market definition:** one market per year. Market size is **hh / 4**, so the potential market is assumed to be a quarter of households. Outside-good share is 1 − Σ share.

## Methodology
**Demand**: following Berry (1994), mean utility is recovered by inverting market shares:
```math
\ln(s_j) - \ln(s_i) = x_i \beta + \alpha p_j + \epsilon_j \quant (simple logit)
```

```math
\ln(s_j) - \ln(s_i) = x_i \beta + \alpha p_j + \sigma \ln(s_{j|g}) + \epsilon_j \quant (nested logit)
```
NOte that: nests g are vehicle classes. Simple logit includes brand, fuel type, class, body and year fixed effects; the nested logit includes brand, fuel type and year fixed effects.

**Endogeneity and instruments.** Price is correlated with the unobserved quality $\epsilon_j$. Characteristics-based instruments are built for fuel efficiency, power (kW), cylinders and footprint:
* #intra-firm#: sum of the characteristic over the firm's other products in the same year;
* #rival#: sum over all competitors' products in the same year;
* for the nested logit, additional <ins>same-class<ins> sums of fuel efficiency (own-firm and rival) help identify σ.

**Supply**: Firms are multi-product price setters under Bertrand-Nash competition. With ownership matrix Ω and share-derivative matrix ∂s/∂p, markups solve

$$ \mu = - \big( \Omega * \frac{\partial s}{\partial p'} \big)^{-1} s, \quant mc = p - c $$

Implied marginal costs are regressed on product characteristics and fixed effects as a plausibility check.

**Merger simulation.** The merging firms' rows in $\Omega$ are combined, marginal costs are held fixed, and prices are updated by fixed-point iteration! (shares → derivatives → markups → prices) until the maximum price change is below 1e-3. The nested logit simulation function includes an optional parameter {\color{red}$\lambda$} that puts a weight on cross-firm entries of $\Omega$, to illustrate partial coordination.

## How to reproduce
1. Place the data at data/cardata.csv  (The author is working on this part, requiring authorization from the resource.)
2. Open Github project 01.Rproj in RStudio so that here::here() resolves the project root.
3. Install the required packages: AER, here, tidyverse, data.table, stargazer, ggplot2, knitr.
4. Run the scripts in R/ **in numerical order**, in the same R session. Later scripts reuse objects created by earlier ones (e.g. dfc, alpha, BLP_NL).

## Limitations and possible extensions
* Static, unilateral-effects framework. No efficiencies, entry or repositioning, and only one year (2017) is simulated.
* Nesting structure is an assumption. Vehicle class is the only nesting criterion; alternatives such as brand tier or fuel type are not tested, and σ is estimated for a single nesting level.
* Instrument validity. Characteristics-based instruments rely on product characteristics being exogenous to $\epsilon_j$. Weak-instrument and over-identification diagnostics are computed in the scripts but not yet reported in the output tables.
* **Market size** {\color{red} (hh / 4)} is an assumption; results are not yet checked for sensitivity to it.
* Possible extensions: random-coefficients BLP estimation; alternative nests; quantity-weighted effects

## References 
* Berry, S. T. (1994). Estimating discrete-choice models of product differentiation. The Rand journal of economics, 242-262.
* Berry, S. T., Levinsohn, J. A., & Pakes, A. (1993). Automobile prices in market equilibrium: Part i and ii.
* Nevo, A. (2000). Mergers with differentiated products: The case of the ready-to-eat cereal industry. The Rand journal of economics, 395-421.





