# Appendix

## Equations

The following are equations utilized to calculate the intermediate and final scores.

::: {.panel-tabset}

### Geometric Mean ($\text{GM}$)

$$ \text{GM} = \left(\prod_{j=1}^{n} M_j\right)^{\frac{1}{n}} $$
  where $s_i$ is the individual domain score.

### LaTeX

```latex
s_i = \left(\prod_{j=1}^{n} M_j\right)^{\frac{1}{n}}
```

### R

```r
library(psych)
geometric_mean <- geometric.mean(data_frame$vector)
```

:::
  
  ::: {.panel-tabset}

### Gini Index Complement ($G_{\text{c}}$)

$$ G_{\text{c}} = 1 - \left( \frac{1}{2n^2\mu} \sum_{i=1}^{n} \sum_{j=1}^{n} | y_i - y_j | \right) $$
  Where: $n$ is the number of domains or degrees, $y_i$ and $y_j$ are domain scores, 
and $\mu$ is the mean domain score.

### LaTeX

```latex
G' = 1 - \left( \frac{1}{2n^2\mu} \sum_{i=1}^{n} \sum_{j=1}^{n} | y_i - y_j | \right)
```

### R

```r
library(psych)
geometric_mean <- geometric.mean(data_frame$vector)
```

:::