# Memory Calculation (Theoretically)

### Setting

We take a look at the following setup:

- Just spline base-learner:
  - Number of base-learner: $n_\text{bl}$
  - spline degree: $d_\text{spline}$
  - Number of columns per base-learner: $p = n_\text{knots} + 2(d_\text{spline} - 1)$
- Sparse matrix representation

### Sparse Data

The memory allocated for a spline base sparse matrix can be calculated by:
$$
\text{mem}_\text{X,sp}(n, d_\text{spline},p) = n(d_\text{spline} + 1)\cdot 12\ \text{Bytes}\ + (p + 3) \cdot 4\ \text{Bytes}
$$
The $12\ \text{Bytes}$ are set together by saving the values of the sparse matrix as double($8\ \text{Bytes}$) and also the row index for each of these values as integer ($4\ \text{Bytes}$) The $p + 3$ comes from saving the column index and the starting index ($p + 1$) and additional two integer for the dimension.

Additionally, we have to store the penalty matrix and the number of knotsfor all base-learner where one penalty matrix consists of $\text{mem}_\text{sp,meta}(p,n_\text{knots}) = p^2\ \cdot 8\ \text{Bytes} + (n_\text{knots} + 2) \cdot 8\ \text{Bytes} $ and the number of knots 

Using this, the memory consumption when storing all data matrices can be calculated as:
$$
\text{mem}_\text{sparse}(n, d_\text{spline}, p) = n_\text{bl} (\text{mem}_\text{X,sp}(n,d_\text{spline},p)\ +\ \text{mem}_\text{sp,meta}(p,n_\text{knots}))
$$

### Binned Sparse Data

The memory allocated depends on the order of binning $b$. Additionally, we have to store the index vector $k \in \mathbb{R}^n$ as integer for each binned matrix, the allocated memory then is:
$$
\text{mem}_\text{X,bin}(n, d_\text{spline},p, b) = n^{1/d}(d_\text{spline} + 1)\cdot 12\ \text{Bytes}\ + n \cdot 4\ \text{Bytes}\ + (p + 3) \cdot 4\ \text{Bytes}
$$
Again, we have to store the penalty matrix which gives us in total:
$$
\text{mem}_\text{bin}(n, d_\text{spline}, p, d) = n_\text{bl} (\text{mem}_\text{X,bin}(n,d_\text{spline},p,d)\ +\ \text{mem}_\text{sp,meta}(p,n_\text{knots}))
$$

### Example

- $n = 100000$
- $n_\text{bl} = 300$
- $p = 24$

- $b = 2$

$$
\begin{align*}
\text{mem}_\text{sparse} &= 1375 \ \text{MB} \\
\text{mem}_\text{bin}    &= 120.2\ \text{MB}
\end{align*}
$$

### Additional Metadata

- Metadata used by simulating data etc. is pretty huge, for the upper example this is $993.5\ \text{MB}$. 

- Parameter of a base-learner are stored in $\mathbb{R}^{1\times p}$ matrices. For the upper example $300 \cdot 24 \cdot 8\ \text{Byte} = 0.054\ \text{MB}$ 

- Raw data is stored as vector for each base-learner: $n_\text{bl} \cdot n \cdot 8\ \text{Bytes}$. For the upper example $300 \cdot 100000 \cdot 8\ \text{Bytes} = 228.9\ \text{MB}$

- The response stores as vector of dimension $n$ the response, initialization, pseudo residuals, and prediction scores, hence $4 \cdot n \cdot 8\ \text{Bytes}$. For the example $4 \cdot 100000 \cdot 8\ \text{Bytes} = 3.05\ \text{MB}$

- Parameter map of current estimates. This depends on the number of selected base-learner, but the size is maximal if all 300 base-learner were selected and then $n_\text{bl} \cdot p \cdot 8 \ \text{Bytes}$. In the example $300 \cdot 24 \cdot 8\ \text{Bytes} = 0.054\ \text{MB}$

- Some kind of "cache" is stored in each iteration for $(X^TWX)^{-1}$ to fasten the fitting process. This requires additional $n_\text{bl}\cdot p^2\cdot 8\ \text{Bytes}$. In the example $300 \cdot 24^2 \cdot 8\ \text{Bytes} = 1.318 \ \text{MB}$