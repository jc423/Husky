<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#orgheadline8">1. K-Nearest-Neighbor</a>
<ul>
<li><a href="#orgheadline1">1.1. Resources</a></li>
<li><a href="#orgheadline2">1.2. Terms and Attributes</a></li>
<li><a href="#orgheadline5">1.3. Algorithm</a>
<ul>
<li><a href="#orgheadline3">1.3.1. Classification</a></li>
<li><a href="#orgheadline4">1.3.2. Regession</a></li>
</ul>
</li>
<li><a href="#orgheadline6">1.4. Choosing Neighbors</a></li>
<li><a href="#orgheadline7">1.5. Optimizations</a></li>
</ul>
</li>
</ul>
</div>
</div>

# [K-Nearest-Neighbor](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm)<a id="orgheadline8"></a>

## Resources<a id="orgheadline1"></a>

[Weighted KNN - Siddharth Deokar](http://www.csee.umbc.edu/~tinoosh/cmpe650/slides/K_Nearest_Neighbor_Algorithm.pdf)
[k-NN - Victor Lavrenko Youtube](https://www.youtube.com/watch?v=GbhZcvPLbQg&index=2&list=PLBv09BD7ez_68OwSB97WXyIOvvI5nqi-3)

## Terms and Attributes<a id="orgheadline2"></a>

-   Supervised Learning
    -   Classification - output class membership
    -   Regression - average value of knn
-   Non-Parametric
-   Configurability
    -   Weight contributions of neighbors by distance
-   Drawbacks
    -   Sensitive to local data structure, outliers
        -   take more neighbors
    -   Curse of Dimensionality - problematic when only few features are relevant but distance calculated using several
    -   Can't be missing any values for features
        -   want to have as little impact on distance as possible
        -   take average of feature across dataset
    -   Computationaly expensive
        -   space - need to store all training data
        -   time - O(nd)
-   [Distances](distances/euclidean-distance.md)
    -   Euclidean Distance
    -   Minkowski Distance
    -   Mahalanobis Distance
    -   Hamming Distance for categorical data
-   Vournoi Cell
    -   Set of point in space closer to training point than any other
    -   Exists around every training point and forms Vournoi Tesselation
    -   Boundary formed at edges of cells where classes differ
-   Potential for ties
    -   random
    -   use prior
    -   nearest 1-nn
-   Confidence can be calculated by (neighbors with output label) / (total neighbors)
-   Similar
    -   Parzen Windows - volume fixed

## Algorithm<a id="orgheadline5"></a>

[KNN implementation](file:///Users/crawfoj0/Me/haskell-projects/Husky/src/Supervised/KNN.hs)

### Classification<a id="orgheadline3"></a>

Given training data \({x_i, y_i}\) and an unknown point x, compute the distance between each
training data and x.  Take k closest neighbors and output class appearing most frequently.

### Regession<a id="orgheadline4"></a>

Same as above but take mean of labels.

## Choosing Neighbors<a id="orgheadline6"></a>

-   large value will result in most frequent class -> approaches prior
-   small value will overfit
-   Cross-validation
    -   Split into training and test and try for different values of k

## Optimizations<a id="orgheadline7"></a>

-   Classic KNN performance O(nd) - n training points with d dimensions
    -   reduce N, only check M of N training points
-   K-d trees
    -   for low dimensional, real valued data
    -   O(d log n)
    -   can miss neighbors
    -   choose attribute, find median and split on median (repeat as necessary)
    -   for unknown traverse tree and perform knn on subset of training data; "drop to bucket"
-   Inverted List
    -   high dimensional, real valued; by being high dimesional vectors of attributes will be sparse
    -   O (d'n') where d' < d and n' < n
    -   for each attribute, list training data containing attribute
    -   for unknown get all training data containing at least one attribute from unknown