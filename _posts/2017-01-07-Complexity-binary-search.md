---
layout: posts
title: On the complexity of binary searches
categories: Algorithms
tags: Algorithms Complexity
mathjax: true
comments: true
---

![](/files/posts/algorithms/binsearch_snap_100.gif){: .post-img-inset}

<!-- Split Here - IMG-snapshot -->

I would like to share here an alternative way to find the complexity of the
[binary search
algorithm](https://en.wikipedia.org/wiki/Binary_search_algorithm). A Binary
Search looks for an element in an ordered list, and is remarkable by both its
simplicity in terms of implementation and its “worst-case” complexity. By
complexity I mean the number of interations performed as function of the number
of elements in the list of elements. At least with respect to number of
iterations, no search algorithm based on comparisons outperforms binary searches
on average or in their worst-case scenario (see [The Art of Computer
Programming](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming), vol
3). The method I share here is an extension of what is commonly presented in
books and classes, but is nothing completely new.

<!-- Split Here - Snapshot -->

<img src="/files/posts/algorithms/binsearch_snap_100.gif"
alt="..." class='post-img' style="width:100%;" />

A binary search is very easy to define, and is based on the fact that the list
is ordered. Let’s say we are looking for an element $e$. First the list is divided
into two. The element in the middle, say $m$, is then compared to $e$: if $m > e$, then
$e$ might be in the first half of the list; if $m < e$, then e might be in the second
half; if $m=e$, then the element being looked up was found. It is clear now that
the main operation in this algorithms are the several comparisons that are
performed during the search.

As a quick review of how binary search works, here is a minimal python module
implementing a minimal version of the Binary Search. The visualization at the
beggining of the post is common way to represent it. To create such animations,
you can [use this small example of python
script](https://gist.github.com/thmosqueiro/63b9226fc94bdc3d9a5daf069017035a#file-binsearch_animation-py)
which only depends on numpy as matpltolib.


<script src="https://gist.github.com/thmosqueiro/63b9226fc94bdc3d9a5daf069017035a.js?file=BinarySearch.py"></script>

The challenge then is to be able to calculate how the number of comparisons
depends on the total length of the list, $n$. From the recursive nature of the
algorithm, we can write the number of operations $C(n)$ as function of the
number of operations needed for a list of length $n/2$,

$$ C(n) = C \left( \left\lfloor \frac{n}{2} \right\rfloor \right) + 1 \quad and \quad C(n),n \in \Z . $$

The [floor function](https://en.wikipedia.org/wiki/Floor_and_ceiling_functions)
$\lfloor \cdot \rfloor$ in $C(n)$ fixes cases where n is not even.


## Solving for powers of two

This first particular case is very easy to find, and is often presented in
Algorithms & Data Structures classes. Let's begin by assuming that $n$ is a
power of $2$, i.e, $n=2^k$ with $k\in \Z$. This simplifies $C(n)$ into

$$
C\left(2^k\right) = \left( \frac{2^k}{2} \right) + 1 = 2^{k-1} + 1 = 2^{k-2} + 2 = 2^{k-p} + p ,
$$

for any $p<n$. For $p=k$,

$$
C\left(2^k\right) = (1) + k = 1 + \log_2 \left( n \right) ,
$$

where we have inverted $k$ as function of $n$. Thus,

$$
C\left(n\right) = 1 + \log_2 \left( n \right)
$$

holds whenever $n$ is a power of two. Some textbooks even skip this proof, so I
tried to make each step very clear. This assumes worst case scenario, because we
went all the way to $C(1)$, which is equivalent to having a single last element
to be checked - i.e., the deepest level in the search was reached. For numerical
experiments, go to the end of this post.


## Solving the general case

If $n$ is not exactly a power of $2$, it still can be written as $n = 2^k + q$
with $k,q \in \Z$. Indeed, we can set

$$
0 < q < 2^{k+1} - 2^k = 2^k ,
$$

or $0 < q < 2^k$. If this condition is not true, then $k$ should be $k+1$ and
$q$ can be re-evaluated in the interval $0 < q < 2^{k+1}$. This means that the
factor $q$ is the smallest possible correction of $n$ from a power of $2$. Then,
we can write the complexity as

$$
C(n) = C \left( \left\lfloor \frac{n}{2} \right\rfloor \right) + 1 = C \left( \left\lfloor \frac{2^{k} + q }{2} \right\rfloor \right) + 1 .
$$

Let's define $q_0 = q$ and

$$
q_p = \left\lfloor \frac{ q_{p-1} }{2} \right\rfloor .
$$

Conveniently,

$$
\left\lfloor \frac{2^{k} + q }{2} \right\rfloor = 2^{k-1} + \left\lfloor \frac{q}{2} \right\rfloor = 2^{k-1} + q_1
$$

Thus, combining these last three pieces of information, we have

$$
C(n) = C \left( 2^{k-p} + \left\lfloor \frac{q_p}{2} \right\rfloor \right) + p .
$$

Now, if $p=k$, we have

$$
q < 2^k \quad \rightarrow \left\lfloor \frac{q_k}{2} \right\rfloor =
\left\lfloor \frac{q}{2^k} \right\rfloor = 0 ,
$$

which leads us to

$$
C(n) = C \left( 2^{k-k} + \left\lfloor \frac{q_k}{2} \right\rfloor \right) + k =  C \left( 1 + 0 \right) + k
$$

and therefore

$$
C(n) = C \left( 1 \right) + k = C\left( 2^k \right) = 1 + \left\lfloor \log_2 \left( n \right) \right\rfloor .
$$

And that is it: This is the general result and extends the special case when $n$
is a power of $2$. Interestingly, there only difference is that now we take the
floor of $log_2 (n)$. This means that binary searches in any list with size $2^k <
n \leq 2^{k}$ should have exactly the same worst-case performance. Although it
is very common to present only the first part of this demonstration in classes
and books, assuming that $n=2^k$, it is indeed quite easy to generalize the
result for any other integer. I particularly think this is an interesting
argument that may give an insight into other derivations. This is, of course,
closely related to the [Master
Theorem](https://en.wikipedia.org/wiki/Master_theorem) from Analysis of
Algorithms.


## How about wrapping this with a numerical test?

This expression for $C(n)$ is indeed very simple, and a great exercise is to
test it numerically. Below we have results of numerical simulations with
randomly created lists of several different sizes, the average complexity for
each size (dark green line) and the comparison with the calculated formula (red
line), $1 + \left\lfloor \log_2 \left( n \right) \right\rfloor$.

<img src="/files/posts/algorithms/BinSearchComplexity.png"
alt="" class='post-img' style="width:90%;" />

In the plot above, the many blue dots represent all the different numbers of
iterations used in each of the 300 different arrays tested. To help visualize
where these dots lie more frequently, I used a small jitter and
alpha-transparency. The final equation for $C(n)$ always estimates the worst
case, scaling linearly in this log-linear plot. As expected, the average however
is slightly smaller than the worst case, although not much. If you want to try
with more than $10^6$ elements, the script may take a few minutes, but the
linear trend continues. Note that, for higher sizes, the number of repetitions
must also grow to guarantee a reasonable estimation of the average complexity.
To reproduce this numerical test, you can use the Python script below.

<script src="https://gist.github.com/thmosqueiro/63b9226fc94bdc3d9a5daf069017035a.js?file=BinarySearch_complexity.py"></script>
