---
layout: posts
title: On the complexity of binary searches
categories: Algorithms
tags: Algorithms Complexity
mathjax: true
comments: true
---

I would like to share here an alternative way to find the complexity of [binary
search algorithms](https://en.wikipedia.org/wiki/Binary_search_algorithm). By
complexity I mean a measure of number of basic operations performed as function
of some input parameter. Binary searches look for an element in an ordered list
and is remarkable by its simplicity in terms of implementation and “worst-case”
low complexity. The method I will share here is an extension of what is commonly
presented, which does not requires the [Master
Theorem](https://en.wikipedia.org/wiki/Master_theorem).

<!-- Split Here - Snapshot -->

A binary search is very easy to define, and is based on the fact that the list
is ordered. Let’s say we are looking for an element e. First the list is divided
into two. The element in the middle, say $m$, is then compared to e: if $m > e$, then
e might be in the first half of the list; if $m < e$, then e might be in the second
half; if $m=e$, then the element being looked up was found. It is clear now that
the main operation in this algorithms are the several comparisons that are
performed during the search.

The challenge then is to be able to calculate how the number of comparisons
depends on the total length of the list, $n$. From the recursive nature of the
algorithm, we can write the number of operations $C(n)$ as function of the
number of operations needed for a list of length $n/2$,

$$ C(n) = C \left( \left\lfloor \frac{n}{2} \right\rfloor \right) + 1 \quad and \quad C(n),n \in \Z . $$

The floor function $C(n)$ fixes cases where n is not even


## Solving for powers of two

Before going on, we will solve this problem for a simpler case: we begin by
assuming that $n$ is a power of $2$, i.e, $n=2^k$ with $k\in \Z$. This
simplifies $C(n)$ into

$$
C\left(2^k\right) = \left( \frac{2^k}{2} \right) + 1 = 2^{k-1} + 1 = 2^{k-2} + 2 = 2^{k-p} + p ,
$$

for any $p<n$. For $p=k$,

$$
C\left(2^k\right) = (1) + k = 1 + \log_n \left( n \right) ,
$$

where we have inverted $k$ as function of $n$. Thus,

$$
C\left(2^k\right) = 1 + \log_2 \left( n \right)
$$

holds whenever $n$ is a power of two. Some textbooks show a similar proof in
this particular case, but not every step is always very clear. This assumes
worst case scenario, because we went all the way to $C(1)$, which is equivalent
to having a single last element to be checked during the binary search. I share
below some numerical results.


## Solving the general case

Assume $n$ is not exactly a power of $2$, but can be written as $n = 2^k + q$
with $q \in \Z$. Indeed, to make both $k$ and $q$ be particularly
useful, let

$$
0 < q < 2^{k+1} - 2^k = 2^k ,
$$

or $0 < q < 2^k $. This means that the correction factor $q$ must be, at most,
the last integer before $2^k$. Then, we can write the complexity as

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
q < 2^k \quad \rightarrow \left\lfloor \frac{q_k}{2} \right\rfloor = 0 ,
$$

which leads us to

$$
C(n) = C \left( 2^{k-k} + \left\lfloor \frac{q_k}{2} \right\rfloor \right) + k =  C \left( 1 + 0 \right) + k
$$

and therefore

$$
C(n) = C \left( 1 \right) + k = C\left( 2^k \right) = 1 + \left\lfloor \log_2 \left( n \right) \right\rfloor .
$$

And that is it, this is the general result and extends the special case when $n$
is a power of $2$ by simply taking the floor of the log. As anticipated in the
first paragraph, this result could have been obtained easily from the Master
Theorem, however it is sometimes very instructive to be able to derive such
results without needing other results.


## How about wrapping this with a numerical test?
