---
layout: posts
title: Break a stick in two random points, can you then build a triangle?
tags: Probability Math NotReady
mathjax: true
comments: true
---

<!-- Split Here - IMG-snapshot -->

This is apparently a classical problem, and the first challenge is to acknowledge that is is
not always possible to make a triangle out of 3 segments. Let's start from the beggining:

 <blockquote>
 Consider a stick of length 1. Select two points uniformly at random on the stick
 and break the stick at those points. What is the probability that the three
 segments form a triangle?
 </blockquote>

 <!-- Split Here - Snapshot -->

Unexpectedly, the answer is not intuitive, and most of time people will
try and guess it. Asking friends, I've got the answers 1, 1/2 and 0 (in order of
frequency). They are all wrong. I came across this problem during the
[3rd QCBio Retreat](https://qcb.ucla.edu/events-seminars/retreat/) during the lunch.
I immediatelly tried to solve it using probability, but got stuck in a passage.
However, another guy was able to come up with a quite interesting solution.
In the afternoon I was able to close my calculations. Here are both solutions.

Let me thank Brenno Barbosa for helping me cut a step in my formal solution.

One of the reasons why I am writing this blog entry is because I think both
of these solutions are very different from [the canonical solution I
found over the web](https://mathoverflow.net/a/66797).


## Why is it now always possible to build a triangle?

The first thing I noticed when asking other people was that it may be surprising
that you can not build a triangle regardless of where you break the sick. A
simple enough counter example is when you break the pieces very, very close to
one of the ends. Two segments are really short, and the third one is very long.
In this case, you can't connect all three vertices.

We can visualize this in the image below, that I borrowed from [Wikipedia](https://en.wikipedia.org/wiki/Triangle_inequality#/media/File:TriangleInequality.svg).

<img src="/files/posts/triangle-problem/triangle.png" alt="..." class='post-img' style="width:300px;" />

This is a result of the [triangle inequality](https://en.wikipedia.org/wiki/Triangle_inequality).
Following the same scheme shown in the figure above, if $x$, $y$ and $z$ are the
three sides of the triangle, then

$$ x \leq y + z . $$

To connect this with our present problem, assume that $ x + y + z = 1 $ and that
the two randomly chosen points were $x$ and $x+y$. However, in this case we also
must satisfy

$$ y \leq x + z $$

and

$$ z \leq x + y . $$

Any solution $(x,y,z)$ that does not satisfy all of the equations above represents
a possibility of breaking the stick and not being able to build a triangle with
the resulting three segments.


## A clever and direct solution: graphically enumerating the solutions

Let's assume points $A$ and $B$ are the two points selected.
The three inequalities from the previous section define a region of the plane
$A$ and $B$ in which solutions are admissible. If we can somehow calculate
the area of that region and normalize it, that solves the problem.

Here is how. Assume $A < B$, i.e., $A = x$ and $B = y + x$. From the first inequality,
$A < 1/2$ must be satisfied. If $A$ is larger than half of the stick, then there
is no point $B$ that will satisfy that inequality.

Let's draw the $A \times B$ plane: on the x-axis, all possible values of $A$ and
on the y-axis, all values of $B$. So, only the green shaded area is valid:

<img src="/files/posts/triangle-problem/drawing1.png" alt="..." class='post-img' style="width:300px;" />

Because of the last inequality, if $B$ is also lower than $1/2$, then the third
segment ($z$) is larger than the sum $A+B$. This is another restriction on the
green area:

<img src="/files/posts/triangle-problem/drawing3.png" alt="..." class='post-img' style="width:300px;" />

The last condition is that the middle segment, defined between $A$ and $B$, is
not larger than the other two itself. To ensure that, $B < A + \tfrac{1}{2}$.
This last inequality defined a straight line in this plane:

<img src="/files/posts/triangle-problem/drawing3.png" alt="..." class='post-img' style="width:300px;" />

The area is clearly $1/4$ of the the admissible area, and thus this is the
probability of forming a triangle with the resulting broken segments.


## A formal solution using elementary probability

The solution above is clever, and fast -- you can explain it in less than 5 minutes.
However, this problem is a great exercise of elementary probability, and by
solving it with a more formal approach we will be able to apply the same reasoning to
solve other interesting problems (more on this below).

First, assume that we selected points A and B at random with a unfirom
distribution, i.e.,

$$ A, B \sim U(0,1) \, . $$

Of course, this assumes $A$ and $B$ independent (or $ A \perp B $), which is always
implicitly assumed. A priori, we don't know which of the points $A$ or $B$ are
closer to one end or the other. So, let's define two new variables:

$$ X = \min(A,B) $$

and

$$ Y = \max(A,B) \, . $$

Although this may seem harder, this is the reason why I got stuck in the first
place (so, thanks Brenno for the tip!). It will be easier this way.

All thee inequalities that we wrote based on the triangle inequalities can then
be re-written in the following form:

$$  2 X < 1 $$

$$  1 < 2 Y $$

$$ 2Y < 2X + 1 $$

Therefore, it is easy to define the event "form a triangle", denoted
by the symbol $\bigtriangleup$:

$$ \bigtriangleup = \big\{ (x,y) \in [0,\tfrac{1}{2}]\times[\tfrac{1}{2},1] \,\,: \,\, x < y \, , \, x < y + \tfrac{1}{2} \big\} . $$

Fine, so the probability we want to calculate is

$$ \mathbb{P} \left( \bigtriangleup \right)  = \int_{\bigtriangleup} dF , $$

where $dF$ is the joint probability density of $X$ and $Y$. There is an easy
way to evaluate it: First, for $x < y$, we have

$$ F\left( [ X \leq x , Y > y ] \right) = \mathbb{P} \left( [ A \leq x , B > y ] \cup [ B \leq x , A > y ] \right) . $$

$$  =
 \mathbb{P} \left( [ A \leq x , B > y ] \right) + \mathbb{P} \left( [ B \leq x , A > y ] \right)
$$

Naturally, for $ x \geq y $, $ F\left( [ X \leq x , Y > y ] \right) = 0 $ by definition.

Note that we chose to measure events $[X \leq x]$ and $[Y > y]$ because they are disjoint and,
thus, independent. Also, because we assumed $ A \perp B $ and using the fact that
the uniform distribution $F(X\leq x) = x$, then

$$ \mathbb{P} \left( [ A \leq x , B > y ] \right) \mathbb{1}_{x<y} =
x (1 - y) \mathbb{1}_{x<y} .$$

Therefore, the join distribution of $X$ and $Y$ is

$$ F\left( [ X \leq x , Y > y ] \right) = 2 x (1 - y) \mathbb{1}_{x<y} . $$

The joint probability density is (let me try and make it very explicit)

$$ dF = \frac{\partial}{ \partial x} \left( - \frac{\partial}{ \partial y} \right) F \, dx dy = 2 dx dy .$$

Finally, going back to the original probability:

$$ \mathbb{P} \left( \bigtriangleup \right) = \int_{\bigtriangleup} dF = \int_{\bigtriangleup} 2 dx dy $$

$$ = 2 \int\limits_{0}^{\tfrac{1}{2}} \int\limits_{x}^{x+\tfrac{1}{2}} dx dy = 2 \left. \frac{x^2}{2} \right|_{0}^{\tfrac{1}{2}} = \frac{1}{4} , $$

by Fubini's theorem.

Thus, $ \mathbb{P} \left( \bigtriangleup \right) = 1 / 4 $ is our final answer.
Note that, at the end of the day, the domain of integration of the integral we
just solved is exactly the same as the region defined in the graphic solution
above.


## Why bother with probability if there is a faster solution...?

This is always a great question: why do we bother (and why should you) with a more
formal solution to this problem, if the one above does the job?

<!-- -->
