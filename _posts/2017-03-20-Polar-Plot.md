---
layout: posts
title: Matplotlib and error bars in polar coordinates
tags: Matplotlib Plots NotReady
mathjax: true
comments: true
---

![](/files/posts/algorithms/RosePlot_example_snapshot.png){: .post-img-inset}

<!-- Split Here - IMG-snapshot -->

I recently needed to construct a rose plot - or a bar plot in polar coordinates.
Matplotlib creates beautiful plots in polar coordinates by simply using the
statement *projection='polar'*. However, when I tried to add error bars, bars
were completely tilted and with different angles depending on the inclination.
To solve it, I wrote a small function to draw the lines appropriately -- at
least according to my expectations and understanding of how error bars should
look like in polar plots.

<!-- Split Here - Snapshot -->

<img src="/files/posts/algorithms/RosePlot_example.png" alt="..." class='post-img' style="width:90%;" />

As shown in the figure above, error bars remain orthogonal to $0$ or
$\pi$ radians direction, suggesting that Matplotlib is using the same code as that
of euclidian coordinates. So, first thing is to define how to properly define
the error bars in this context. In principle, error bars represent the interval
within which a considerable part of the data being displayed (usually, standard
deviation). There are two error bars that can be defined: radial and angular
error. Radial error is useful for my practical case of use, but I will also
touch the subject of an angular error for point plots.


## Radial error

To represent radial error of the point $( r \pm dr , \theta )$ we will draw by
two bars located at $r + dr$ and $r - dr$. Contrary to error bars in euclidean
coordinates, each bar should have different lengths: the bar at $r-dr$ should be
smaller than that at $r+dr$. This difference is naturally achieved in a polar
plot by defining the length of the interval as $2 \Delta \theta$. Thus, for a
given point $(r_0, \theta_0)$, we will plot a curve with fixed radius $r_0 \pm
dr$ and angle $ \theta \in [ \theta_0 + \Delta, \theta_0 - \Delta \theta ] . $

Especially in the case of a bar plot (as in the figure at the beginning of this
post), this will ensure that this curve is orthogonal to the end of the bar.


<img src="/files/posts/algorithms/plot_bothoptions.png" alt="..." class='post-img' style="width:90%;" />


## Angular error


## Implementation

Here is the current implementation.

<script src="https://gist.github.com/thmosqueiro/1c8167d6eb308a51a2627392a038621f.js?file=polarError.py"></script>

As an example, I updated the plot from the beginning of this post and here is
the final result (find the source code
[here](https://gist.github.com/thmosqueiro/1c8167d6eb308a51a2627392a038621f.js?file=polarError.py)).
In the context of rose plots, the free parameter $\Delta \theta$ can be defined
as a function of the bar width.

<img src="/files/posts/algorithms/RosePlot_example_fixed.png" alt="..." class='post-img' style="width:90%;" />
