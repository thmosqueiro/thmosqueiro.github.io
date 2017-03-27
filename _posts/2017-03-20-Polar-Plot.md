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

As shown in the figure above, error bars remain orthogonal to $0^{o}$ or
$\pi^{o}$ direction, suggesting that Matplotlib is using the same code as that
of euclidian coordinates. So, first thing is to define how to properly define
the error bars in this context. In principle, error bars represent the interval
within which a considerable part of the data being displayed (usually, standard
deviation). There are two error bars that can be defined: radial and angular
error. Radial error is useful for my practical case of use, but I will also
touch the subject of an angular error for point plots.


## Radial error

For a radial error, $r \pm dr$ for a point with angle $\theta$ can be
represented by two bars located at $r + dr$ and $r - dr$. Each bar is defined in
a range $[\theta_-, \theta_+]$. However, at $r-dr$ the bar should be smaller
than that at $r+dr$. Thus, for a given point $(r_0, \theta_0)$, we will plot a
curve with fixed radius $r_0$ and angle

$$ \theta \in [ r_0 (\theta_0 + \Delta \theta), r_0 (\theta_0 - \Delta \theta)] . $$

Especially in the case of a bar plot (as in the figure at the beginning of this
post), this will ensure that this curve is orthogonal to the end of the bar.

In the context of rose plots, the free parameter $\Delta \theta$ can be defined
as a function of the bar width.


## Angular error


## Implementation

I'm sharing my current implementation below.

<script src="https://gist.github.com/thmosqueiro/1c8167d6eb308a51a2627392a038621f.js?file=polarError.py"></script>

As an example, I updated the plot from the beginning of this post and here is
the final result (find the source code
[here](https://gist.github.com/thmosqueiro/1c8167d6eb308a51a2627392a038621f.js?file=polarError.py)).

<img src="/files/posts/algorithms/RosePlot_example_fixed.png" alt="..." class='post-img' style="width:90%;" />

<img src="/files/posts/algorithms/RosePlot_Example-Points.png" alt="..." class='post-img' style="width:90%;" />
