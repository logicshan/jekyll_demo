---
layout: default
title: 简单一点，容易一点!
---

{{ page.title }}
----------------

在最近的一篇文章[Simply Easy!(An Implementation of a Dependently Typed Lambda Calculus)](http://www.andres-loeh.de/LambdaPi/)中，作者们展示了type checking一个dependently typed language是多么容易。我衷心的赞同这一点，本来这就一点儿都不难。但是我不认为这篇文章展示了最简单的方法，所以接下来我就要写一个简单的dependent type checker。（这儿没有什么新鲜玩意儿，文章作者们毫无疑问熟悉用到的所有东西）。
