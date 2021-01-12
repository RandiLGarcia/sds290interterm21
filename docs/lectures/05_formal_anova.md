Formal ANOVA
========================================================
author: Prof Randi Garcia
date: January 12, 2021
autosize: true
transition: linear
transition-speed: default
font-family: 'Helvetica'


Reading Contemplation Question
========================================================

  1. Chapter 3 introduced how we will calculate the treatment effects. What do 
     we mean by "effect"?  

Announcements
========================================================

- HW2 due *right now*
- HW4 assigned - check website!
- Won't need it today, but here is the [Jamboard link](https://jamboard.google.com/d/1mmGtXITXmD908iyWsHCe3RO1TbxkuEjvavIUneLgZR4/edit?usp=sharing)

Agenda
========================================================

- Level of measurement of response variable
- Six Fisher Assumptions
- Informal ANOVA code: Assembly line metaphor

Notes on Mini Project 1
========================================================

- Giving your account an upgrade!
- Make sure your factors are *experimental*.
- Adding the "click consent" form.
- Pre-launch approval process.

What's Your Response?
========================================================
Categorical

- **nominal:** categorical variable whose levels have no ordering.
    - gender, race, etc.
- **ordinal:** categorical variable whose levels have an order. 
    - education level

What's Your Response?
========================================================
Numerical

- **interval:** numerical variable where we assume the distance between points is equal. No true zero.
    - scores on a “self-esteem” scale, measured from 1 to 7 
- **ratio:** numerical variable that has a true zero point. 
    - students’ times to complete cognitive task
    - enzyme concentration

Six Fisher Assumptions
=======================================================
type: section

Six Fisher Assumptions
=======================================================
![](04_exp_decisions-figure/CA-SINZ2.png)

***

- C. Constant effects
- A. Additive effects
- S. Same standard deviations
- I. Independent residuals
- N. Normally distributed residuals
- Z. Zero mean residuals

Parallel Dot Graphs
=======================================================

- See code **02-Informal ANOVA** from the course website
- [Link to code](https://randilgarcia.github.io/sds290interterm21/lectures/02_informal_anova.Rmd).

C. Constant effects
=======================================================

We assume every observation in a similar condition is affected exactly the same. (Gets the same true score).  

For example,


```r
animals_sim <- animals %>%
  mutate(benchmark = mean(calm)) %>%
  group_by(animal) %>%
  mutate(animal_mean = mean(calm),
         aminal_effect = animal_mean - benchmark) #every "dog" observation gets the same effect
```

A. Additive effects
=======================================================

We add the effects as we go down the assembly line. 

All effects are **added** on. 


```r
calm_sim = benchmark 
         + aminal_effect 
         + cue_effect 
         + interaction_effect 
         + student_effect 
```

S. Same standard deviations
=======================================================

The piece of code for adding error is not dependent on which condition the observations is in. Every condition gets the **same** standard deviation, here 0.65.


```r
 + rnorm(68, 0, 0.68) #rnorm(n, mean, sd = 0.65)
```

I. Independent residuals
=======================================================

Takes 68 independent draws from a normal distribution.


```r
 + rnorm(68, 0, 0.68) #rnorm function assumes independence
```

N. Normally distributed residuals
=======================================================

It's `rnorm()`, and not `rbinom()` or `rpois()`...


```r
 + rnorm(64, 0, 0.68)
```

Z. Zero mean residuals
=======================================================

The second argument is the mean.


```r
 + rnorm(64, 0, 0.68) #rnorm(n, mean = 0, sd)
```


<!-- Leafhopper survival -->
<!-- ======================================================= -->
<!-- It is reasonable to assume that the structure of a sugar molecule has something to do with its food value.  -->
<!-- An experiment was conducted to compare the effects of four sugar diets on the survival of leafhoppers. The four diets were glucose and fructose (6-carbon atoms), sucrose (12-carbon), and a control (2% agar). The experimenter prepared two dishes with each diet, divided the leafhoppers into eight groups of equal size, and then randomly assigned them to dishes. Then she counted the number of days until half the insects had died in each group. -->


<!-- Decomposing the data -->
<!-- ======================================================= -->
<!-- ```{r, echo = FALSE} -->
<!-- library(knitr) -->
<!-- library(dplyr) -->
<!-- library(tidyr) -->
<!-- library(mosaic) -->
<!-- library(ggplot2) -->

<!-- kable(data.frame(control = c(2.3,1.7), sucrose = c(3.6, 4.0), glucose = c(3.0,2.8), fructose = c(2.1,2.3))) -->
<!-- ``` -->

<!-- - Draw the factor diagram, including the benchmark and residuals. -->

<!-- Leafhoppers -->
<!-- ======================================================= -->

<!-- Bar graph of treatment condition averages.  -->

<!-- ```{r, echo=FALSE} -->
<!-- leaf <- data.frame(diet = c("control","control","sucrose", "sucrose", "glucose", "glucose", "fructose","fructose"), days = c(2.3,1.7,3.6,4.0,3.0,2.8,2.1,2.3)) -->
<!-- leaf %>% -->
<!--   group_by(diet) %>% -->
<!--   summarise(means = mean(days)) %>% -->
<!--   mutate(bench = mean(means)) %>% -->
<!-- ggplot(aes(x = diet, y = means)) + -->
<!--   geom_bar(stat = "identity") + -->
<!--   geom_hline(aes(yintercept = bench), color = "red")  -->


<!-- leaf <- leaf %>% -->
<!--   mutate(benchmark = mean(days)) %>% -->
<!--   group_by(diet) %>% -->
<!--   mutate(grp_mean = mean(days), -->
<!--          diet_effect = grp_mean - benchmark) %>% -->
<!--   ungroup() %>% -->
<!--   mutate(fitted = benchmark + diet_effect, -->
<!--          resid = days - grp_mean, -->
<!--          resid_alt = days - fitted) -->
<!-- ``` -->

<!-- *** -->

<!-- - We need to start thinking about if those differences in treatment means are real, or could possibly be due to chance error.  -->
<!-- - To your factor diagram, let's add in the benchmark, the effects for diet, and the residuals -->
<!-- ```{r, echo = FALSE} -->
<!-- kable(data.frame(' ' = c("","","means"), control = c(2.3,1.7, 2.0), sucrose = c(3.6, 4.0, 3.8), glucose = c(3.0,2.8,2.9), fructose = c(2.1,2.3,2.2))) -->
<!-- ``` -->

<!-- Analysis of Variance ANOVA -->
<!-- ======================================================= -->
<!-- Formal ANOVA starts with the simple idea that we can compare our estimate of **treatment effect variability** to our estimate of **chance error variability** to measure how large our treatment effect is.  -->

<!-- Variability in treatment effects = True Effect Differences + Error -->

<!-- Variability in residuals = Error -->

<!-- Variability in treatment effects/Variability in residuals -->

<!-- - If our null hypothesis is, ${H}_{0}$: True Effect Differences $=0$, then what would we expect the following ratio to equal? -->


<!-- Sum of Squares (SS) -->
<!-- ======================================================= -->
<!-- ANOVA measures variability in treatment effects with the sum of squares (SS) divided by the number of units of unique information (df). For the BF[1] design, -->

<!-- $${SS}_{Treatments} = n\sum_{i=1}^{a}(\bar{y}_{i.}-\bar{y}_{..})^{2}$$ -->

<!-- $${SS}_{E} = \sum_{i=1}^{a}\sum_{j=1}^{n}({y}_{ij}-\bar{y}_{i.})^{2}$$ -->

<!-- $${SS}_{Total} = {SS}_{Treatments} + {SS}_{E}$$ -->

<!-- where $n$ is the group size, and $a$ is the number of treatments. -->

<!-- Degrees of Freedom (df) -->
<!-- ======================================================= -->
<!-- The df for a table equals the number of free numbers, the number of slots in the table you can fill in before the pattern of repetitions and adding to zero tell you what the remaining numbers have to be.  -->

<!-- $${df}_{Treatments}=a-1$$ -->

<!-- $${df}_{E}=N-a$$ -->

<!-- Mean Squares (MS)  -->
<!-- ======================================================= -->
<!-- The ultimate statistic we want to calculate is Variability in treatment effects/Variability in residuals. -->

<!-- **Variability in treatment effects**: -->
<!-- $${MS}_{Treatments}=\frac{{SS}_{Treatments}}{{df}_{Treatments}}$$ -->

<!-- **Variability in residuals** -->
<!-- $${MS}_{E}=\frac{{SS}_{E}}{{df}_{E}}$$ -->


<!-- F-ratios and the F-distribution -->
<!-- ======================================================= -->
<!-- The ratio of these two MS's is called the F ratio. The following quantity is our test statistic for the null hypothesis that there are no treatment effects.  -->

<!-- $$F = \frac{{MS}_{Treatments}}{{MS}_{E}}$$ -->

<!-- If the null hypothesis is true, then F is a random variable $\sim F({df}_{Treatments}, {df}_{E})$. The [F-distribution](https://en.wikipedia.org/wiki/F-distribution). -->

<!-- ```{r, eval=FALSE} -->
<!-- qplot(x = rf(500, 3, 4), geom = "density") -->
<!-- ``` -->

<!-- We can find the p-value for our F calculation with the following code -->

<!-- ```{r, eval=FALSE} -->
<!-- pf(17.67, 3, 4, lower.tail = FALSE) -->
<!-- ``` -->

<!-- Inside-outside Factors -->
<!-- ======================================================= -->
<!-- - We cannot always use the same formula for the treatment effects. It depends on inside and outside factors -->
<!-- - Estimated effect for a factor = Average for the factor - sum of estimated effects for all outside factors. That is, -->

<!-- $$Effect = Average - Partial Fit$$ -->

<!-- - One factor is *inside* another if each group of the first (inside) fits completely inside some group of the second (outside) factor. -->

<!-- Inside-outside Factors -->
<!-- ======================================================= -->
<!-- ```{r animals-example, echo = FALSE, warning=FALSE, message=FALSE} -->
<!-- animals <- read.csv("/Users/randigarcia/Dropbox/Smith-Teaching/SDS-290/Spring2018/www/animal_data.csv", header = TRUE) -->

<!-- animals <- animals %>% -->
<!--   mutate(student = seq.int(1, nrow(animals))) %>% -->
<!--   select(student, order, animal, cute_calm, scary_calm, cute_happy, scary_happy,  -->
<!--          cute_nerv, scary_nerv, cute_scared, scary_scared) %>% -->
<!--   gather(var, value, cute_calm:scary_scared) %>% -->
<!--   separate(var, c("cue", "emotion"), sep = "_") %>% -->
<!--   spread(emotion, value) %>% -->
<!--   unite(cell, cue, animal, remove = FALSE) %>% -->
<!--   select(student, animal, cue, calm) %>% -->
<!--   group_by(animal) %>% -->
<!--   slice(1:4) %>% -->
<!--   spread(cue, calm) %>% -->
<!--   arrange(animal) -->

<!-- kable(animals) -->
<!-- ``` -->
<!-- - Draw the factor diagram as a hierarchy of inside and outside factors  -->

<!-- Group time -->
<!-- ======================================================= -->
<!-- [Project instructions](http://www.science.smith.edu/~rgarcia/sds290-S18/project.html) -->

<!-- Start HW 4 -->
<!-- ======================================================= -->
<!-- - Ch4: B1-3, C3, D1, RE CH 3: 3-4 (data in fig 3.21), 11-13, 17-19 -->