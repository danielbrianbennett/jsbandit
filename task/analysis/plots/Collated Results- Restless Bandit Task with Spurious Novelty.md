# Collated Results: Restless Bandit Task with Spurious Novelty

### Overview of experiments
Tasks were restless 4-armed bandit tasks with spurious novelty, operationalised as a salient visual change to one stimulus that did not signal any change in the generative process underlying payouts. MTurk participants completed 3 blocks of 30 trials each. The nature of the visual change differed between experiments:

*Experiment 1*: The fill colour of one circle changed. The changed colour was always the same (dark grey).

*Experiment 2*: The fill colour of one circle changed. The changed colour was different in each block (one of yellow, royal blue, or dark grey), and the order of colours was counterbalanced across participants.

*Experiment 3*: A 'semantic tag' was attached to one circle. This semantic tag was an arrow pointing to the circle from a text box that read either "good", "bad", or "???". Participants were randomly assigned to one of these tags in a between-participants design. Each individual participant saw the same circle in all three blocks of the task. 

## Results: Experiment 1
Participants showed an initial bump for the novel option, which then decayed across time both within a block and across blocks.

![Experiment 1 overview](/Users/danielbennett/Desktop/Exp2.png)
*Figure 1.* Novel stimulus choice proportion as a function of trial number ('lag') relative to the visual change and block number in Experiment 1. 

I quanitified these effects using a logistic regression:

```
glm.fit <- glm(filledChosen ~ block + changeLag + block:changeLag,
               data = eligible.data,
               family = binomial(link = "logit"))
```

This produced the coefficient estimates below:

```
Coefficients:
                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -0.704335   0.159031  -4.429 9.47e-06 ***
block           -0.406834   0.078740  -5.167 2.38e-07 ***
changeLag       -0.048782   0.016658  -2.928  0.00341 ** 
block:changeLag  0.018590   0.008202   2.267  0.02342 *  

```

These coefficients seem to be consistent with the explanation above: decreases in choice proportion with increasing  a rather stronger effect of block than of trial. NB: Note though that this interpretation might change somewhat depending on what trials are included for analysis. The logistic regression above is for **all trials** following the perceptual change, which includes trials as much as 20 trials distant. That seems liked the least prejudicial approach, but I'm very happy to try something else out if you're interested.

## Results: Experiment 2

As in Experiment 1, participants showed an initial bump for the novel option, which then decayed across time both within a block and across blocks.

![Experiment 2 overview](/Users/danielbennett/Desktop/Exp3.png)
*Figure 2.* Novel stimulus choice proportion as a function of trial number ('lag') relative to the visual change and block number in Experiment 2. 

I quanitified these effects using a logistic regression:

```
glm.fit <- glm(filledChosen ~ block + changeLag + fillColour + block:changeLag + fillColour:block,
               data = eligible.data,
               family = binomial(link = "logit"))
```

This produced the coefficient estimates below:

```
Coefficients:
                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)             -0.914301   0.216259  -4.228 2.36e-05 ***
block                   -0.323490   0.101586  -3.184  0.00145 ** 
changeLag               -0.068648   0.016559  -4.146 3.39e-05 ***
fillColour#2F4F4F        0.999783   0.226554   4.413 1.02e-05 ***
fillColour#FFFF00        0.531128   0.218496   2.431  0.01506 *  
block:changeLag          0.032226   0.007887   4.086 4.39e-05 ***
block:fillColour#2F4F4F -0.518693   0.109594  -4.733 2.21e-06 ***
block:fillColour#FFFF00 -0.199459   0.101947  -1.957  0.05041 .  
```
To my eye, there are two somewhat interesting results to fall out of this:

- That there is not an especially pronounced effect of varying the colour of the novel option across blocks relative to Experiment 1.
- That there appears to be a main effect of the change colour even when the effect of block number is taken into account. That seems to imply that different colours have different effects on novelty-seeking no matter where they occur in a sequence. You can see that in Figure 3 below, which shows the average effect of a change to each of the three different colours marginalised across block number.  

![Experiment 2 overview by colour](/Users/danielbennett/Desktop/Exp3_byColour.png)
*Figure 3.* Novel stimulus choice proportion as a function of trial number ('lag') relative to the visual change and fill colour. 

I initially wondered whether this might be to do with a mismatch of different colours appearing in different blocks (more yellow in block 1 by chance, say), but that doesn't seem to be the case:

| Block  | Grey   | Blue | Yellow |
|:-----: |:------:|:----:|:------:|
| 1      | 51     | 36   | 58     |
| 2      | 48     | 59   | 38     |
| 3      | 47     | 50   | 49     |
*Table 1.* Cross-tabulation of novel change colour by block number across participants.

And we can verify this by plotting the choice proportions in block 1 alone:
![Experiment 2 overview by colour](/Users/danielbennett/Desktop/Exp3_byColour_block1.png)
*Figure 4.* Novel stimulus choice proportion as a function of trial number ('lag') relative to the visual change and fill colour for block 1 only. 

Interestingly, block 1 seems to be the only place where the difference between colours is at all pronounced. That seems psychologically relevant -- perhaps yellow was a more salient change in the first instance?


## Results: Experiment 3
Finally, the same general pattern emerges in experiment 3, but this time the preference for observing the novel option is increased overall:

![Experiment 3 overview](/Users/danielbennett/Desktop/Exp4.png)
*Figure 5.* Novel stimulus choice proportion as a function of trial number ('lag') relative to the visual change and block number in Experiment 3. 

Once again, I used a logistic regression to assess these patterns:

```
glm.fit <- glm(filledChosen ~ block + changeLag + tagText + block:changeLag + tagText:block,
               data = eligible.data,
               family = binomial(link = "logit"))
```

This produced the coefficient estimates below:

```
Coefficients:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)        0.286318   0.188795   1.517  0.12938    
block             -0.547686   0.089963  -6.088 1.14e-09 ***
changeLag         -0.091110   0.016952  -5.374 7.68e-08 ***
tagTextbad        -2.262386   0.259784  -8.709  < 2e-16 ***
tagTextgood       -0.032143   0.190854  -0.168  0.86625    
block:changeLag    0.042351   0.007923   5.345 9.03e-08 ***
block:tagTextbad   0.318076   0.117828   2.699  0.00694 ** 
block:tagTextgood -0.218310   0.092922  -2.349  0.01880 *  
```

And as you would expect, performance differed markedly depending on the content of the semantic tag:

![Experiment 3 overview by tag](/Users/danielbennett/Desktop/Exp4_byTag.png)
*Figure 6.* Novel stimulus choice proportion as a function of trial number ('lag') relative to the visual change and the meaning of the tag text. 

Somewhat nicely (given our hypotheses), the '???' tag produced just as strong an effect on behaviour as the 'good' tag.