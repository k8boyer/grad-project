Grad Project
================
Kate Boyer
4/12/2022

## No Interference Assumption: A Simulation

Like with any statistical method, some assumptions are made when
performing causal inference. One of which is the assumption that there
is no interference. This assumption is part of the Stable Unit Treatment
Value Assumption (SUTVA). The no interference assumption states that:

*“The potential outcomes for any unit do not vary with the exposures
assigned to other units.”*

This means that each unit’s potential outcomes must be consistent
regardless of the exposures of the other units. For example, if my
potential outcomes are that I am happy when I win money and sad when I
don’t, I must still be just as happy winning money if everyone around me
does not win any.

One setting where the no interference assumption is easily violated if
study investigators are not careful is vaccine trials.

## An Example:

Following is a simulation of this setting to illustrate the consequences
of violating this assumption, and an alternate way to design vaccine
trials to avoid doing so.

Here we create 5 hospitals, each with 100 participants who are taking
part in the trial, and we randomize each participant to have a 50%
chance of receiving the vaccine and a 50% chance of receiving a placebo.

The outcome of interest is whether you get sick or not. To get sick, a
participant must first be exposed to the virus, then they get sick with
100% chance if they are not vaccinated, and with 0% chance if they are
vaccinated.

We make sure the chance of being exposed to the virus is lower in the
hospitals with higher vaccination rates by setting it equal to 1-the
vaccination rate.

``` r
observations=tibble(
  id = 1:500,
  hospital = rep(c(1,2,3,4,5), each = 100),
  vaccinated = rbinom(500, 1, 0.5)
)

observations%>%group_by(hospital) %>%
  summarise(exposure_chance = 1 - mean(vaccinated))->e
```

Those in hospitals with higher vaccination rates will have more people
protected by the vaccine, so the virus will have fewer chances to
spread, and this will increase protection for everyone in the hospital
rather than just those who are vaccinated.

Next, we create the potential outcomes for each participant. For the
purposes of this example, we are assuming the vaccine performs
perfectly. So, the potential outcomes for each participant are that they
do not get sick if they are vaccinated, but if they are not vaccinated,
they do get sick with probability that they were exposed to the virus.

``` r
observations=observations %>%
  left_join(e) %>%
  mutate(y0=rbinom(500, 1, exposure_chance), ##if you are not vaccinated, you get sick if you are infected. so you get sick with probability that you are infected 
         y1=0, ##if you are vaccinated, never get sick
         yobs=ifelse(vaccinated, y1, y0))
```

    ## Joining, by = "hospital"

``` r
observations%>%
  group_by(vaccinated)%>%
  summarise(outcome=mean(yobs))
```

    ## # A tibble: 2 × 2
    ##   vaccinated outcome
    ##        <int>   <dbl>
    ## 1          0   0.486
    ## 2          1   0

Here we can see that the difference in outcomes is less than 1/2, when
we know the true causal effect is 1 since you get sick if you are not
vaccinated and you do not get sick if you are. The violation to the
interference assumption is hiding much of the effect of the vaccine and
making it look less protective than it actually is.

## How to Fix it:

We can solve this problem by redefining our unit from the individual to
the cluster. We will now simulate a randomization by hospital rather
than by individual to show that the full effect of the vaccine is
observed.

``` r
corrected = tibble(
  hospital = rep(c(1,2,3,4,5), each = 1),
  vaccinated = rbinom(5, 1, 0.5),
  exposure_chance=1-vaccinated
)
```

We still make the exposure chance equal to one minus the vaccination
rate, but now that we are randomizing by hospital, each hospital has
either 100% or 0% vaccination rate, so the exposure probabilities are
either 0 or 1.

``` r
corrected%>%group_by(hospital) %>%
  summarise(exposure_chance = 1 - mean(vaccinated))
```

    ## # A tibble: 5 × 2
    ##   hospital exposure_chance
    ##      <dbl>           <dbl>
    ## 1        1               0
    ## 2        2               0
    ## 3        3               0
    ## 4        4               0
    ## 5        5               0

Let’s look at the effect of the vaccine after randomizing by hospital.

``` r
observations2=tibble(
  id = 1:500,
  hospital = rep(c(1,2,3,4,5), each = 100),
)
observations2 %>% left_join(corrected)%>%
  mutate(y0=rbinom(500, 1, exposure_chance), ##if you are not vaccinated, you get sick if you are infected. so you get sick with probability that you are infected 
         y1=0, ##if you are vaccinated, never get sick
         yobs=ifelse(vaccinated, y1, y0))%>%
  group_by(vaccinated) %>%
  summarise(outcome=mean(yobs))
```

    ## Joining, by = "hospital"

    ## # A tibble: 1 × 2
    ##   vaccinated outcome
    ##        <int>   <dbl>
    ## 1          1       0

Now that we have met the assumption of no interference by randomizing by
the hospital rather than by the individual, the whole effect of the
vaccine is observed when looking at the outcome among those not
vaccinated (1) - among those vaccinated (0) = 1.
