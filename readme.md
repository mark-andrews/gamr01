# Nonlinear Regression using Generalized Additive Models

This course provides a general introduction to nonlinear regression analysis
using generalized additive models. As an introduction, we begin by covering
practically and conceptually simple extensions to the general and generalized
linear models framework using polynomial regression.  We will then cover more
powerful and flexible extensions of this modelling framework by way of the
general concept of *basis functions*, which includes spline and radial basis
functions.  We then move on to the major topic of generalized additive models
(GAMs) and generalized additive mixed models (GAMMs), which can be viewed as
the generalization of all the basis function regression topics, but cover a
wider range of topic including nonlinear spatial and temporal models and
interaction models. 

## Intended Audience

This course is aimed at anyone who is interested to learn and apply nonlinear regression methods. These methods have major applications throughout the sciences.

## Teaching Format

This course will be hands-on and workshop based. Throughout each day, there will be some lecture style presentation, i.e., using slides, introducing and explaining key concepts. However, even in these cases, the topics being covered will include practical worked examples that will work through together.

The course will take place online using Zoom. On each day, the live video broadcasts will occur between (UK local time, GMT/UTC timezone) at:

* 12pm-2pm
* 3pm-5pm
* 6pm-8pm
 
All sessions will be video recorded and made available to all attendees as soon as possible, hopefully soon after each 2hr session.
 
Teaching will be done online via video link using Zoom. Although not strictly required, using a large monitor or preferably even a second monitor will make the learning experience better, as you will be able to see my RStudio and your own RStudio simultaneously. All the sessions will be recorded, and made available immediately on a private video hosting website. All materials will be shared via Git, which will allow for instantaneous sharing of code etc.

## Assumed quantitative knowledge

We assume familiarity with linear regression analysis, and the major concepts of classical inferential statistics (p-values,
hypothesis testing, confidence intervals, model comparison, etc). Some
familiarity with common generalized linear models such as logistic or Poisson
regression will also be assumed.


## Assumed computer background

R experience is desirable but not essential. Although we will be using R extensively, all the code that we use will be made available, and so attendees will just to add minor modifications to this code. Attendees should install R and RStudio on their own computers before the workshops, and have some minimal familiarity with the R environment. 

## Equipment and software requirements

A computer with a working version of R or RStudio is required. R and RStudio are both available as free and open source software for PCs, Macs, and Linux computers. R may be downloaded by following the links here https://www.r-project.org/. RStudio may be downloaded by following the links here: https://www.rstudio.com/.

Instructions on how to install all the software is [here](software.md).

# Course programme 

## Day 1

* Topic 1: *Regression modelling overview*. We begin with a brief overview and summary of regression modelling in general. The purpose of this is to provide a brief recap of general and generalized linear models, and to show how nonlinear regression fits into this very widely practiced framework.
* Topic 2: *Polynomial regression*. Polynomial regression is both a conceptually and practically simple extension of linear modelling and so provides a straightforward and simple means to perform nonlinear regression. Polynomial regression also leads naturally to the concept of basis function function regression and thus is bridge between the general or generalized linear models and nonlinear regression modelling using generalized additive models.
 * Topic 3: *Spline and basis function regression*: Nonlinear regression using splines is a powerful and flexible non-parametric or semi-parametric nonlinear regression method. It is also an example of a basis function regression method. Here, we will cover spline regression using the `splines::bs` and `splines::ns` functions that can be used with `lm`, `glm`, etc. We also look at regression using radial basis functions, which is closely related to spline regression. Understanding basis functions is vital for understanding Generalized Additive Models.

## Day 2

* Topic 4: *Generalized additive models*. We now turn to the major topic of generalized additive models (GAMs). GAMs generalize many of concepts and topics covered so far and represent a powerful and flexible framework for nonlinear modelling. In R, the `mgcv` package provides a extensive set of tools for working with GAMs. Here, we will provide an in-depth coverage of `mgcv` including choosing smooth terms, controlling overfitting and complexity, prediction, model evaluation, and so on.
* Topic 5: *Interaction nonlinear regression*: A powerful feature of GAMs is the ability to model nonlinear interactions, whether between two continuous variables, or between one continuous and one categorical variable. Amongst other things, interactions between continuous variables allow us to do spatial and spatio-temporal modelling. Interactions between categorical and continuous variables allow us to model how nonlinear relationships between a predictor and outcome change as a function of the value of different categorical variables.
* Topic 6: *Generalized additive mixed models*. GAMs can also be used in linear mixed effects, aka multilevel, models where they are known as generalized additive mixed models (GAMMs). GAMMs can also be used with the `mgcv` package.

