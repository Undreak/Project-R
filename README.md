# Project R

## Introduction 
The goal of this project is to make a program in R to analyse data

## Data

The data used comes from the Complex Material course, using the powder X-Ray diffraction result of three samples. 
The samples are: 
- Ni75Co25
- Ni50Co50
- Ni25Co75
Each dataset is stored in a `.dat` file with the two columns, the first one contains the 2 theta angle and the second contains the intensity values.

## Goal
The goal is to use *Experimental Design* to distinguishes the peaks from one another and to try to understand the differences between the three datasets. 

### Idea 1

One of idea is to use ANOVA to tell if two peaks are at the same value 2 theta or if they are separate, as it can be difficult to tell one from another with a graph.
So the concept is to use hypothesis testing to see if the difference is statistically significant

### Idea 2

Another idea is to use try to understand the shape of the peak, if it is Gaussian or something else and to get the values that describes the curve.
