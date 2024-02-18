# Project R

## Introduction 
The goal of this project is to make a program in R to analyse data

## Data

The data used comes from the Complex Material course, using the powder X-Ray diffraction result of three samples. 
The samples are: 

- Ni75Co25
- Ni50Co50
- Ni25Co75
- Co

Each dataset is stored in a `.dat` file with the two columns, the first one contains the 2 theta angle and the second contains the intensity values.

## Goal
The goal is to use *Experimental Design* to distinguishes the peaks from one another and to try to understand the differences between the three datasets. 

### Idea 1

One idea is to use ANOVA to tell if two peaks are at the same value 2 theta or if they are separate, as it can be difficult to tell one from another with a graph.
So the concept is to use hypothesis testing to see if the difference is statistically significant

### Idea 2

Another idea is to use try to understand the shape of the peak, if it is Gaussian or something else and to get the values that describes the curve.

## Experimental Design

### Objectives

Identify if we have a change in structure of the perovskite induced by the change in composition.
The objective is to study the cause an effect relationship between the composition and the structure.
The effect is small but should be noticable with powder X-ray diffraction.

### Experimental Units

We expect the 2 theta position of the peak to be shifted depending on the composition and also less significantly the intensity of the peak. 

### Measurable Variables

The angle 2 theta and the instensity of the peak for each composition.

### Independent Variables
The shifting of the position of the peak may be influenced by its 2 theta position.
