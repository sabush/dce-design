# Discrete Choice Designer
Stephen Bush
March 2020

## Outline

This app enables users to rest the ability of a discrete choice design to estimate various effects, and to construct discrete choice designs using generators, based on the method described in Street and Burgess (2007). A live version of this dashboard can be found at: https://stephen-bush.shinyapps.io/DCEDesign/.

This dashboard is based on Mathematica code accompanying Street and Burgess (2007), written by Leonie Burgess. 

Users can choose the following options:
* The number of levels for each factor
* The number of options in each choice set
* The effects that the design will be evaluated against
.* All main effects
.* All main effects and all two factor interactions
.* All main effects and selected two factor interactions
* Either the design to be tested, or a set of generators that will be used to construct a design.

The dashboard returns the following infomation:
* The Fisher information matrix for the estimation of the effects selected
* The determinant of the above information matrix, which is the value of the D-optimality criterion for the desgin for the estimation of the selected effects
* For main effects, the relative D-efficiency of the design compared to the optimal design for the same number of levels and options per choice set. 
* If generators have been used to construct the design, the choice sets that arise from the generators.

## Examples

### Example 1: Evaluating a 2<sup>2</sup> design for main effects 

### Example 2: Evaluating a 2<sup>2</sup> design for main effects and selected two-factor interactions  

### Example 3: Constructing and evaluating a 2<sup>2</sup> x 3 design for main effects using generators

References:
D.J. Street and L.B Burgess, The Construction of Optimal Stated Choice Experiments: Theory and Methods, Wiley, 2007.