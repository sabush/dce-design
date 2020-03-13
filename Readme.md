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

In this example, we test two designs to see their ability to estimate two main effects. Both designs will have two 2 level factors, and choice sets containing two options.   

To set this design up, we start by entering the number of levels for each factor. By default, the interface will allow for two factors, but you are able to right click on the table to add or remove rows to add or remove factors. We will also keep the number of options equal to 2, and the effects to be estimated as 'Main effects only'. The screen will look like this:

![Image1a](https://github.com/sabush/dce-design/blob/master/Images/Example1a.png)


Click on 'Enter Choice Sets' to continue. To start over, click on the 'Reset' button.

Now we enter the choice sets for the first design. The first design that we will test is the following

| Option 1 | Option 2 | 
| -------- | ---------| 
| 00       | 11       |
| 01       | 10       |

So the first choice set will compare the treatment combination with both factors at the 'low' level to the treatment combination with both factors at the 'high' level. This design is the so-called 'foldover design', which is known to be optimal.

To enter in this design, we will populate the first two rows of the choice set table, and right click to remove the third and fourth rows (as they are not needed). Once this is complete, the dashboard should look like the screenshot below.

![Image1b](https://github.com/sabush/dce-design/blob/master/Images/Example1b.png)

Now we are ready to assess the design. Click on 'Test Design'. We now see that the determinant of the information matrix is 0.0625, which has a relative D-efficiency of 100%, whcih means that this design is optimal. 

![Image1c](https://github.com/sabush/dce-design/blob/master/Images/Example1c.png)

Now we test a second design:

| Option 1 | Option 2 | 
| -------- | ---------| 
| 00       | 10       |
| 01       | 11       |
| 00       | 01       |
| 01       | 00       |

If we click 'Reset' and then 'Enter Choice Sets', we are now able to enter these choice sets, and click 'Test Design' to test the design. We obtain the below, which shows that this design has 50% relative D-efficiency.

![Image1d](https://github.com/sabush/dce-design/blob/master/Images/Example1d.png)

### Example 2: Evaluating a 2<sup>3</sup> design for main effects and selected two-factor interactions  

In this example, we look at how to evaluate a design when we are interested in estimating main effects and selected two factor interactions. Suppose that we have an experiment with three two-level factors, and want to estimate all main effects and the interactions between factors 1 and 3. 

To set this up, we need to enter in the number of levels for each factor. By default, the dashboard will have two two-level factors entered. Select a cell on the table and right click, choosing to 'Insert row below'. Now there will be three rows, which you should set equal to 2. 

We will keep two options per choice set, but need to specify the interactions that we are interested in. Select 'Main effects plus selected two factor interactions' under 'Effects to be estimated'. A new table will appear, where we enter the interactions that we wish to estimate. In our case, we want factors 1 and 3, so in the first row, make sure that the cells of the table contain 1 and 3. To add additional interaction effects, we can add rows to the table. The dashboard should now appear as below.

![Image2a](https://github.com/sabush/dce-design/blob/master/Images/Example2a.png)

Now we will enter our design. The design we are using here is the foldover design, augmented with another four choice sets that are the same as the first four, except that the levels for the first factor in the second option are flipped. The choice sers are:  

| Option 1 | Option 2 | 
| -------- | ---------| 
| 000      | 111      |
| 011      | 100      |
| 101      | 010      |
| 110      | 001      |
| 000      | 011      |
| 011      | 000      |
| 101      | 110      |
| 110      | 101      |

Entering these choice sets into the dashboard, we get the dashboard below (remember that you can add rows to the table using a right click)
![Image2b](https://github.com/sabush/dce-design/blob/master/Images/Example2b.png)

Testing the design, we see that the D-criterion value is 3.052 x 10<sup>-5</sup>, and that there si some confounding between the main effect of the second factor (second row/column of information matrix) and the interaction between factors 1 and 3 (fourth row/column of information matrix).  
![Image2c](https://github.com/sabush/dce-design/blob/master/Images/Example2c.png)

### Example 3: Constructing and evaluating a 2<sup>2</sup> x 3 design for main effects using generators

So far, we have been providing the design to test, but this dashboard will also allow you to use generators to construct a design. This follows the appraoch taken in in Street and Burgess (2007). 

![Image3a](https://github.com/sabush/dce-design/blob/master/Images/Example3a.png)


We will use the generator (111, 112) to create choice sets. The way that this works is as follows:
1. The first option consists of the complete 2<sup>2</sup> x 3 factorial design, which will have 12 runs
2. To obtain the second option of each choice set, we add 1 to the level of the first factor (modulo 2), 
add 1 to the level of the second factor (modulo 2), and 1 to the level of the third factor (modulo 3).
3. To obtain the third option of each choice set, we add 1 to the level of the first factor (modulo 2), 
add 1 to the level of the second factor (modulo 2), and 2 to the level of the third factor (modulo 3).
4. Any duplicate choice sets are then removed. 

![Image3b](https://github.com/sabush/dce-design/blob/master/Images/Example3b.png)

![Image3c](https://github.com/sabush/dce-design/blob/master/Images/Example3c.png)


## References:
D.J. Street and L.B Burgess, The Construction of Optimal Stated Choice Experiments: Theory and Methods, Wiley, 2007.