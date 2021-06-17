/***************************************************

Bruno Ferman

This code presents a simple example to run the inference assessment proposed by Ferman (2019), "A simple way to assess inference methods".

First version: August 10th, 2020

Update: November 7th, 2020
- As shown in a revised version of the paper, there is no need to estimate the model under the null to construct the assessment. The assessment can be constructed by simple replacing the vector of outcomes with iid standard normal varibables. 


***************************************************/


/***************************************************
You should include this part of the code with the program "assessment" at the beginning of your code.

The program has 3 inputs, and should run like:

assessment "command" "D" "other".

"command": this input includes the first part of the estimation command. For example, "reg", "xtreg", "xi: reg", and so on.

"D": covariate of interest (we want to assess inference about this variable)

"other": rest of the command (for example, "X , robust" or "X , cluster(W)") 

This command runs 1000 simulations with iid standard normal distribution for the errors. It is easy to modify the command to consider other parameters for the assessment.

you cannot have any variable in your dataset labeled "random".

This code can be easily adjusted to considered other commands that are not OLS regressions. If the command does not have the form "something" "outcome variable" "covariate of interest" "something", then the code must be modified.

This code consider a 5% test based on a t-statistic using the determined estimator for the variance. You can easily adjust the code to consider alternative inference methods.

***************************************************/

clear all
set matsize 1000


cap program drop assessment
program assessment, eclass
args command D other  

qui: mat R=J(1000,1,.)

_dots 0, title(Assessment running) reps(1000)
forvalues r=1(1)1000 {

qui: gen random = rnormal()

qui: `command' random `D' `other'
qui: mat R[`r',1]=abs(_b[`D']/_se[`D'])>1.96

qui: drop random


_dots `r' 0

}


qui {
preserve
clear 
svmat R
summ R
local summ = r(mean)
restore
}

di "Inference assessment = `summ'"

end	


/***************************************************
This part of the code simply creates a random dataset to use the program.

You should replace that with your dataset

***************************************************/

set seed 1

set obs 100 

gen Y = rnormal() // outcome variable

gen D =_n<=5 // covariate of interest (we want to assess inference about this variable)

gen X = rnormal() // other control variables


/***************************************************
Run the regression, and then the assessment 

***************************************************/

reg Y D X , robust // Regression you want to run. In this case, you want to check whether inference based on t=_b[D]/_se[D] using 1.96 as critical value is reliable.

assessment "reg" "D" "X , robust"


/***************************************************
Notes:

- If you are using another command, such as "areg", "xtreg", "xi: reg", and so on, you can just replace the first input in the program.

- The final input can be changed to include, for example, "X , cluster(Z)", "X , ab(FE)", and so on






