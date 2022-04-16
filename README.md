# Mitigating the Influence of Missing Data
## Summary:

It is important to carefully consider missing values in a data set before drawing conclusions from the results of an analysis. Conclusions based on data that are systematically missing may only apply to a small subset of the broader population the conclusions are intended to apply to. In this project, we use personality and negative affect data to demonstrate a statistical method that accounts for the cause of the missing values. We show that the conclusions drawn from the results of an analysis that does not account for the cause of the missing values are different from the results of an analysis that DOES account for the cause of the missing values.

## Application of Results:

The implication of our findings is that the presence of missing values in a data set may lead to inaccurate conclusions. Application of statistical methods for addressing missing data can improve the accuracy of conclusions drawn from the results of an analysis.

## Details of the Analysis:

* A set of questions pertaining to personality and negative affect (respectively) were used to measure personality and negative affect (respectively). Statistical methods were used to evaluate the psychometric properties of these sets of questions to ensure accuracy of the conclusions drawn from the results of our analysis. The R scripts stored above (“GreatestLowerBound.R” and “Greatest Lower Bound NegAff.R”) are for evaluating the psychometric properties (reliability via the greatest lower bound) of the personality and negative affect scales.

* Mplus was used to conduct the substantive statistical analyses, and the Mplus input and output files (“Mod5_big5&negaffCFA.inp” and “mod5_big5&negaffcfa.out”) are stored above. The output file contains the results of the substantive statistical analyses.

* A Powerpoint file summarizes the project and is stored above.
