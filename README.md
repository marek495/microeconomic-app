# Microeconomic Computational App
 
<p align="center">
  <img width="1626" height="855" alt="image" src="https://github.com/user-attachments/assets/78568a83-4b0d-475c-8996-1c2c95242414" />
   <img width="1641" height="487" alt="image" src="https://github.com/user-attachments/assets/fdd75a79-74a7-4593-b44f-837a73cbbfd7" />
   <img width="1877" height="395" alt="image" src="https://github.com/user-attachments/assets/31596834-e417-4e66-b673-838a78d1dc67" />
</p>
<p align="center">
  An interactive R Shiny web application developed as part of my bachelor's thesis.
</p>
 
## Overview
 
Interactive indifference curve mapping. Engel curves. Formulas for marginal revenue and cost.

The application is a relatively advanced tool that enables the analysis of a wide range of utility and production functions, not limited to standard textbook forms such as Cobb–Douglas or Leontief functions. For example, you can also explore the following utility function:

<p align="center">
   <img width="222" height="76" alt="image" src="https://github.com/user-attachments/assets/f8e0e0a2-08a9-42a6-a9c4-04907def5e57" />
</p>
 
It is well-suited for use in **university-level teaching** or **independent self-study** of microeconomics.
 
 
## Preview
 
<img width="1680" height="851" alt="image" src="https://github.com/user-attachments/assets/25385898-8509-49c8-be46-841dfad0b578" />
<img width="1846" height="403" alt="image" src="https://github.com/user-attachments/assets/0e20b62d-bb32-42d6-97a3-06cfe08d77c6" />
<img width="1830" height="857" alt="image" src="https://github.com/user-attachments/assets/bd60c729-4de5-4ced-845e-bf38967911cd" />

 
## Features
 
The app is organised into four main sections:
 
### 1. Consumer Theory
- Interactive indifference maps (up to 4 curves)
- Analytical formulas for marginal utilities of both goods and the Marginal Rate of Substitution (MRS)
- Numerical values of marginal utilities and MRS at any given point on a curve
### 2. Consumer's Optimal Choice
- Finds the optimal consumption bundle of goods x<sub>1</sub> and x<sub>2</sub>
- Analytical formulas for optimal quantities x<sub>1</sub> and x<sub>2</sub>
- Engel curves for both goods
- Verification of Gossen's Second Law
### 3. Firm Theory
- Interactive isoquant maps (up to 4 curves)
- Maximum quantities of capital, labour, and output at given values of total cost, price of capital, and price of labour
- Marginal productivities of labour and capital, and the Marginal Rate of Technical Substitution (MRTS)
- Analytical formulas for marginal productivities and MRTS
### 4. Monopoly
- Calculation of fixed and variable costs, marginal revenue, profit, and more
- Interactive chart of the monopolist's optimal decision
- Analytical formulas for all revenue and cost types

 
## Technologies
The app was built entirely in R using <i>Shiny</i> as a web framework and <i>caracas</i> package for symbolic calculus.
 
## Thesis Context
 
This application was developed as part of my bachelor's thesis, which I **successfully** defended in June 2026 with an A grade, completing my bachelor's studies.

<b>Institution:</b> Technical University of Košice, Slovakia; Faculty of Economics

<details>
 <summary>THESIS KEYWORDS</summary>
 Preference transitivity, behavioral economics, rational decision-making, pre-test/post-test design, R Shiny application
</details>
 
<details>
<summary>ABSTRACT</summary>
The aim of this bachelor’s thesis is to examine the extent to which economic education delivered through an interactive computational tool can influence the consistency of consumer decision-making, measured by violations of preference transitivity. Transitivity is one of the fundamental axioms of rational behavior in economics, and its violation is interpreted as a deviation from the standard model of rationality. The research is based on an experimental design with a pre-test and post-test structure, in which a sample of 56 respondents was divided into an experimental group and a control group. The respondents were students of the Faculty of Economics, TUKE. The experimental group had access, between the pre-test and post-test phases, to an interactive application developed in the R programming language, focused on visualization and computation in microeconomic theory. Data were collected through a questionnaire based on choices between pairs of goods and subsequently analyzed using non-parametric statistical tests. The results show a statistically significant difference between the experimental and control groups in the change in the number of violations of preference transitivity between the pre-test and post-test. At the same time, a statistically significant change between the experimental phases was observed within the experimental group, while no such change was found in the control group. The analysis also did not identify a statistically significant effect of respondents’ gender or the highest level of parental education on changes in transitivity. The findings suggest a possible relationship between the use of an interactive educational tool and increased consistency in consumer decision-making; however, due to limitations of the experimental design, a causal relationship cannot be conclusively established. 
</details>

## Notes

Since my bachelor's thesis was originally written in Slovak, the application was also developed in Slovak. It has since been translated into English. If you notice any translation issues or errors, feel free to contact me via email.

Also, the main focus of this project was on building a functional and useful application rather than on strict code optimization. This is partly because the project was developed within an economics program, where formal software engineering standards were not the main focus. As a result, some parts of the code may not fully follow best programming practices.

## License
This project is licensed under the [MIT License](LICENSE).
