# Assignment 2
This repository presents a Monte Carlo simulation to explore portfolio allocation strategies involving four technology stocks: GOOG, META, MSFT, and AAPL. The goal is to visualize the opportunity set under different investment strategies: long-only vs. long-and-short positions.
## How to Run This Project  
### Requirements  
R (version 4.1 or higher)  
R packages: MASS, ggplot2
### Execution  
Step 0 - Fill in the parameters if they are different.  
Step 1 - Run the script in code/Assignment2_Monte_Carlo.Rmd
### Changing Parameters
Parameters for the chosen four stocks were pre-filled, but you can try out different combinations. Change if you decide to explore a different set of assets.
A script to scrape the adjusted close price of defined tickers and period is provided in code/data_scraping_and_parameters.py  
Change 'tickers', 'start_date', and 'end_date' values if you wish to use a different set of tickers. The script will output three csv files with the parameters (mean returns, volatility, and correlation matrix) that need to be filled into the R script for the correct simulation.
### TBA  
research report to be added.  

## Source jump-start code and technical report
The jump-start code and technical report are provided by Professor Thomas W. Miller, the instructor for this course. 
