import yfinance as yf
import pandas as pd
import numpy as np

# Download historical adjusted close prices
tickers = ['GOOG', 'META', 'MSFT', 'AAPL']
start_date = '2022-07-01'
end_date = '2025-07-01'

print(f"Downloading data for {tickers} from {start_date} to {end_date}...")

data = yf.download(tickers, start=start_date, end=end_date, auto_adjust=False)['Adj Close']

# Calculate daily returns
daily_returns = data.pct_change().dropna()

# Calculate mean annual returnand volatility
mean_returns = daily_returns.mean() * 252
annual_volatility = daily_returns.std() * np.sqrt(252)
correlation_matrix = daily_returns.corr()

print("\nMean Annual Returns (μ):\n", mean_returns.round(4))
print("\nAnnualized Volatility (σ):\n", annual_volatility.round(4))
print("\nCorrelation Matrix:\n", correlation_matrix.round(4))

# Export
mean_returns.to_csv("mean_returns.csv", header=["MeanAnnualReturn"])
annual_volatility.to_csv("volatility.csv", header=["AnnualizedVolatility"])
correlation_matrix.to_csv("correlation_matrix.csv")

print("\nFiles exported: mean_returns.csv, volatility.csv, correlation_matrix.csv")
