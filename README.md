# Advanced Time Series & Volatility Forecasting
*A Statistical Analysis of NVIDIA and Amplifon Market Data*

### Project Overview
This repository contains a rigorous end-to-end statistical analysis of financial time series (NVIDIA and Amplifon). While the dataset belongs to the financial domain, the core objective of this project is to showcase a robust pipeline for **Time Series Analysis**, **Stationarity Testing**, and **Volatility Forecasting** using advanced econometric models. 

The methodologies applied here—handling volatility clustering, non-normal distributions, and dynamic forecasting—are fully translatable to any industry dealing with complex, noisy sequential data (e.g., demand forecasting, IoT sensor data, or energy consumption).

---

### Analytical Workflow

The analysis is structured into three main phases for each asset (Daily/Monthly):

1. **Exploratory Data Analysis & Stationarity:** - Investigated asset prices and log-returns for skewness and leptokurtosis.
   - Tested for Random Walk hypothesis and unit roots using **Augmented Dickey-Fuller (ADF)** and **Ljung-Box** tests.
2. **Mean Forecasting (ARMA):** - Modeled the linear dependence of returns using ARMA processes.
   - Conducted both static and dynamic forecasting, proving that simple mean models often outperform complex linear structures in highly efficient markets.
3. **Volatility Modeling (GARCH Family):** - Detected ARCH effects and volatility clustering using Rolling Variance and LM tests.
   - Fit and evaluated multiple volatility models: **GARCH(1,1)**, **E-GARCH**, and **GJR-GARCH** to capture asymmetric responses to market shocks.
   - Addressed the non-normality of residuals by comparing Normal, Student-t, and Skewed Student-t (sstd) distributions.

---

### Tech Stack & Methodology

| Category | Tools/Concepts |
| :--- | :--- |
| **Language** | R (with logic easily adaptable to Python's `statsmodels` & `arch`) |
| **Data Manipulation** | `tidyverse`, `xts`, `quantmod` |
| **Econometrics & Stats** | `rugarch`, `tseries`, `forecast`, `fUnitRoots` |
| **Validation Methods** | Information Criteria (AIC, BIC), **Diebold-Mariano Test** (for predictive accuracy comparison) |

---

### Key Findings
* **Tails matter:** Standard normal distributions consistently failed to capture the extreme market movements. Implementing Skewed Student-t (sstd) distributions drastically improved the models' goodness-of-fit.
* **Asymmetric Shocks:** For assets like Amplifon, models that account for asymmetric volatility (like EGARCH) significantly outperformed standard GARCH models, correctly capturing how negative shocks impact variance differently than positive ones.
* **Forecast Evaluation:** Used the Diebold-Mariano test to rigorously prove statistical significance between competing forecasts (MSE/RMSE), rather than relying on visual approximations.

---

### How to Read this Project
To make this research easily accessible without needing to download raw datasets or execute local environments:

**Please view the `Finance_in_R.md` file in this repository.** The raw source code is available in the `.Rmd` file.

---
