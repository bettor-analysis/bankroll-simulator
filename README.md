# Bankroll Management Simulator

**Type:** Shiny Web App  
**Tags:** Sports Betting, Expected Value, Kelly Criterion, Risk Management, Simulation

---

## Overview

The Bankroll Management Simulator is a web-based Shiny app designed to help sports bettors simulate the long-term impact of various bet sizing strategies. Whether you're managing a personal bankroll or analyzing staking systems for professional betting, this tool gives a visual and statistical look at how variance, edge (EV), and odds ranges affect outcomes over time.

---

## Features

- Simulate bankroll trajectories using:
  - Flat bet sizing
  - Kelly Criterion (custom fraction)
  - Percentage of bankroll risk
- Input a custom expected value (EV) distribution and odds range
- Toggle visualization options:
  - Show or hide mean and median trajectory lines
  - Adjust the number of simulations plotted (from 10 to 1000)
- View final bankroll distribution
- Output key statistics including:
  - Median and mean final bankroll
  - Percent of simulations ending in ruin
  - Drawdowns and volatility
- Export results to CSV

---

## How to Use

1. Set your parameters in the left-hand sidebar:
   - Bankroll, number of bets, EV range, and odds range (entered in American odds)
   - Choose a betting strategy (Flat, Kelly, or % Risk)
2. Click "Run Simulation"
3. Use the slider below the chart to adjust how many bankroll lines are shown
4. Review:
   - Bankroll trajectories over time
   - Final bankroll histogram
   - Simulation summary statistics
5. Export the results using "Download CSV"

---

## Use Case Example

You're a bettor looking for +EV wagers in the range of -200 to +200. You usually bet at +120 and believe your edge is somewhere between 1% and 10%. You're curious to know:

- How aggressive should your Kelly fraction be?
- What’s the risk of ruin over 1000 bets?
- How does volatility compare between strategies?

This simulator helps answer those questions before you put money on the line.

---

## Requirements

This app uses the following R libraries:

```r
shiny
ggplot2
mc2d
scales
shinyWidgets
shinythemes
reshape2
```

You can install them using:

```r
install.packages(c("shiny", "ggplot2", "mc2d", "scales", "shinyWidgets", "shinythemes", "reshape2"))
```

---

## Deployment

To run locally:

```r
library(shiny)
runApp("path/to/your/app")
```












