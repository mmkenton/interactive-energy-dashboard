# 🌍 Renewable Energy Dashboard

## Interactive Dashboard
https://mkenton.shinyapps.io/STAT436_final_proj/

## Overview

Renewable energy adoption is a critical issue in today’s world, but the complexity of data surrounding it can make the topic inaccessible to non-experts. This project aims to bridge that gap by creating an interactive Shiny dashboard that simplifies and visualizes renewable energy data in a clear, engaging, and accessible way. The dashboard includes a heat map, faceted boxplots, and a density (ribbon) plot to provide a comprehensive view of how renewable energy is adopted across time and regions.

## Project Goals

- Create an interactive and educational dashboard to explore global renewable energy data.
- Reduce the complexity of technical data for broader audience accessibility.
- Encourage informed engagement with clean energy issues.

## Visualizations

### 🗺️ Global Heat Map
- Displays percentage of renewable energy use per country.
- Interactive tooltips show country name, year, and exact renewable energy usage.
- Includes an animated year slider to observe trends over time.
- Inspired by map-based visualizations from academic literature, focused on clarity and simplicity.

### 📊 Faceted Boxplots
- Show distributions of renewable electricity generation by energy type (e.g., solar, wind, hydro).
- Faceted by continent for comparative analysis.
- Helps users understand where specific types of clean energy are most common.
- Simplified and generalized alternative to more complex visualizations like the Sankey diagram.

### 📈 Density (Ribbon) Plot
- Illustrates how renewable energy consumption has changed over time across continents.
- Groups all renewable types for a broad overview.
- Designed to be digestible for new learners while still providing meaningful insights.

## Literature-Inspired Design

We grounded our visual design in best practices from academic and professional literature:

1. **Emergent Landscapes of Renewable Energy Storage** – Emphasized geographic clarity and accessibility in map-based visualizations.
2. **Lawrence Livermore National Lab’s Sankey Diagram** – Highlighted trade-offs between detail and user-friendliness.
3. **Systematic Review on Visualisation in Energy Eco-Feedback Systems** – Informed our audience-centric and context-aware approach to visualization design.

## Tools & Technologies

- **R**
- **Shiny**
- **tidyverse (dplyr, ggplot2, tidyr)**

## Key Takeaways

- Accessibility and clarity are just as important as data accuracy.
- Geographic and temporal patterns in renewable energy use can be meaningfully visualized with the right design.
- Non-expert audiences are more likely to engage when visualizations prioritize simplicity without sacrificing relevance.

## Future Directions

- Improve visual consistency and labeling across all plots.
- Add more granular filtering options for users (e.g., by energy type or region).
- Explore integrating policy or emissions data to enrich insights.

## 💡 Conclusion

Our dashboard simplifies the story of renewable energy for a broader audience, making critical data accessible and engaging. Through interactive, clean, and purposeful design, we hope to encourage wider awareness and advocacy for sustainable energy practices around the world.
