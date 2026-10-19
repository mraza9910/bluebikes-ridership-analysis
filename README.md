# BlueBikes Ridership Analysis | R & Statistical Modeling

This research investigates spatial and behavioral variations in bike-sharing patterns across Greater Boston using over 3.7 million BlueBikes trip records from 2022.

Using **R, exploratory data analysis (EDA), geospatial data processing, and degree-3 polynomial regression**, we examined how rider type (Subscribers vs. Customers), time of day, and geographic location influence trip duration.

### Key Components
- **Data Cleaning & Preprocessing:** Addressed missing and invalid ZIP codes using the Google Maps Geocoding API and USPS geographic data.
- **Exploratory Data Analysis:** Examined ridership trends, trip duration distributions, peak travel hours, and geographic differences.
- **Statistical Modeling:** Developed a cubic polynomial regression model incorporating rider-type interactions and city fixed effects.
- **Data Visualization:** Created geographic comparisons, hourly demand heatmaps, trip duration distributions, and regression prediction plots.

### Key Findings
- Casual Customers consistently took longer trips than Subscribers, suggesting distinct leisure and commuting patterns.
- Trip duration exhibited nonlinear relationships with time of day, particularly among casual riders.
- Geographic differences in trip duration highlighted opportunities for improved bike availability, station placement, and fleet rebalancing.

**Technologies:** R, Google Maps Geocoding API, USPS ZIP Code Data, Statistical Regression, Data Visualization.
