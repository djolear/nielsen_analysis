# NielsenIQ Relative Income Analysis

This repository contains code for pre-processing and analyzing NielsenIQ HomeScan data or Consumer Panel data as part of a project on the relationship between reference group income (also known as relative income) and health behaviors. 

The Consumer Panel Data comprise a nationally representative panel of households that continually provide information about their purchases in a longitudinal study in which panelists stay on as long as they continue to meet NielsenIQ's criteria. NielsenIQ consumer panelists use in-home scanners to record all of their purchases (from any outlet) intended for personal, in-home use. Consumers provide information about their households and what products they buy, as well as when and where they make purchases. 

Note that this repository contains code used to analyze data exported by scripts that are described in the [Nielsen Data Munging  repository](https://github.com/djolear/nielsen_data_munging).

### Dataset Details

**Years Available:** Starting with 2004 and including annual updates.

**Panel Size:** 40,000-60,000 active panelists (varies by year), projectable to the total United States using household projection factors.

**Panelists:** Household demographic, geographic, and product ownership variables are included, as well as select demographics for the heads of household and other members.
    - Demographic variables include household income range, size, composition, presence and age of children, marital status, type of residence, race, and Hispanic origin. Male and female heads of household also report age range, birth date, hours employed, education, and occupation. For other family members, birth date, employment, and relationship/sex are reported.
    - Geographic variables include panelist zip code, FIPS state and county codes, region (East, Central, South, West), and Scantrack Market code (assigned by NielsenIQ).
    - Product Ownership variables include kitchen appliances, TV items, and internet connection.

**Products:** All 10 NielsenIQ food and nonfood departments (~1.4 million UPC codes). These departments are dry grocery, frozen foods, dairy, deli, packaged meat, fresh produce, nonfood grocery, alcohol, general merchandise, and health and beauty aids.

**Product Characteristics:** All products include UPC code and description, brand, multipack, and size, as well as NielsenIQ codes for department, product group, and product module. Some products contain additional characteristics (e.g., flavor).

**Purchases:** Each shopping trip contains the date, retail chain code, retail channel, first three digits of store zip code, and total amount spent. For each product purchased, the UPC code, quantity, price, and any deals/coupons are recorded. Note that retailer names are not available.

**Geographies:** Entire United States, divided into 52 major markets.

**Retail Channels:** All retail channels-grocery, drug, mass merchandise, superstores, club stores, convenience, health, and others.


### Results

For a high-level description of the project and our findings, please [click here](https://github.com/djolear/gallup_rs/blob/main/results/main.md).

### Folder Structure

1. The **preprocessing** folder contains code for cleaning the data and preparing it for analyses.
2. The **analysis** folder contains scripts and markdowns showcasing the statistical analyses.
3. The **results** folder contains markdowns that describe our findings.
4. The **plots** folder contains scripts and markdowns for producing visualizations.


### Data Sources

In addition to using data from the Gallup US Daily Polls, this project uses data from several other sources.

1. [The US Census](https://www.census.gov/)
2. [County Health Rankings and Roadmaps](https://www.countyhealthrankings.org/)
3. [The Bureau of Economic Analysis](https://www.bea.gov/)