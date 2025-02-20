### Replication materials for:

"Can we estimate crisis death tolls by subtracting total population estimates? A critical review and appraisal" by Hampton Gaddy and Maria Gargiulo (https://doi.org/10.31235/osf.io/nrpb3).

These materials can be accessed on the web page of the article (on https://www.demographic-research.org) and on the authors' GitHub repository for the project (https://github.com/thegargiulian/GRDM).

All analyses are replicable in R (version 4.2.2). The code and data files needed to reproduce the main and supplementary figures of the paper are as follows. For each data files that cannot be redistributed is noted, the access procedure used and variable metadata is described.

#### Figure 1
- "code/method-types/src/graph-methods.R"

#### Figure 2
- "code/simulation/src/simulation.R"

#### Figure 3
- "code/methods-comparison/src/graph-comparison.R"
- "data/gold-standard-comparison/comparisons.csv"
- "data/gold-standard-comparison/denominators.csv"

#### Figure 4

##### Italy
- "code/spread/src/1624-Italy.R"
- "data/spread/1624_Italy/Database_AlfaniAndPercoco.xlsx"
  - Access: Publicly available at https://guidoalfani.eu/wp-content/uploads/2023/07/Database_AlfaniAndPercoco.xlsx
  - Main variables: "City", "1624-25 (mortality rate, per thousand)", "1629-30 (mortality rate, per thousand)", "1656-57 (mortality rate, per thousand)", "pop1600", "pop1700" (56 cases total in file)

##### Amsterdam
- "code/spread/src/1871-Amsterdam.R"
- "data/spread/1871_Amsterdam/population_by_neighborhood.xlsx"
  - Access: Obtained from Tim Riswick at Radboud University on 22 July 2024
  - Main variables: "neighborhood" and "sum" (50 cases total in file)
- "data/spread/1871_Amsterdam/Smallpox_1871_Amsterdam_neighborhood.xlsx"
  - Access: Obtained from Tim Riswick at Radboud University on 22 July 2024
  - Main variables: "buurt_thuis" and "CountOftblPersoonID" (50 cases total in file)

##### England and Wales
- "code/spread/src/1918-EnglandWales.R"
- "data/spread/1918_England+Wales/1921_cr03_values.csv"
  - Access: Publicly available at https://www.nomisweb.co.uk/sources/census_1921_bulk
  - Main variables: "area", "area_type", "area_type_id", "X2c3_0002", "X2c3_0003" (20,213 cases total in file)
- "data/spread/1918_England+Wales/RGdata.xls"
  - Access: Available from https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=4350 after agreeing to the UK Data Archive's licence
  - Main variables: "Administrative unit", "Administrative County", and "Total" (335 cases total in file)

##### Japan
- "code/spread/src/1918-Japan.R"
- "data/spread/1918_Japan/japan_denominators.csv"
- "data/spread/1918_Japan/japan_excess.csv"

##### Spain
- "code/spread/src/1918-Spain.R"
- "data/spread/1918_Spain/spain_denominators.csv"
- "data/spread/1918_Spain/spain_excess.csv"

##### United States (1918)
- "code/spread/src/1918-US.R"
- "data/spread/1918_US/us_denominators.csv"
- "data/spread/1918_US/us_excess.csv"

##### Austria
- "code/spread/src/1939-Austria.R"
- "data/spread/1939_Austria/01_gem_timeline_1939-2011.dta"
  - Access: Obtained from Christopher Eder, formerly based at Johannes Kepler University Linz, on 22 July 2024
  - Main variables: "gem_id_1939", "gem_name_1939", "gem_id_2013", "gem_name_2013" (2,639 cases total in file)
- "data/spread/1939_Austria/01_tote_ww2.dta"
  - Access: Obtained from Christopher Eder, formerly based at Johannes Kepler University Linz, on 22 July 2024
  - Main variables: "gem_id_2013", "tote_ww2", "gem_name_2013" (823 cases total in file)
- "data/spread/1939_Austria/gem_hist_einwohner.dta"
  - Access: Obtained from Christopher Eder, formerly based at Johannes Kepler University Linz, on 22 July 2024
  - Main variables: "gem_id_2011", "gem_name_2011", "einwohner_1939", "einwohner_1951", "einwohner_1961" (2,353 cases total in file)

##### Netherlands
- "code/spread/src/1944-Netherlands.R"
- "data/spread/1944_Netherlands/NL-excess_mortality-1944-1945-municipalities.xlsx"
  - Access: Obtained from Peter Ekamper at the Netherlands Interdisciplinary Demographic Institute (NIDI) on 19 July 2024
  - Main variables: "exc_deaths", "pop1944", "pop1946" (1,016 cases total in file)

##### El Salvador
- "code/spread/src/1980-ElSalvador.R"
- "data/spread/1980_ElSalvador/elsalvador_denominators.csv"
- "data/spread/1980_ElSalvador/elsalvador_MSE.csv"

##### Brazil
- "code/spread/src/2020-Brazil.R"
- "data/spread/2020_Brazil/brazil_denominators.csv"
- "data/spread/2020_Brazil/brazil_excess.csv"

##### United States (2020)
- "code/spread/src/2020-US.R"
- "data/spread/2020_US/co-est2022-alldata.csv"
  - Access: Publicly available at https://www.census.gov/data/tables/time-series/demo/popest/2020s-counties-total.html
  - Main variables: "STATE", "COUNTY", "FIPSCODE", "POPESTIMATE2022" (3,195 cases total in file)
- "data/spread/2020_US/COVIDUCDMonthlyData.feather"
  - Access: Publicly available at https://osf.io/fvwqm
  - Main variables: "year", "month", "FIPSCode", "imputedCOVIDDeaths" (100,064 cases total in file)
- "data/spread/2020_US/nhgis0012_ds146_2000_county.csv"
  - Access: Available from IPUMS NHGIS (https://www.nhgis.org/) under their licence, using the "Total Population" variable from the "2000_SF1a" dataset
  - Main variables: "State.Code", "County.Code", "Data.File.Year", "Total" (3,141 cases total in file)
- "data/spread/2020_US/nhgis0012_ds172_2010_county.csv"
  - Access: Available from IPUMS NHGIS (https://www.nhgis.org/) under their licence, using the "Total Population" variable from the "2010_SF1a" dataset
  - Main variables: "State.Code", "County.Code", "Data.File.Year", "Total" (3,221 cases total in file)

##### Combine case studies
- "code/spread/src/combine-case-studies.R"

### Figure S1
- "code/hmd-experiment/src/grdm-hmd.R"
  - Access: in order to download the HMD data using this script, a user must register for an account with the HMD (https://mortality.org/Account/Auth), agreeing to the HMD licence, and then specify their username and password in the script

### Figure S2
- "code/trail-of-tears/src/cherokee.R"

### Figure S3
- "code/methods-comparison/src/graph-comparison.R"
- "data/gold-standard-comparison/comparisons.csv"
- "data/gold-standard-comparison/denominators.csv"