# Assignment 2: COVID
This repository contains code and analysis of COVID-19 cases in the United States, using data from [The New York Times](https://github.com/nytimes/covid-19-data/).


## Analysis Reflections
As you follow the instructions in the `analysis.R` file, you will be prompted to write six (6) _reflections_ interpreting the results of your code. Those reflections should do in this section.

Each reflection should be about 2-3 sentences long. You can organize the reflections into an _ordered list_ or otherwise label which is which.

1. Each row in each of the data sets represents a single day of observation. The features vary between the data sets. For example, the state_df includes a state and FIPS number (geographic locator code). The county_df also has these features plus the name of the county. Lastly, all data sets have a cases and deaths feature. 

2. Most of the "states" with the lowest amount of cases aren't even in the continental United States and have a different setting. This could also be due to population size and/or density.

3. They are not the same location. The highest deaths is in New York and the highest cases is in Los Angeles. This could be due to a multitude of reasons. Just to name a few: average age of citizens, number of tests being issued, how local hospitals constitute a COVID death, etc. Features must be looked at singularly as they each are not perfectly correlated with each other all the time.

4. The most basic thing that these plots tell me is that the recorded pandemic has gotten exponentionally increased over time. The new deaths plot looks much more reactive to the "waves" in the pandemic.

5. I noticed that a few of them were Native American sounding or unknown. The unknown ones were typically under 10 cases and I noticed these areas didn't have FIPS numbers. These could be very secluded or small communities.

6. What surprised me most is how quickly the numbers began to rise. The exponential growth of the cases and deaths was very shocking.
