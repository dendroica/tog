# Tautog stock assessment update (2021-2024): NJ-NY bight

I aimed to start each script with "canonical" inputs distributed by ASMFC and code every step through generating output files. An exception to this is the use of the following files/data from in-house: NJ Ocean Trawl and VAT.

## Paired structure ages
The NJ-NYB and LIS regions elected to only use opercula data. There was no otolith data for NJ-NYB prior to 2023. 

## Age-Length Key (ALK)
To determine what the length bounds should be for the ALK, you need to look at your region's raw data to find the minimum and maximum lengths. In the NJ-NYB data, lengths ranged from 4-83cm. From there, it is necessary to see where the gaps are, and look to the other datasets that will be used in the assessment to determine where the gaps need to be filled. The only gaps that need to be filled are those in the rec data. The next smallest length in the NJ-NYB dataset was 17cm. I used the American Littoral Society (ALS) and MRIP data to see what lengths were present to figure out what would be relevant for the assessment. The smallest length in the recreational fishing data was 17cm, so this set the minimum possible bound for the ALK.

An upper bound for this update was established post hoc. After subsetting the data for ages derived from opercula and both structures, I tested filling the ALK set to the maximum length (83cm) by first "filling down" the max age class (i.e. if the largest length for a given year was in 12+, I filled the rest down in age 12). Then, I used the rules established for our region for filling from rows either above and/or below. Then, I used the nearest regions (DMV and LIS) to fill remaining gaps. This left a few gaps at 63cm and above. (For reference, those lengths were not present in otolith data either.) The MARI region had an upper bound of 60cm, and so did NJ-NYB at the last benchmark. So, it seemed prudent to also cap this update at 60cm.

From here, I am leveraging git branching to version analyses:  
* main: The parameters and data subsets used in the assessment (i.e. opercula and ages determined from both opercula and otoliths). Thus, the next smallest length in the raw data when subset for opercula was 29cm, so this set the lower bound of the ALK. The youngest fish aged by opercula was 2.  
* all: An experiment using all age data, no matter which structure was used to estimate the age. The youngest fish was age 1, and this allowed the length bins to go down to 17cm.
