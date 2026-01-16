# Tautog stock assessment update (2021-2024): NJ-NY bight

I aimed to start each script with "canonical" inputs distributed by ASMFC and code every step through generating output files. An exception to this is the use of the following files/data from in-house: NJ Ocean Trawl and VAT.

## Paired structure ages
The NJ-NYB and LIS regions elected to only use opercula data. There was no otolith data for NJ-NYB prior to 2023. 

## Age-Length Key (ALK)
To determine what the length bounds should be for the ALK, you need to look at your region's raw data to find the minimum and maximum lengths. In the NJ-NYB data, lengths ranged from 4-83cm. From there, it is necessary to see where the gaps are, and look to the other datasets that will be used in the assessment to determine where the gaps need to be filled. The only gaps that need to be filled are those in the rec data. The next smallest length in the NJ-NYB dataset was 17cm. I used the American Littoral Society (ALS) and MRIP data to see what lengths were present to figure out what would be relevant for the assessment. The smallest length in the recreational fishing data was 17cm, so this set the minimum possible bound for the ALK.

An upper bound for this update was established post hoc. After subsetting the data for ages derived from opercula and both structures, I tested filling the ALK set to the maximum length (83cm) by first "filling down" the max age class (i.e. if the largest length for a given year was in 12+, I filled the rest down in age 12). Then, I used the rules established for our region for filling from rows either above and/or below. Then, I used the nearest regions (DMV and LIS) to fill remaining gaps. This left a few gaps at 63cm and above. (For reference, those lengths were not present in otolith data either.) The MARI region had an upper bound of 60cm, and so did NJ-NYB at the last benchmark. So, it seemed prudent to also cap this update at 60cm. So, I set the upper limit at 60cm and repeated the above procedure to produce filled ALKs.

From here, I am leveraging git branching to version analyses:  
* main: The parameters and data subsets used in the assessment (i.e. opercula and ages determined from both opercula and otoliths). Thus, the next smallest length in the raw data when subset for opercula was 29cm, so this set the lower bound of the ALK. The youngest fish aged by opercula was 2. This is what the waa ended up looking like:
```
  X1       X2        X3        X4       X5       X6       X7       X8       X9      X10      X11      X12
1  0      NaN 0.7763193 0.9252209 1.024419 1.371454 1.479255 1.514572 2.068825 1.796253 1.437173 2.065067
2  0 1.087465 1.1975156 1.2090998 1.264996 1.203165 1.348014 1.594575 1.708735 1.981787 1.763198 2.406508
3  0 1.258088 1.2226417 1.0547964 1.099389 1.469868 1.416375 1.847330 1.817970      NaN 2.364789 3.467907
4  0      NaN 0.6710866 1.0178610 1.127675 1.211674 1.170385 1.450400 1.504600 1.802521 2.636113 1.583045
```
* all: An experiment using all age data, no matter which structure was used to estimate the age. The youngest fish was age 1, and this allowed the length bins to go down to 17cm. This is what the waa ended up looking like:
```
         X1        X2        X3        X4       X5       X6       X7       X8       X9      X10      X11      X12
1       NaN 0.2644182 0.5030544 0.8583753 1.008225 1.357548 1.449950 1.508741 2.003082 1.777701 1.346507 2.065067
2       NaN 0.7733461 0.9344601 0.9441560 1.192450 1.203165 1.325325 1.594575 1.708735 1.981787 1.763198 2.406508
3       NaN 1.3267948 1.3687492 0.9230795 0.985230 1.406676 1.399279 1.607998 1.805251 1.220929 3.174069 3.034602
4 0.1897087 0.4496243 0.5881182 0.9636918 1.164075 1.183147 1.139561 1.308316 1.361825 1.685422 1.675412 1.514675
```
* operc: This is truly just using opercula (not "both"). This is what the waa ended up looking like:
```
  X1       X2        X3        X4       X5        X6       X7       X8       X9      X10      X11      X12
1  0      NaN 0.7763193 0.9252209 1.024419 1.3714540 1.479255 1.514572 2.068825 1.796253 1.437173 2.065067
2  0 1.087465 1.1975156 1.2090998 1.264996 1.2031647 1.348014 1.594575 1.708735 1.981787 1.763198 2.406508
3  0 1.258088 1.3383643 1.1571795 1.173510 1.5215902 1.394820 1.723266 1.750667      NaN      NaN 3.467907
4  0      NaN 0.5347857 0.8909305 1.050318 0.9829141 1.145502 1.550584 1.313462 2.526188 2.166455 2.255141
```
