# PloidyModifier

### The ASCAT.sc Ploidy Modifier allows users to manually refit the profiles generated by ASCAT.sc for a more accurate ploidy fitting

### 1. Loading the ASCAT.sc object

The ASCAT.sc rdata object must first be loaded, then the profile of each sample in the dataset can be visualized and modified. Please note that large datasets take longer to load.

### 2. Tools to refit the profiles

Different tools exist for different ways of refitting the profile. The original profile, along with its ploidy/purity sunrise plot, is always shown on the "Orignal profile" station. Each profile modification will be visible on the Working Station Profile once clicking on "Apply". 

#### 2.1 Modifying the purity/ploidy of the samples

The values of the ploidy and purity of each sample can be changed using the sunrise plot on the Working Station, by clicking on the point corresponding to the desired purity/ploidy values ("Modify purity & ploidy"). This is the recommended way to modify a profile, since all the purity/ploidy values on the sunrise plot will result in a valid profile.

#### 2.2 Other modifications

Additional methods for refitting are: shifting the whole sample ploidy by a certain value ("Shift sample ploidy"/"Shift ploidy on graph"), or modifying the copy number of two different segments and refitting the profile based on them ("Refit segments"/"Refit segments on graph"). The two methods can be applied either by numeric input ("Shift sample ploidy"/"Refit segments") or by clicking directly on the Working Station profile ("Shift ploidy on graph"/"Refit segments on graph"). Due to the permissiveness concerning the input values, these methods can sometimes result in impossible purity/ploidy values, and thus profiles that aren't valid.

### 3. Saving the profiles

The original profiles are always kept, along with the modified ones, so each sample can be reverted back to the original. The (modified) profiles of all the samples can be downloaded in text format. For the unmodfied samples, their original profile will be downloaded. The entire ASCAT.sc rdata object can also be downloaded, with the new profiles/solutions stored in a new attribute of the object. The new, modified rdata object can then also be used as starting point for additional modifications by loading it (instead of the original object) into the application.
