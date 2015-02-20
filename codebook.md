---
title: "codebook.md"
output: html_document
---
### Variables


- data.x - Measurement data. Combined data set of the two above variables
- data.subject - Subjects. Combined data set of the two above variables
- data.y - Data Labels. Combined data set of the two above variables. 
- data.features - table contents of `features.txt`
- features - Names of for data columns derived from featuresList
- keepColumns - logical vector of which features to use in tidy data set
- activities - table contents of `activity_labels.txt`. Human readable
- tidyData - subsetted, human-readable data ready for output according to
  project description.
- uS - unique subjects from data.subject
- nS - number of unique subjects
- nA - number of activities
- nC - number of columns in tidyData
- td - second tiny data set with average of each variable for each activity and
  subject

### Output

#### tidyData.txt

`tidyData.txt` is a 10299x68 data frame.

- The first column contains subject IDs.
- The second column contains activity names.
- The last 66 columns are measurements.
- Subject IDs are integers between 1 and 30.

#### tidyData2.txt

`tidyData2.txt` is a 180x68 data frame.

- The first column contains subject IDs.
- The second column contains activity names.
- The averages for each of the 66 attributes are in columns 3-68.