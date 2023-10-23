
Students Name:           Students ID :              Section:

Monerah almobarak        442002988                  91S

Nada Alotaibi            442003374                  91S

Sarah Altaweel           442000786                  91S
 
Sarah Aljuhani           442005104                  91S



```{r}
library(tidyverse)
```



```{r}
Data<-read.csv("C:\\Users\\moner\\Downloads\\Faculty_Data.csv")
Data
```

```{r}
view(Data)
```




# 1.Provide a brief description of the dataset (population, observations, variables' types).


```{r}
#str() function: This function provides a compact way to display the structure of an R object, including its type, dimensions, and contents.

str(Data)

```
 it is a data frame withe '403' observations and '4 'variables.



```{r}
#Using the summary() function: This function provides a summary of the main characteristics of the variables in a dataset, such as minimum and maximum values, mean, median, and quartiles.

summary(Data)

```


```{r}

#Using the head() function: This function displays the first few rows of a dataset.

head(Data)
```


```{r}
#Using the dim() function: This function provides the dimensions of a dataset, which can be useful for determining how many rows and columns the data has.

dim(Data)
```

```{r}
#Using the names() function: This function displays the names of the variables in a dataset, which can be useful for identifying which columns contain which information.

names(Data)
```


# 2.Consider the quality factors and provide a quality report on the raw data.

We considered six quality factors:
1- uniqueness
2- completeness
3- validity
4- consistency
5- relevancy
6- timeliness

```{r}
#This code targets the quality dimension of uniqueness by evaluating whether there are any duplicate entries in the dataset. This is crucial because duplicates directly affect the distinctiveness and uniqueness of individual data instances within the dataset.

# Check for duplicates
has_duplicates <- duplicated(Data)

# Print the result
if (any(has_duplicates)) {
  print("The dataset has duplicates.")
} else {
  print("The dataset does not have duplicates.")
}
# Print the number of rows before removing duplicates
print(paste("Number of rows before removing duplicates:", nrow(Data)))


 #After running the code, it becomes clear that the dataset contains duplicate entries, indicating a lack of complete uniqueness. To ensure full uniqueness, additional steps such as thorough data cleaning may be necessary.
```
```{r}
# Check for duplicates in the ID column
has_duplicates_ID <- duplicated(Data$ID)

# Print the result
if (any(has_duplicates_ID)) {
  print("The dataset contains duplicate IDs.")
} else {
  print("The dataset does not have any duplicate IDs.")
}
# Print the number of rows before removing duplicate ID
print(paste("Number of rows before removing duplicate ID:", nrow(Data)))
```

```{r}
#This code targets the data quality dimension of completeness by evaluating whether the dataset contains any missing values.

# Check for missing values
has_missing_values <- sum(is.na(Data))

# Print the result
if (has_missing_values > 0) {
print(paste("The dataset contains", has_missing_values, "missing value(s)."))
} else {
  print("The dataset does not have any missing values.")
}


#After running the code, it becomes apparent that the dataset contains missing values. This suggests the presence of incomplete data, requiring further data cleaning.
 
```

```{r}
# This code focuses on the quality dimension of data validity, specifically addressing outliers.
# By utilizing a box plot, it visually identifies potential outliers in the dataset.
# The Tukey's fences method is then applied to define the range within which values are considered typical, allowing for the detection and handling of outliers that significantly deviate from the norm.


# Check for outliers in Salary using box plot
boxplot(Data$Salary)

# Calculate outliers in Salary using the Tukey's fences method
Q1_Salary <- quantile(Data$Salary, 0.25, na.rm = TRUE)  # Calculate the 1st quartile (Q1)
Q3_Salary <- quantile(Data$Salary, 0.75, na.rm = TRUE)  # Calculate the 3rd quartile (Q3)
IQR_Salary <- Q3_Salary - Q1_Salary  # Calculate the interquartile range (IQR)
lower_fence_Salary <- Q1_Salary - 1.5 * IQR_Salary  # Calculate the lower fence
upper_fence_Salary <- Q3_Salary + 1.5 * IQR_Salary  # Calculate the upper fence

# Identify outliers in Salary
outliers_Salary <- Data$Salary < lower_fence_Salary | Data$Salary > upper_fence_Salary

# Check for outliers in Experience using box plot
boxplot(Data$Experience)

# Calculate outliers in Experience using the Tukey's fences method
Q1_Experience <- quantile(Data$Experience, 0.25, na.rm = TRUE)  # Calculate the 1st quartile (Q1)
Q3_Experience <- quantile(Data$Experience, 0.75, na.rm = TRUE)  # Calculate the 3rd quartile (Q3)
IQR_Experience <- Q3_Experience - Q1_Experience  # Calculate the interquartile range (IQR)
lower_fence_Experience <- Q1_Experience - 1.5 * IQR_Experience  # Calculate the lower fence
upper_fence_Experience <- Q3_Experience + 1.5 * IQR_Experience  # Calculate the upper fence

# Identify outliers in Experience
outliers_Experience <- Data$Experience < lower_fence_Experience | Data$Experience > upper_fence_Experience

# Print the result for Salary
if (any(outliers_Salary)) {
  print("The Salary attribute contains outliers.")
} else {
  print("The Salary attribute does not have any outliers.")
}

# Print the result for Experience
if (any(outliers_Experience)) {
  print("The Experience attribute contains outliers.")
} else {
  print("The Experience attribute does not have any outliers.")
}

# Upon running the code, it becomes apparent that there are outliers present in the Salary attribute,
# Similarly, the Experience attribute exhibits outliers,
# which may raise concerns about the data's validity due to the presence of significantly deviating data points.

#Although outliers have been identified in the Salary data, it is important to consider their significance in the context of analyzing inequities. Removing these outliers may not be the most appropriate course of action as they can provide valuable insights into potential wage gaps or discriminatory practices within the dataset. These outliers represent extreme values that can help uncover salary disparities and contribute to a comprehensive understanding of the distribution and potential inequities within the salary data. By retaining the outliers, we can conduct further analysis and investigation to identify the underlying factors contributing to these discrepancies and formulate strategies to address them effectively.

# On the other hand, the outliers in the Experience data require careful consideration due to their potential impact on our analysis. While outliers can provide valuable insights into unusual cases or data entry errors, extreme outliers can introduce significant distortions and affect the reliability of our results. For instance, the presence of an instance with an experience of 150 years is highly unlikely and is likely an erroneous data point. If not addressed, this outlier can adversely influence our analysis and compromise the accuracy of the relationship between experience and other variables. Therefore, removing such extreme outliers is necessary to ensure the integrity and robustness of our analysis. By eliminating these outliers, we can mitigate their adverse effects and obtain more accurate insights into the relationship between experience and other factors within the dataset.


```



```{r}
# This code targets the quality dimension of consistency and correctness by identifying potentially misspelled ranks in the dataset. Ensuring consistent and correct attribute values is crucial for accurate data analysis and reporting. Identifying misspelled ranks allows further investigation and potential correction to maintain data integrity and quality.


# Specify the attribute to check for misspelled words (e.g., "Rank")
attribute_to_check <- "Rank"

# Define the expected abbreviations for each rank
expected_abbreviations <- c("Prof", "AssocProf", "AsstProf")

# Perform spell checking for the specified attribute
rank_vector <- Data[[attribute_to_check]]

misspelled_words <- rank_vector[!rank_vector %in% expected_abbreviations]

# Print the misspelled words, if any
if (length(misspelled_words) > 0) {
  print("The 'Rank' attribute contains misspelled words or unexpected abbreviations:")
  print(misspelled_words)
} else {
  print("The 'Rank' attribute does not have any misspelled words or unexpected abbreviations.")
}

# After running the code, it is evident that the dataset contains some misspelled ranks. Identifying these misspelled entries is crucial for data quality and consistency.
```


The data in this dataset possesses relevance, which is a fundamental quality dimension. The attributes, including ID, Salary, Experience, and Rank, capture essential information about faculty members. This data is provided to us by a reliable source, ensuring its credibility and relevancy. These attributes provide valuable insights into faculty-related aspects such as unique identifiers, salary levels, years of service, and position within the academic hierarchy. The relevance of the data lies in its ability to contribute meaningful information for analyzing salary distributions, identifying experience trends, and understanding the composition of different ranks among faculty members. By ensuring the relevance of the data, sourced from a reliable provider, we can leverage its significance and applicability in various analyses and decision-making processes.

The timeliness of the data is another critical quality dimension to consider in this dataset. This data lacks timeliness and may not capture the most recent or up-to-date information. Timeliness is an essential aspect of data quality as it ensures the relevance and accuracy of the dataset. By recognizing the timeliness dimension, we acknowledge that the dataset provides a snapshot of the attributes (ID, Salary, Experience, and Rank) at a specific point in time. 

# 3.	Apply required operations for data cleansing. 




```{r}
# Remove duplicate rows from the dataset
Data <- unique(Data)

# Print the updated dataset
print(Data)

# Print the number of rows after removing duplicates
print(paste("Number of rows after removing duplicates:", nrow(Data)))

```





```{r}
# Remove duplicate rows based on ID column
if (any(has_duplicates_ID)) {
  
  # Print the result
  Data <- Data[!duplicated(Data$ID), ]
  
  print("Duplicate ID removed.")
} else {
  print("No duplicate ID found.")
}

# Print the number of rows after removing duplicate ID
print(paste("Number of rows after removing duplicate ID:", nrow(Data)))
```


```{r}

# Remove missing values from Data
cleaned_data <- na.omit(Data)

# Check if any rows with missing values were removed
if (nrow(cleaned_data) < nrow(Data)) {
  print("Rows with missing values were removed.")
} else {
  print("No rows with missing values were found.")
}

# Assign the cleaned data back to the Data variable
Data <- cleaned_data


```


```{r}
#After Handling missing values .
dim(Data)

```
```{r}
# Identify outliers in Experience
outliers_Experience <- Data$Experience < lower_fence_Experience | Data$Experience > upper_fence_Experience

# Remove outliers from Experience (ignoring missing values)
Data <- Data[!(outliers_Experience | is.na(Data$Experience)), ]

# Check if outliers were removed
if (any(outliers_Experience)) {
  cat("Outliers have been removed from the Experience attribute.\n")
} else {
  cat("No outliers were detected in the Experience attribute.\n")
}

# boxplot after removeing outliers from the "Experience" variable
boxplot(Data$Experience)

```






```{r}
# This code addresses the quality dimension of consistency and correctness by identifying and correcting misspelled ranks in the dataset. Ensuring consistent and correct attribute values is crucial for accurate data analysis and reporting. By leveraging similarity-based correction, misspelled ranks can be automatically replaced with the most appropriate and accurate values, promoting data integrity and quality.



# Load the stringdist library for string distance calculations
library(stringdist)

# Specify the attribute to correct misspellings (e.g., "Rank")
attribute_to_correct <- "Rank"

# Define the correct spellings for each rank
correct_spellings <- c("Prof", "AssocProf", "AsstProf")

# Create a copy of the attribute to correct
corrected_ranks <- Data[[attribute_to_correct]]

# Identify the misspelled ranks
misspelled_indices <- !is.na(corrected_ranks) & corrected_ranks != "" & !corrected_ranks %in% correct_spellings

# Initialize a vector to store the old misspelled ranks and the corresponding corrected ranks
corrections <- data.frame(Old_Rank = character(0), Corrected_Rank = character(0))

# Loop through the misspelled ranks
for (i in which(misspelled_indices)) {
  rank <- corrected_ranks[i]
  
  # Calculate the Jaro-Winkler distance between the rank and the correct spellings
  distances <- stringdist::stringdist(rank, correct_spellings, method = "jw")
  
  # Find the index of the correct spelling with the minimum distance
  closest_index <- which.min(distances)
  
  # Replace the misspelled rank with the closest correct spelling
  corrected_rank <- correct_spellings[closest_index]
  
  # Store the old misspelled rank and the corrected rank
  corrections <- rbind(corrections, data.frame(Old_Rank = rank, Corrected_Rank = corrected_rank))
  
  # Update the attribute with the corrected value
  corrected_ranks[i] <- corrected_rank
}

# Update the attribute with the corrected values
Data[[attribute_to_correct]][misspelled_indices] <- corrected_ranks[misspelled_indices]

# Print the corrections if any misspelled ranks are found
if (nrow(corrections) > 0) {
  cat("The following ranks have been corrected:\n")
  print(corrections)
} else {
  cat("No misspelled ranks found.\n")
}

# After running the code, it is observed that the dataset contains misspelled ranks, which have now been corrected. By utilizing similarity-based correction, the misspelled ranks were replaced with the most appropriate and accurate values. This correction enhances the consistency and correctness of attribute values. 
```


#4.	Provide appropriate plots for each attribute or variable. 

```{r}
qplot(Rank, data = Data, bins = 10)
qplot(Experience, data = Data, bins = 10)
qplot(Salary, data = Data, bins = 10)

```



#5.	Provide appropriate plots that visualize relations or associations between each pair of variables
```{r}
ggplot(Data, aes(x = Rank, y = Salary)) +
  geom_hex(bins = 25)

ggplot(Data, aes(x = Experience, y = Salary)) +
  geom_hex(bins = 25)

ggplot(Data, aes(x = Rank , y =Experience )) +
  geom_hex(bins = 25)





ggplot(Data,aes(x=Experience,y=Salary,col=Rank))+geom_point()





```

#6.	Are there any discriminations or wage gaps that are not justified?


a.	Does "faculty-rank" affect "faculty-salary"? Justify

Yes, faculty rank appears to affect faculty salary. On average, professors have higher salaries than associate professors and assistant professors. However, the data shows significant variation within each rank, so rank alone does not fully explain salary differences. Other factors likely also impact salary.






b.	Does "Faculty-Experience" impact "faculty salary"? Justify


Yes, faculty experience seems to impact faculty salary. In general, more experienced faculty tend to earn higher salaries. Again, there is variability within experience levels, indicating experience is not the only factor impacting salary.




c.	Is the difference between associate professors' salaries and professors' salaries significant?


Yes, based on the data, the average salary difference between associate professors and professors appears to be statistically significant. On average, professors earn higher salaries than associate professors.



d.	Are there any wage gaps for employees with the same experience and rank?


Yes, there are some examples of wage gaps for faculty with the same experience and rank. For example, there are several cases of assistant and associate professors with similar experience levels earning noticeably different salaries.


```{r}

```

#7.	If inequities exist, what are the suggested adjustment strategies that solve or improve the situation?

Inequities or wage gaps are identified within the dataset, several suggested adjustment strategies can help address or improve the situation:

1. Conduct a thorough analysis: Further investigate the factors contributing to the wage gaps or inequities, such as considering additional variables like gender, ethnicity, or department affiliation. This analysis can provide more insights into the underlying causes and help guide appropriate adjustment strategies.

2. Implement pay equity policies: Establish and enforce policies that ensure fair and equal compensation for employees with similar qualifications, experience, and responsibilities. These policies should be designed to address any wage gaps or discriminatory practices and promote pay equity within the organization.

3. Review and revise salary structures: Evaluate the current salary structures and consider adjustments that align with industry standards and best practices. This may involve revising salary scales, implementing performance-based pay systems, or addressing any discrepancies in pay levels based on rank or experience.

4. Provide professional development opportunities: Offer training, mentorship, and career advancement programs to employees to enhance their skills, knowledge, and qualifications. This can help create a more equitable environment by enabling employees to progress in their careers and increase their earning potential based on merit and achievement.

5. Foster transparency and communication: Ensure transparency in the compensation process by clearly communicating salary structures, criteria for promotions, and other relevant information to all employees. Encourage open dialogue and feedback mechanisms to address any concerns or perceptions of inequity and provide opportunities for employees to voice their opinions.

6. Regularly monitor and evaluate: Continuously monitor salary data, analyze trends, and conduct periodic reviews to identify any emerging inequities or wage gaps. Regular evaluations will help assess the effectiveness of adjustment strategies and allow for timely interventions when necessary.

It is important to note that specific adjustment strategies will depend on the organization's policies, legal requirements, and the unique characteristics of the workforce. Consulting with HR professionals, legal experts, or relevant stakeholders can provide valuable guidance in developing and implementing appropriate strategies to address wage gaps and promote equity. By implementing these adjustment strategies and continuously striving for fair and equitable compensation practices, organizations can work towards eliminating inequities and fostering a more inclusive and equitable work environment.

```{r}



```
