# Spatial Autocorrelaiton Tutorial

The objective of this tutorial is to demonstrate how to detect and analyze spatial autocorrelation at both a global and local scale. This example will use household income and French knowledge in Penticton BC to preform tests of spatial autocorrelation. 

## Introduction

In simple terms spatial autocorrelation can help us answer the question if a variable is more similar in areas closer together. Specifically, it is the measure of the relationship of a variable to itself across space (Getis, 2008). A variables relationship to itself can be revealed as random, clustered, or dispersed using statistical tools such as Moran's I (Esri, n.d.). This can have further implications for spatial dependence. 

In this study, census data is used as an example for this analysis. Demographic characteristics (such as those in census data) often exhibit spatial autocorrelation (Ghang Xiong, 2012). The census provides data on numerous demographic characteristics as well as their geographic location based upon census track location which allows for a spatial scale of analysis. Further, there is a wide range of variables provided by the census allowing for many data relationships to be explored. For example, analyzing factors such as incoming could reveal economic inequality when clustered.


## Setting Up

### What are Libraries?
Below we call our libraries. The necessary libraries must be loaded for each new R environment. Libraries allow for complicated code to be called as a defined function.There are many packages available to be loaded onto R however we only need a few for the spatial autocorrelation calculations we are doing here. It is important to note that packages must be installed before they are called as a library, however packages only need to installed once on your computer. 

### What is a Working Directory?
After we call our libraries, we set our working directory. The working directory is essentially the file path R is looking for to read and save files. Make sure the slashes are in the correct direction as this can pose issues. 

```{r Libraries, tidy.opts=list(width.cutoff=60), echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}

#Loads in libraries:
library("sf")
library("knitr")
library("tmap")
library("raster")
library("st")
library("e1071")
library("spdep")
library("shinyjs")

#Sets the working directory
dir <- "C:/Users/kinas/OneDrive - University of Victoria/Classes/GEOG 418/Assignment 3/Assignment 3 Data"
setwd(dir)
```

## Reading the Files and Cleaning the Data
To start we are creating variables for both the .csv and .shp file we will be using. The reading functions used below read the census data into a data frame and the census boundaries into a st data frame (a data frame for spatial data).

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#From the working dir read in the csv
csv <- read.csv(
  "C:/Users/kinas/OneDrive - University of Victoria/Classes/GEOG 418/Assignment 3/Assignment 3 Data/ucgsJQnBVLvP_data.csv") 

#Reading the shapefile
shp <- st_read(
  "C:/Users/kinas/OneDrive - University of Victoria/Classes/GEOG 418/Assignment 3/Assignment 3 Data/lda_000b16a_e.shp") 
```
Next, we are cleaning our data for ease of use. First we create the vector cols to create column names so we can identify what columns are referring to what. Next we are removing unwanted rows and merging merging the results with the spatial polygon data frame. Finally we have selected the subset of interest (in this case Penticton). 

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID characters
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Assigning Penticton subset of census data
Municp <- subset(census_DAs, census_DAs$CMANAME == "Penticton")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Next we need to remove all polygons with missing or NA values for either median total income or knowledge of French. This is important to ensuring the relevancy of the polygons and ensuring they actually contain the variables of interest. 

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$PercFrench)),]
```

### Calculating Descriptive Statistics
Now we are creating descriptive statistics for the two variables of interest (Median income and French knowledge). The statistics are output in the table below. 

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$PercFrench)
stdevFrench <- sd(French_noNA$PercFrench)
skewFrench <- skewness(French_noNA$PercFrench)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
```
<img width="253" alt="Table1Descriptive" src="https://github.com/user-attachments/assets/79cb89e0-6daa-4346-b0e2-b6af26f101a6">


### Creating a Map
Next we will create maps of the distribution of our two variables in the selected area using the tmap package. These maps will highlight both income and french knowledge over the study area. By creating these maps, we can visualize potential income disparities and language and cultural hotspots which will be further tested for autocorrelaiton.

The code below demonstrates how the map is made as col selects which values from the data will be used (in this case Income and French Knowledge). Style is the method of breaking down and classifying the data, in this case we are using 6 (n=6) jenks (natural grouping of the data). Palette chooses the color palette of the maps and colorNA selects the colors of the missing data. 


```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Penticton's census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "PuBu", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```

<img width="429" alt="DescriptiveStatsMap" src="https://github.com/user-attachments/assets/3c12bb4f-73a0-485b-8ea2-5cd4fb3c4dab">


## Neighbourhood matrix

We will use a neighborhood matrix in our next analysis. Using spatial weights allow us to integrate relation into analysis (Janatabadi & Ermagun, 2024). This is a way to determine what is 'local' when calculating the local statistics of the area surrounding an observation. If we are to determine how similar an observation is to its surrounding, we must first define its surrounding. For example, areas closer to the observation will have a greater weight than areas further away. In this case our neighbors will be different census track polygons. 

These neighborhoods are based on a list of neighbors that is created from the poly2nb() function in the 'spdep' package. It does so by identifying neighbors with shared borders. Then we can select which neighbors will be used in analysis. In this case we are using the queen matrix which selects the eight closest neighbors. In rooks weight, the four nearest neighbors are used. To change from queen weight to rook weight, we must change the 'queen = TRUE' to 'queen = FALSE'.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates of center
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
#Matches coordinators of Income.net to Income_noNA
crs(Income.net) <- crs(Income_noNA)

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)

```

The maps below are networks depicting which census track neighbors are included in different weighting matrices (rooks, queens, and both). These have been created by converting the neighborhood list into a set of lines that connect the center of neighboring polygons depending on weight (with the function nb2lines). 


```{r NeighboursmapIncome, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Penticton's census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='blue', lwd = .5)

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='red', lwd = .5)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='blue', lwd = .5) +
               tm_shape(Income.net2) + tm_lines(col='red', lwd = .5)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```

<img width="415" alt="IncomeNeighbors" src="https://github.com/user-attachments/assets/ae6971eb-d1da-4a7a-aa40-d4988c06836a">


Next a map of the French weight networks are created. Although, it is essentially the same as the census polygons involved in the analysis are the same. 

```{r NeighboursmapFrench, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Penticton's census dissemination areas showing french knowledge neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Make queens map
FrenchQueen <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(French.net) + tm_lines(col='blue', lwd = .5)

#Make rooks map
FrenchRook <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(French.net2) + tm_lines(col='red', lwd = .5)

#Make combined map
FrenchBoth <- tm_shape(French_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(French.net) + tm_lines(col='blue', lwd = .5) +
               tm_shape(French.net2) + tm_lines(col='red', lwd = .5)

#Print maps in a three pane figure
tmap_arrange(FrenchQueen, FrenchRook, FrenchBoth, ncol = 3, nrow = 1)

```

<img width="412" alt="French Neighbors" src="https://github.com/user-attachments/assets/3d52a2af-a945-43cf-8bcf-01a8c20547f6">


These maps highlight the difference in which polygons are selected using rooks and queens weight. Queens weight obviously expands the size of the neighborhood as there are eight neighboring connections (blue) rather than four (red). The two together (far right) is fairly similar to the queens weight with a few extra connections. 


### Weighting the Neighbors

The weighting of the neighbors determines how much weight each neighbor is given, thus determining its effect on analysis. Below we create a weights matrix using the "nb2listw" function from the "sdpep" library. We apply these functions to the "Income.nb" and "French.nb" files above which contains the neighborhood links. To ensure the program will run in the case of a polygon having zero neighborhood links, we define "zero.policy"  as equal to true to assign vectors of zero length for polygons with no neighbors. Additionally, in the code below we defined the style of the weight (which includes "B", "W", and "C"). "B" weights gives each neighbor a weight of 1 and all other polygons a weight of 0. "W" gives each neighbor a weight that sums to one and all other neighbors a weight of 0. "C" gives all equal weight across the study area. 



```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]

```

## Global Moran’s I
Once we have selected and weighted our neighbors, the Global Moran's I statistic can be calculated. As a global measure, it measures spatial autocorrelation based on all feature locations and values simultaneously (Esri, n.d.). This single statistic provides a total measure of spatial autocorrelaiton for the entire study area. The equation for the statistic is: 

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

$x$ is the variable being assessed (so both Income and French Knowledge) and $x_i$ is the variable value at a point of interest (i) (so each individual observation). $x_j$ is a neighbor of $x_i$ (based on our queen weighting scheme defined above). Then, the spatial weighting matrix $W_{i,j}$ (as defined by style in the code above) is multiplied by both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$.

Finally the denominator is used to standardize our values like in most statistics equations. Thus, random clustering will be closer to 0 (as the Expected Moran's I is relatively close to zero depending on the number of observations). Relatively high values are then associated with positive (clustered) spatial autocorrelation and relatively low values are associated with negative (dispersed) autocorrelation.  


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   MoransI = c(round(mIIncome,2), round(mIFrench,2)),
                   ExpectedI = c(round(eIIncome,2), round(eIFrench,2)),
                   Variance = c(round(varIncome,4), round(varFrench,4)))

#Produce table
kable(data, caption = paste0("Moran's I for selected ", 2016, " census variables"))
```

<img width="209" alt="Table2Moran's" src="https://github.com/user-attachments/assets/835a16ea-cd73-400a-aac9-998ade326cbc">

As seen in the table above both Income and French Knowledge, the Moran's I value is positive and greater than the Expected I value. Variance will be used in the following Z score calculation.

The following code calculates the possible Moran's I range values. This allows us to contextualize the observed Moran's I value. If the observed Moran's I is close to the maximum range it suggests a strong clustered autocorrelation. If the observed Moran's I is close to to the minimum range, it indicates a strong dispersed spatial autocorrelation. 


```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
rangeIncome <- moran.range(Income.lw)
minRangeIncome <- rangeIncome[1]
maxRangeIncome <- rangeIncome[2]

#Calculate the range for the French variable
rangeFrench <- moran.range(French.lw)
minRangeFrench <- rangeFrench[1]
maxRangeFrench <- rangeFrench[2]


#Create dataframe for display in table
datarange <- data.frame(Variable = c("Income", "French Language"),
                   Minimum = c(round(minRangeIncome,2), round(minRangeFrench,2)),
                   Maximum = c(round(maxRangeIncome,2), round(maxRangeFrench,2)))

#Produce table
kable(datarange, caption = paste0("Range of Moran's I for selected ", 2016, " census variables"))

```

<img width="247" alt="Table3Range" src="https://github.com/user-attachments/assets/861885fb-a250-42b8-a83b-1fd99657b1c4">


Thus, while both the French Moran's I and the Income Moran's I are greater than the expected I, they are not very close to the maximum Moran's I value. Thus they may not show super strong clustering. 

We can further improve the confidence in our analysis by testing the statistical significance using a Z-test. Here our null hypothesis is that Income and French Knowledge are randomly distributed. Our alternative hypothesis is that Income and French Knowledge are spatially autocorrelated. Using an $\alpha$ value of 0.05, if our Z-score falls above 1.96 we can say the data is clustered and below 1.96 we can say it is dispersed.

We can calculate a Z-test using the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))

```

The z-score for income is 3.9. Since the z-score is greater than the confidence value of 1.96 we can conclude that the median income in Penticton is significantly clustered. The z-score for french knowledge is 1.72. Since this is below the value of 1.96 we cannot conclude that the French knowledge in Penticton is significantly autocorrelated.  

## Local spatial autocorrelation

While global spatial autocorrelatuion summarizes every observation (census tracks in this case) in a single statistic, it fails to show where clustering may occur. Local spatial autocorrelation on the other hand provides each census track its own set of statistics (Rey et al., 2020). This allows us to analyze patterns at a finer scale. For example we could determine where clustered hot spots may occur. 

The calculation for Local Moran’s I has similar features to the Global Moran's I calculation. However it is arranged in a different way as seen by the equation below: 

$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

Now, to use the calculate the local indicators of spatial autocorrelation (LISA) we can call the function loclmoran() as long as we input our variable and pre-determined weighting scheme. For each variable we are given a Local Moran's I value (Ii), am estimate Local Moran's I value (E.Ii), a Variance (var), a z-score (Z.Ii), and a p-value (P).


```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```


Now we will map our outcomes of the LISA test using our basic mapping functions. In this case we are mapping census tracks that are significantly clustered in red and those which are significantly dispersed in blue. 


```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Penticton's census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```

As can be seen in the map below, both variables show some hot spots for both clustering and dispersion. Median total income appears to have multiple census tracks which are clustered in the southeastern side of town, and one census track which is dispersed just slightly north of the towns core. French knowledge shows less overall significantly autocorrelated areas with a few of the census tracks showing either clustering or dispersion. 

<img width="415" alt="LISAmaps" src="https://github.com/user-attachments/assets/065afa65-df86-4751-858f-d9c020d3bfc2">

These maps provide an excellent visual of which and how many polygons are significantly spatially autocorrelated. However graphing these trends may provide us with a deeper look into patterns. 


```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```

<img width="406" alt="Scatterplotincome" src="https://github.com/user-attachments/assets/08c9610d-146e-4f56-8f57-26fe048f62da">


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```

<img width="400" alt="ScatterplotFrench" src="https://github.com/user-attachments/assets/488e41b2-3a19-4d68-bee3-da37dbed0925">


For these plots, points with diamonds are considered statistically significant while the line shows the overall trend in the data. A positive sloped line indicates clustering while a negative sloped line indicates dispersion.To further analyze these plots, we can look to the quadrants. The top right quadrant indicated clustering of values higher than the mean. The bottom left quadrant indicates clustering of values lower than the mean. The top left quadrant indicates values lower than the mean surrounded by values higher than the mean and the lower right quadrant indicates values higher than the mean surrounded by values lower than the mean. 

In the case of total median income it can be seen that most of the values are generally clustered as they are in the top right and bottom left quadrant. There is however one significant outlier in the bottom right quadrant which is likely a higher income neighborhood surrounded by lower income values. In the case of French knowledge, there is less of an overall trend in the data as many values exist across the quadrants, however the general trend still indicates slight clustering. 



## Summary

The goal of this study was to determine the spatial autocorrelation of both total median income and french knowledge in Penticton BC. Using a queen's weighting  matrix, spatial autocorrelation was calculated at both a global and local scale. Pentiction's median income appeared to be clustered, and after preforming a z-test the significance of its clustering was confirmed. At a local scale it was also clear that income showed clustering however test revealed a census track which showed significant evidence of being dispersed. Measures of autocorrelation can aid in measures of inequality (Pattnaik, 2020), this is because it can indicate clusters of low or high income. Clusters of wealthy areas as seen in Pentiction could pose challenges such as inequitably distributed services and infrastructure.

French knowledge in Pentiction did appear slightly clustered in initial testing however after the z-test, this clustering was not determined to be statistically significant. The local measures of spatial autocorrelation showed somewhat varied levels of both dispersion and clustering fitting with the global measure that indicated there was no significant spatial autocorrelation.  

## Recomendations

This method of testing for spatial autocorrelaiton can be applied to other data sets to analyze income inequality as well as clustering of different demographic factors such as language, race, and housing status. Additionally, this study used 2016 census data, however using more current or past data, longitudinal studies could be conducted to analyze potential changes in distribution of different demographic factors. These could help policy makers reduce clustering of disadvantaged communities and overall improve community well-being. 

## References
Esri. (n.d.). How Spatial Autocorrelation (Global Moran’s I) works—ArcGIS Pro | Documentation. Pro.arcgis.com. https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/h-how-spatial-autocorrelation-moran-s-i-spatial-st.htm

Getis, A. (2008). History of the Concept of Spatial Autocorrelation: A Geographer’s Perspective. Geographical Analysis, 40(3), 297–309. https://doi.org/10.1111/j.1538-4632.2008.00727.x

Janatabadi, F., & Ermagun, A. (2024). Access Weight Matrix: A Place and Mobility Infused Spatial Weight Matrix. Geographical Analysis. https://doi.org/10.1111/gean.12395

Pattnaik, A. (2020, April 2). Spatial Autocorrelation: Close Objects Affecting Other Close Objects. Medium; Towards Data Science. https://towardsdatascience.com/spatial-autocorrelation-close-objects-affecting-other-close-objects-90f3218e0ac8

Rey, S., Arribas-Bel, D., & Wolf, L. (2020). Local Spatial Autocorrelation — Geographic Data Science with Python. Geographicdata.science. https://geographicdata.science/book/notebooks/07_local_autocorrelation.html
