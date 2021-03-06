###########################################################################
# A Data Science Analysis of the PlayerUnknown's BattleGrounds (PUBG) data
#
# See https://github.com/anshul2209/PUBGrankpredictor
#
# Essential Data Science Template R from Graham.Williams@togaware.com
#
# Template applied to pubg dataset by Anshul Bansal as a Jupyter Notebook
#
# Converted to MLHub package by Graham and Anshul.
#
# License: MIT

cat("====================================
PlayerUnknown's BattleGrounds (PUBG)
====================================

This package illustrates the use of an Essentials of Data Science Template for
the analysis of a dataset provided by the publishers of the PlayerUnknown's
BattleGrounds datset. We illustrate the use of the template to gain insights
into the data and then to model a derived outcome: the player is in the top
10% of player.

Please wait while we load the data.
")

suppressMessages(
{
  # Load required packages from local library into the R session.

  library(dplyr) 	# Data wrangling, glimpse() and tbl_df().
  library(ggplot2)      # Visualise data.
  library(lubridate)    # Dates and time.
  library(randomForest) # Impute missing values with na.roughfix().
  library(readr)        # Efficient reading of CSV data.
  library(rattle)       # normVarNames().
  library(magrittr)     # Pipes %>%, %T>% and equals(), extract().
  library(scales)       # Format comma().
  library(ggplot2)      # Visualise data.
  library(tidyr)        # Prepare a tidy dataset, gather().
  library(stringi)      # String concat operator %s+%.
  library(stringr)      # String manipulation.
  library(FSelector)    # Feature selection: information.gain().
  library(tibble)       # Convert row names into a column.
  library(lubridate)    # Dates and time.
  library(GGally)       # Extension to ggplot2
  library(gridExtra)    # Grid evaluation
  library(rpart)        # rpart function
  library(ROCR)         # ROC curve
})

# Data Ingestion

pubg   <- read.csv("pubg.csv") # 3s
dsname <- "pubg"
ds     <- get(dsname)

cat("Press Enter to begin the data review: ")
invisible(readChar("stdin", 1))

# Exploring The Shape of the Data

sprintf("
================
Data Preparation
================

The provided dataset contains %s observations (players) of %s variables.
", comma(nrow(ds)), comma(ncol(ds))) %>% cat()


cat("
As standard practise for the Essentials templates the original variable
names are normalised into our prefered standard format. Here we illustrate
that transformation from the original names to the normalised names.

")

names(ds) %>%
  paste(collapse=", ") %>%
  paste("Original Names: ", .) %>%
  str_wrap(width=78) %>%
  cat() 

# Normalizing Variable Names

names(ds) %<>% normVarNames()

cat("\n\n")

names(ds) %>%
  paste(collapse=", ") %>%
  paste("Normalised Names: ", .) %>%
  str_wrap(width=78) %>%
  cat() 

cat("\n\nPress Enter to continue with the data review: ")
invisible(readChar("stdin", 1))

cat("
================
Data Observation
================

Below we list the first few variables, reporting their data type
together with the first few values of the variables from the dataset.
Notice that variables like x, id, group_id, match_id are clearly
identifiers and thus have no role in the analysis for our purposes.

")

# Confirm the results are as expected and review the data.

nc <- ncol(ds)
dc <- 11 # Number of columns to display to fit the screen.

glimpse(ds[1:dc])

cat("\nPress Enter to continue with the data review: ")
invisible(readChar("stdin", 1))

cat("
================
Data Observation
================

Here we see a random selection of variables and their data type and the first
few values of the variables from the dataset.

")

glimpse(ds[sample(nc, dc)])

cat("\nPress Enter to continue with the analysis: ")
invisible(readChar("stdin", 1))

# * There are some variables like ids, groupids, x which doesn't give any information about data modelling so we may want to remove those data points.


# TODO VISUALISE THE win_place_perc DISTRIBUTION.

# The target variable is_top_ten which we will create based on win_place_perc.

target <- "is_top_ten"

ds %<>% 
  mutate(is_top_ten = if_else(ds$win_place_perc >= 0.9, TRUE, FALSE))

# Visualising and checking the distribution of the target variable.

sprintf("
===================
Target Distribution
===================

We will create a binary target variable for our model demonstration. The target
will record whether the player is in the top ten percent of all players. The
target is derived from the variable win_place_perc. Of the %s observations
there are %s that are in the top 10 percent, making that %d%% as targets.

We can visulaise this distribution of the target variable to reinforce the
observations we have just made.
",
nrow(ds) %>% comma(),
ds[[target]] %>% sum() %>% comma(),
round(100*sum(ds[[target]])/nrow(ds))) %>% cat()

cat("
Press Enter to continue to a plot of the distribution: ")
invisible(readChar("stdin", 1))

fname <- "battle_target_distribution.pdf"
pdf(file=fname, width=8)
ds %>%
  ggplot(aes_string(x=target)) +
  geom_bar(width=0.2, fill="grey") +
  theme(text=element_text(size=14)) +
  scale_y_continuous(labels=comma) +
  labs(title = "Distribution of Target : Top Ten Percent of Players",
       x = "Top Ten",
       y = "Count",
       caption = "Source: PUBG Dataset")
invisible(dev.off())
system(sprintf("xpdf %s", fname), ignore.stderr=TRUE, wait=FALSE)

cat("
Close the plot window using Ctrl-w
Press Enter to continue: ")
invisible(readChar("stdin", 1))

cat("
=================
Data Observations
=================

Many of the other variables in the dataset will be used to predict the target.
It is useful to observe these variables to gain some understanding and to
ensure the data itself appears to have integrity.

=====================
Distribution of Kills
=====================

First we review the distribution of the number of kills. The variable *kills*
records the number of enemy kills achieved by the player. Noting that this is
an integer variable ranging from 0 to 35 in value, we will plot the
distribution. Most players die without killing anybody in the game. 

Press Enter to review the distrbution of the number of kills: ")
invisible(readChar("stdin", 1))

fname <- "battle_kills_distribution.pdf"
pdf(file=fname, width=12)
ds %>%
  ggplot(aes(x=kills)) +
  geom_bar() +
  scale_y_continuous(labels=comma)
invisible(dev.off())
system(paste("xpdf", fname), ignore.stderr=TRUE, wait=FALSE)

cat("

Close the plot window using Ctrl-w.
Press Enter to continue: ")
invisible(readChar("stdin", 1))

cat("
===============================
Distribution of Kills by Target
===============================

The previous plot shows just the distribution of the specific variable. It
can also be useful to view the break-up of the target variable for each
segment. The following plot will do so. That is, we include the distribution
of the target variable (*is_top_ten*) within the distribution of *kills*.
We can then observe that players with higher kills have a higher chance of
being in the top ten.

Press Enter to continue on to the plot: ")
invisible(readChar("stdin", 1))


fname <- "battle_kills_target_distribution.pdf"
pdf(file=fname, width=12)
ds %>%
    ggplot(aes(x=kills, fill=is_top_ten)) +
    geom_bar()
invisible(dev.off())
system(paste("xpdf", fname), ignore.stderr=TRUE, wait=FALSE)

cat("
Close the plot window using Ctrl-w.
Press Enter to continue: ")
invisible(readChar("stdin", 1))

cat("
========================
Basic Variable Summaries
========================

")

summary(ds %>% select(walk_distance, weapons_acquired, match_duration))

cat("
Press Enter to continue: ")
invisible(readChar("stdin", 1))

cat("
================
Types of Matches
================
")

table(ds$match_type)

cat("
Press Enter to continue: ")
invisible(readChar("stdin", 1))

cat("
==============
Data Wrangling
==============

Data wrangling, sometimes referred to as data munging, is the process of
transforming and mapping data from a raw form into a structured format with
the intent of making it more appropriate and valuable for a variety of
downstream purposes such as analytics.

Here we choose a number of variables to be coverted into factors. First
we can check the number of unique values for each variable.")

sapply(ds, function(x) length(unique(x)))

# choose assists,  match_type, vehicle_destroys, road_kills and kill_streaks for factor variable conversion

vnames <- c("assists","dbnos", "headshot_kills", "boosts", "revives", "match_type", "vehicle_destroys", "road_kills", "kill_streaks", "is_top_ten")


stop("UP TO HERE")

# In[54]:


# Convert to factor variables

ds[vnames] %<>%
  lapply(factor)

# Note which variables are categoric.

ds %>%
  sapply(is.factor) %>%
  which() %T>%
  print() ->
  catc

# Normalise the levels of all categoric variables.

for (v in catc)
  levels(ds[[v]]) %<>% normVarNames()

# Review the categoric variables to confirm normalisation of levels.

# Ensure target is a factor
# Note the target variable.

target <- "is_top_ten"

# Ensure the target is categoric.

ds[[target]] %<>% as.factor()

# Confirm the distribution.

ds[target] %>% table()


# In[11]:


# Check if there is NA in the target variable 
sapply(ds, is.na) %>% sum()
# 1 such value, remove that value from ds

ds <- subset(ds, !is.na(ds$is_top_ten)) 

# Found no such record


# ## Feature Selection
# 
# We identify the  variables (features or columns of the dataset) that are irrelevant or inappropriate for modelling.
# 
# 

# In[12]:


# Note the available variables and assign to the variable vars.

ds %>%
  names() %T>%
  print() ->
  vars

# Place the target variable is_top_ten at the beginning of the vars.

c(target, vars) %>%
  unique() %T>%
  print() ->
  vars

# collect the identifiers.

id <- c("x", "id", "group_id", "match_id", "match_type", "win_place_perc")

id %>%
  union(if (exists("risk")) risk) %T>%
  print() ->
  ignore


# In[13]:


# Defining a helper function to count the number of distinct values in a column of a dataset

count_unique <- function(x)
{
  x %>% unique() %>% length()
}

# Using the heuristic in the above cell, identify the variables which have all unique values and assign their names to ids.

ds[vars] %>%
  sapply(count_unique) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  ids

"variables to ignore"
ignore <- union(ignore, ids) %T>% print()


# In[14]:


# Helper function to count the number of values missing.

count_na <- function(x)
{
  x %>% is.na() %>% sum()
}

# Heuristic to identify variables with only missing values.

ds[vars] %>%
  sapply(count_na) %>%
  equals(nrow(ds)) %>%
  which() %>%
  names() %T>%
  print() ->
  missing

# Add them to the variables to be ignored for modelling.

ignore %<>% union(missing) %T>% print()


# In[15]:


# Identify a threshold above which proportion missing is fatal.

missing.threshold <- 0.5

# Identify variables that are mostly missing.

ds[vars] %>%
  sapply(count_na) %>%
  '>'(missing.threshold*nrow(ds)) %>%  # This line checks if count_na(column) > threshold*nrow(ds)
  which() %>%
  names() %T>%
  print() ->
  mostly

# Add them to the variables to be ignored for modelling.

ignore <- union(ignore, mostly) %T>% print()


# In[16]:


# Helper function to count the number of levels.
# extract2(x) extracts the column with the name stored in x, from the dataset

count_levels <- function(x)
{
  ds %>% extract2(x) %>% levels() %>% length()
}


# Identify a threshold above which we have too many levels.

levels.threshold <- 20

# Identify variables that have too many levels.

ds[vars] %>%
  sapply(is.factor) %>%            # Returns TRUE for factor variables
  which() %>%
  names() %>%                      # Returns the names of all the factor variables 
  sapply(count_levels)%>%
  '>='(levels.threshold) %>%
  which() %>%
  names() %T>%
  print() ->
  too.many

ignore <- union(ignore, too.many) %T>% print()


# In[17]:


#Helper function to test if all values in vector are the same.

all_same <- function(x)
{
  all(x == x[1L])   # Checks if all the values in the column equals the value at x[1]
}

# Identify variables that have a single value.

ds[vars] %>%
  sapply(all_same) %>%
  which() %>%
  names() %T>%
  print() ->
  constants

# Add them to the variables to be ignored for modelling.

ignore <- union(ignore, constants) %T>% print()


# In[18]:


# Note which variables are numeric and store to the variable numc

vars %>%
  setdiff(ignore) %>%
  magrittr::extract(ds, .) %>%
  sapply(is.numeric) %>%
  which() %>%
  names() %T>%
  print() ->
  numc

# For numeric variables generate a table of correlations

ds[numc] %>%
  cor(use="complete.obs") %>%
  ifelse(upper.tri(., diag=TRUE), NA, .) %>%
  abs() %>%
  data.frame() %>%
  tbl_df() %>%
  set_colnames(numc) %>%
  mutate(var1=numc) %>%
  gather(var2, cor, -var1) %>%
  na.omit() %>%
  arrange(-abs(cor)) %T>%
  print() ->
  mc


# * We identify pairs of variables where we want to keep one but not the other variable because they are highly correlated.
# We limit the removals to those correlations that are 0.90 or more.
# * The varibales "num_groups" , "win_points", "rank_points" would be added to the ignore list of varibale.

# In[19]:


correlated <- c("num_groups", "win_points", "rank_points")

ignore <- union(ignore, correlated) %T>% print()


# ### Remove the varibales

# In[20]:


# Check the number of variables currently.

length(vars)

# Remove the variables to ignore.

vars %<>% setdiff(ignore) %T>% print()

# Confirm they are now ignored.

length(vars)


# In[21]:


# Construct the formulation of the modelling to undertake.

form <- formula(target %s+% " ~ .") %T>% print()

# Use correlation search to identify key variables.
# This might take some time to execute.

cfs(form, ds[vars])

# Use information gain to identify variable importance.

information.gain(form, ds[vars]) %>%
  rownames_to_column("variable") %>%
  arrange(-attr_importance)


# Looking at the above importance chart, we find that walk_distance, kill_place and boosts have the most importance.

# # Data Exploration
# 
# Use visualisations to explore the data. Intersperse code/output with your notes and observations.

# In[51]:


# Setting default plot size for this notebook.
# Use this line of code to set the size of your visualisation plots.

options(repr.plot.width = 10, repr.plot.height = 4)






options(repr.plot.width = 20, repr.plot.height = 8)

ds %>%
    ggplot(aes(x=match_type, y=kills, fill=match_type, srt=45)) +
    geom_bar(stat="summary", fun.y="mean") +
    theme(legend.position="none")


# From the above graph we infer, a player kills more number of players while playing solo.

# In[26]:


solos <- subset(ds, num_groups > 50) %>% length()
duos <- subset(ds, num_groups > 25 && num_groups <= 50) %>% length()
squads <- subset(ds, num_groups <= 25) %>% length()

"There are " %s+% solos %s+% " solos"
"There are " %s+% duos %s+% " duos"
"There are " %s+% squads %s+% " squads"


# In[57]:


options(repr.plot.width = 20, repr.plot.height = 8)

ds %>%
    ggplot(aes(x=match_type, y=kills, fill=target)) +
    geom_violin() +
    theme(legend.position="none")


# In[1]:


ds %>%
    ggplot(aes(x=boosts, y=win_place_perc, fill=is_top_ten)) +
    geom_boxplot(notch=FALSE) +
    theme(legend.position="none") +
    labs(title = "Boxplot for boosts",x = "boosts")


# To survive as a winner must actively leverage other items or use boost items to increase their power.

# # Predictive Modeling
# 
# We will partition the full dataset into three: train (70%), validate (15%), test (15%).
# 

# In[29]:


# Record the number of observations.

nobs <- nrow(ds) %T>% comcat()

# Create a trainin dataset of 70% of the observation

nobs %>%
  sample(0.70*nobs) %T>% 
  {length(.) %>% comma() %>% cat("Size of training dataset:", ., "\n")} ->
  train

# Create a validation dataset of 15% of the observations.

nobs %>%
  seq_len() %>% 
  setdiff(train) %>% 
  sample(0.15*nobs) %T>%
  {length(.) %>% comma() %>% cat("Size of validation dataset:", ., "\n")} ->
  validate

# Create a testing dataset of 15% (the remainder) of the observations.

nobs %>%
  seq_len() %>%
  setdiff(union(train, validate)) %T>%
  {length(.) %>% comma() %>% cat("Size of testing dataset:", ., "\n")} ->
  test


# In[30]:


# Cache the various actual values for target

tr_target <- ds[train,][[target]]    %T>% {head(., 20) %>% print()}

va_target <- ds[validate,][[target]] %T>% {head(., 20) %>% print()}

te_target <- ds[test,][[target]]     %T>% {head(., 20) %>% print()}


# In[31]:


# Splitting function: "anova" "poisson" "class" "exp"

mthd <- "class"

# Splitting function parameters.

prms <- list(split="information")

# Control the training.

ctrl <- rpart.control(maxdepth=5)

# Build the model

m_rp <- rpart(form, ds[train, vars], method=mthd, parms=prms, control=ctrl)


# In[32]:


# Capture the model in generic variables.

model <- m_rp
mtype <- "rpart"
mdesc <- "Decision Tree"

# Visually expose the discovered knowledge.
options(repr.plot.width = 8, repr.plot.height = 6)

fancyRpartPlot(model)


# In[33]:


# Review which importance of the variables.

ggVarImp(model)


# Walk distance has the greatest impact in deciding if the player would be in top 10.

# ## Evaluation
# 

# In[34]:


# create a function predict probabily and class of varibales
predict_prob_class <- function(data_type)
{
  model %>%
    predict(newdata=ds[data_type, vars], type="prob") %>%
    .[,2] %>%
    set_names(NULL) %>%
    round(2) ->
    prob
  
  model %>%
    predict(newdata=ds[data_type, vars], type="class") %>%
    set_names(NULL) ->
    class
  
  predict_list <- list("probability" = prob, "class" = class)
  return(predict_list)
  
}


# In[35]:


# Function to evaluate performance, in terms of accuracy , precison, recall, fscore etc
perf <- function(mdesc, dstype, target, prob, class, verbose=TRUE)
{
  # Provide informative introduction.
  
  if (verbose)
    "Performance Evaluation\n" %s+%
    "======================\n\n" %s+%
    "Model: " %s+% mdesc %s+% "\n" %s+%
    "Dataset: " %s+% dstype %s+% " dataset with " %s+%
    comma(length(prob)) %s+% " observations.\n" %>%
    cat("\n")
  
  # Calculate accuracy and error rates.
  
  sum(class == target, na.rm=TRUE) %>%
    divide_by(class %>% is.na() %>% not() %>% sum()) ->
    acc
  
  sum(class != target, na.rm=TRUE) %>%
    divide_by(class %>% is.na() %>% not() %>% sum()) ->
    err
  
  if (verbose)
    "Overall accuracy: " %s+% percent(acc) %s+% "\n" %s+%
    "Overall error: " %s+% percent(err) %s+% "\n" %>%
    cat("\n")
  
  # Generate error matricies.
  matrix <- errorMatrix(target, class)
  
  if (verbose)
  {
    cat("Error Matrices:\n\n")
    errorMatrix(target, class, count=TRUE) %>% print()
    cat("\n")
    matrix %>% print()
    cat("\n")
  }
  
  # Calculate recall, precision and F-score.
  
  rec <- (matrix[2,2]/(matrix[2,2] + matrix[2,1]))
  pre <- (matrix[2,2]/(matrix[2,2] + matrix[1,2]))
  fsc <- ((2 * pre * rec)/(rec + pre))
  
  if (verbose)
    "Recall: " %s+% percent(rec) %s+% "\n" %s+%
    "Precision: " %s+% percent(pre) %s+% "\n" %s+%
    "F-Score: " %s+% round(fsc, 3) %s+% "\n" %>%
    cat("\n")
  
  # Calculate AUC for the ROC curve.
  
  prob %>%
    prediction(target) %>%
    performance("auc") %>%
    attr("y.values") %>%
    .[[1]] ->
    auc
  
  if (verbose)
    "Percentage area under the ROC curve AUC: " %s+%
    percent(auc) %>%
    cat("\n")
  
  prob %>%
    prediction(target) %>%
    performance("tpr", "fpr") ->
    rates
  
  # Return a list of the evaluations.
  invisible(list(acc=acc,
                 err=err,
                 matrix=matrix,
                 rec=rec,
                 pre=pre,
                 auc=auc,
                 rates=rates))
  
}


# In[36]:


# function to create auc plot

aucplot <- function(rates, auc, mdesc, dstype)
{
  data.frame(tpr=attr(rates, "y.values")[[1]],
             fpr=attr(rates, "x.values")[[1]]) %>%
    ggplot(aes(fpr, tpr)) +
    geom_line() +
    labs(title="ROC - " %s+% mtype %s+%  dstype %s+% "- Dataset",
         subtitle=paste("AUC =", percent(auc)),
         x="False Positive Rate (1-Specificity)",
         y="True Positive Rate (Sensitivity)")
}


# In[37]:


# Assigning varibales for training, validation and testing dataset
mdesc <- "Decision Tree"
dstype_train <- "Training"
dstype_validate <- "Validation"
dstype_test <- "Testing"
train_prob <- predict_prob_class(train)$probability
train_class <- predict_prob_class(train)$class
validate_prob <- predict_prob_class(validate)$probability
validate_class <- predict_prob_class(validate)$class
test_prob <- predict_prob_class(test)$probability
test_class <- predict_prob_class(test)$class


# In[38]:


# Evaluate the performance on each data group

m_rp_perf_train <- perf(mdesc, "Training", tr_target, train_prob, train_class)
m_rp_perf_validate <- perf(mdesc, "Validation", va_target, validate_prob, validate_class)
m_rp_perf_test <- perf(mdesc, "Testing", te_target, test_prob, test_class)


# In[39]:


# Plot the graph for training auc
aucplot(m_rp_perf_train$rates, m_rp_perf_train$auc, mdesc, "Training")
# Plot the graph for validation auc
aucplot(m_rp_perf_validate$rates, m_rp_perf_validate$auc, mdesc, "Validation")
# Plot the graph for testing auc
aucplot(m_rp_perf_test$rates, m_rp_perf_test$auc, mdesc, "Testing")


# # Conclusion
# 
# * The most important features which helps in predicting if the player would be present in the top 10 is the walking distance and killplace in the game.
# 
# * If the person walks a distance of more than ~2600 metres and and has a killplace(Ranking in match of number of enemy players killed) less than 6, better chances of the player making it to the top 10 list.
# 
# * Most people prefer to play solo and that too in 'solo first person' category.

# ## Model Performance Results Summarised

# ### Training Data Set
# 
# * Overall accuracy: 91.5%
# * Overall error: 8.48%
# * Recall: 41%
# * Precision: 65.2%
# * F-Score: 0.503
# * AUC: 87.6%
# 
# #### Validation Data Set
# 
# * Overall accuracy: 91.6%
# * Overall error: 8.37%
# * Recall: 42.5%
# * Precision: 66.2%
# * F-Score: 0.517
# * AUC: 87.8%
# 
# #### Testing Data set
# 
# * Overall accuracy: 91.4%
# * Overall error: 8.56%
# * Recall: 41.7%
# * Precision: 67.2%
# * F-Score: 0.514
# * AUC: 87.9%
# 
# 

# The model has an overall acurracy of 91.4% and AUC of 87.9%, on testing dataset.

# ## Improvements and Future Scope

# * The precision of the model can be improved to pick the right set of players to be displayed on the online portal using some different modelling techniques like random forest.
# 
# * Data can be pruned more to identify cheaters ( eg a person having more than 10 kills without even moving in the game.
# 
# * In a game it is not necessary that there would be 100 players everytime, a feature- 'playersJoined' can be introduced and its weightage can be taken into account to see any model improvements.
# 
# * The model can be extended to actually predict the probabilty of the person winning the game instead of top 10 position.
