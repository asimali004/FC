
df<-read.csv("F://Reserach Work/R-Studio/Smoker/insurance.csv")
head(df)
library(magrittr)               # to use pipes
library(funModeling)
library(skimr)                  # to get a quick summary table
library(caret) 
library(BSDA)
library(ggplot2)
library(foreign)
library(MASS)
library(robustbase) 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dplyr)

# denote factor variables
df$sex <- factor(df$sex)
df$smoker <- factor(df$smoker)
df$region <- factor(df$region)
df$children <- factor(df$children)
# check for missing values
df %>%
  is.na() %>%
  sum()
# check data types
df %>%
  str()
head(df)
figsize <- options(repr.plot.width=12, repr.plot.height=12) # set plot size for this plot 

# Smoker count plot
smoker <- df %>%
  ggplot(aes(x=smoker, fill=smoker)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(label=paste0(round(after_stat(prop*100), digits=1), "%"),group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of policyholders by smoking"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("no" = "Non-smoker", "yes" = "Smoker")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,2000,100)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )
### 

# Region count plot
region <- df %>%
  ggplot(aes(x=forcats::fct_infreq(region), fill=region)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(label = paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of policyholders by region"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("northeast" = "North East", "northwest" = "North West",
               "southeast" = "South East", "southwest" = "South West")
  ) +
  # adjust ticks
  scale_y_continuous(
    breaks=seq(0,350,50)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

##
# Sex count plot
sex <- df %>%
  ggplot(aes(x=forcats::fct_infreq(sex), fill=sex)) +
  geom_bar(show.legend = FALSE) +
  # add percentages on top of bars
  geom_text(
    stat='count',
    aes(
      label=paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of policyholders by sex",
    fill = "Sex"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("male" = "Male", "female" = "Female")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,700,100)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

##
# Children count plot
children <- df %>%
  ggplot(aes(x=forcats::fct_infreq(children), fill=children)) +
  geom_bar(show.legend = FALSE) +
  # add percentages
  geom_text(
    stat='count',
    aes(label=paste0(round(after_stat(prop*100), digits=1), "%"), group=1),
    vjust=-0.4,
    size=4
  ) +
  # add labels
  labs(
    x = "",
    y = "",
    title = "Number of dependents per policy"
  ) +
  # rename x-ticks
  scale_x_discrete(
    labels = c("0" = "None")
  ) +
  # adjust y-ticks
  scale_y_continuous(
    breaks=seq(0,600,50)
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14)
  )

# Plot grid
cowplot::plot_grid(
  smoker, region, sex, children,
  labels="AUTO",
  ncol = 2,
  nrow = 2
)

options(figsize)

###

figsize <- options(repr.plot.width=20, repr.plot.height=16)

# Age distribution
age_hist <- df %>%
  ggplot(aes(x=age))+
  geom_histogram(
    binwidth = 5,
    show.legend = FALSE,
    fill="#ff5733"
  )+
  labs(
    x = "Ages of policyholders",
    y = "Number of policyholders",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

age_dens <- df %>%
  ggplot(aes(x=age)) +
  geom_density(
    alpha=.3,
    fill="#ff5733"
  )+
  labs(
    x = "Ages of policyholders",
    y = "Probability density",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

age_box <- df %>%
  ggplot(aes(y=age)) +
  geom_boxplot(
    alpha=.5,
    fill="#ff5733"
  )+
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "Ages of policyholders",
    x = "",
    title = "Distribution of ages"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

# BMI distribution
bmi_hist <- df %>%
  ggplot(aes(x=bmi))+
  geom_histogram(
    binwidth = 4,
    show.legend = FALSE,
    fill = "#55ab11"
  )+
  labs(
    x = "BMI scores of policyholders",
    y = "Number of policyholders",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_dens <- df %>%
  ggplot(aes(x=bmi)) +
  geom_density(
    alpha=.3,
    fill="#55ab11"
  )+
  labs(
    x = "BMI scores of policyholders",
    y = "Probability density",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_box <- df %>%
  ggplot(aes(y=bmi)) +
  geom_boxplot(
    alpha=.5,
    fill="#55ab11"
  )+
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "BMI scores of policyholders",
    x = "",
    title = "Distribution of BMI scores"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

# Charges distribution
charges_hist <- df %>%
  ggplot(aes(x=charges)) +
  geom_histogram(
    binwidth = 2000,
    show.legend = FALSE,
    fill = "#FFC300"
  )+
  labs(
    x = "Charges to policyholders ($)",
    y = "Number of policyholders",
    title = "Distribution of medical charges"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

charges_dens <- df %>%
  ggplot(
    aes(x=charges)
  ) +
  geom_density(
    alpha=.3,
    fill="#FFC300"
  ) +
  labs(
    x = "Charges to policyholders ($)",
    y = "Probability density",
    title = "Distribution of medical charges"
  ) +
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

charges_box <- df %>%
  ggplot(aes(y=charges))+
  geom_boxplot(
    alpha=.5,
    fill="#FFC300"
  )+
  coord_flip()+
  # remove ticks from y-axis
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  labs(
    y = "Charges to policyholders ($)",
    x = "",
    title = "Distribution of medical charges"
  )+
  # resize text
  theme(
    plot.title = element_text(size=16),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

cowplot::plot_grid(
  age_hist, age_dens, age_box,
  bmi_hist, bmi_dens, bmi_box,
  charges_hist, charges_dens, charges_box,
  labels="AUTO",
  ncol = 3,
  nrow = 3
)

options(figsize)

#####

figsize <- options(repr.plot.width=12, repr.plot.height=8)

age_scatter <- df %>%
  ggplot(aes(x=age, y=charges)) +
  geom_point()+
  # add a linear regression line
  geom_smooth(method='lm')+
  labs(
    x = "Ages of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder Age"
  )+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_scatter <- df %>%
  ggplot(aes(x=bmi, y=charges)) +
  geom_point()+
  # add a linear regression line
  geom_smooth(method='lm')+
  labs(
    x = "BMI scores of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder BMI"
  )+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

cowplot::plot_grid(
  age_scatter, bmi_scatter,
  labels="AUTO",
  ncol = 2,
  nrow = 1
)

options(figsize)

#####

figsize <- options(repr.plot.width=20, repr.plot.height=22)

# by sex
age_scatter_sex <- df %>%
  ggplot(aes(x=age, y=charges, color=sex)) +
  geom_point()+
  labs(
    x = "Ages of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder Age by Sex",
    color = "Sex:"
  )+
  scale_color_hue(labels = c("male" = "Male", "female" = "Female"))+
  guides(fill=FALSE)+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_scatter_sex <- df %>%
  ggplot(aes(x=bmi, y=charges, color=sex)) +
  geom_point()+
  labs(
    x = "BMI scores of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder BMI by Sex",
    color = "Sex:"
  )+
  scale_color_hue(labels = c("male" = "Male", "female" = "Female"))+
  guides(fill=FALSE)+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

#by smoker
age_scatter_smoker <- df %>%
  ggplot(aes(x=age, y=charges, color=smoker)) +
  geom_point()+
  labs(
    x = "Ages of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder Age by Smoking",
    color = "Smoker:"
  )+
  scale_color_hue(labels = c("no" = "Non-smoker", "yes" = "Smoker"))+
  guides(fill=FALSE)+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_scatter_smoker <- df %>%
  ggplot(aes(x=bmi, y=charges, color=smoker)) +
  geom_point()+
  labs(
    x = "BMI scores of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder BMI by Smoking",
    color = "Smoking:"
  )+
  scale_color_hue(labels = c("no" = "Non-smoker", "yes" = "Smoker"))+
  guides(fill=FALSE)+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

#by children
age_scatter_kids <- df %>%
  ggplot(aes(x=age, y=charges, color=children)) +
  geom_point()+
  labs(
    x = "Ages of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder Age by Dependents",
    color = "Dependents:"
  )+
  scale_color_hue(labels = c("no" = "Non-smoker", "yes" = "Smoker"))+
  guides(fill=FALSE)+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_scatter_kids <- df %>%
  ggplot(aes(x=bmi, y=charges, color=children)) +
  geom_point()+
  labs(
    x = "BMI scores of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder BMI by Dependents",
    color = "Dependents:"
  )+
  scale_color_hue(labels = c("0" = "None"))+
  guides(fill=FALSE)+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

#by region
age_scatter_region <- df %>%
  ggplot(aes(x=age, y=charges, color=region)) +
  geom_point()+
  labs(
    x = "Ages of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder Age by Region",
    color = "Regions:"
  )+
  scale_color_hue(labels = c("northeast" = "North East", "northwest" = "North West",
                             "southeast" = "South East", "southwest" = "South West"))+
  guides(fill=FALSE)+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

bmi_scatter_region <- df %>%
  ggplot(aes(x=bmi, y=charges, color=region)) +
  geom_point()+
  labs(
    x = "BMI scores of policyholders",
    y = "Charges to policyholders ($)",
    title = "Medical Charges vs Policyholder BMI by Regions",
    color = "Regions:"
  )+
  scale_color_hue(labels = c("northeast" = "North East", "northwest" = "North West",
                             "southeast" = "South East", "southwest" = "South West"))+
  guides(fill=FALSE)+
  # resize text
  theme(
    plot.title = element_text(size=18),
    axis.text = element_text(size=14),
    axis.title = element_text(size=14)
  )

# make a grid
cowplot::plot_grid(
  age_scatter_sex, bmi_scatter_sex,
  age_scatter_smoker, bmi_scatter_smoker,
  age_scatter_kids, bmi_scatter_kids,
  age_scatter_region, bmi_scatter_region,
  labels="AUTO",
  ncol = 2,
  nrow = 4
)

options(figsize)


######
**Hypothesis-testing **
**Smoking **
  
  df %>%
  group_by(smoker) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    IQR = IQR(charges)
  ) %>%
  arrange(desc(median)) # sort by median in descending order
wilcox.test(df$charges ~ df$smoker)


df %>%
  group_by(region) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    IQR = IQR(charges)
  ) %>%
  arrange(desc(median)) # sort by median in descending order
kruskal.test(charges ~ region, data = df)



df %>%
  group_by(children) %>%
  summarise(
    count = n(),
    min = min(charges),
    median = median(charges),
    max = max(charges),
    IQR = IQR(charges)
  ) %>%
  arrange(desc(median)) # sort by median in descending order
kruskal.test(charges ~ children, data = df)
pairwise.wilcox.test(df$charges, df$children, p.adj = "BH")


# log10 transform of response variable 
df$logCharges<- log10(df$charges)

# Split the data into training and test sets
set.seed(122)                    # Set the seed to make the partition reproducible
training.samples <- df$logCharges %>%
  createDataPartition(p = 0.8, list = FALSE)
train  <- df[training.samples, ]
test <- df[-training.samples, ]
# Train the model on the training dataset
formula <- as.formula("logCharges ~ smoker + bmi + age + children + sex + region")

model <- lm(formula, data = train)

summary(model)
# Make predictions on the training dataset
predictions <- model %>% predict(train)
# Model performance
# (a) Calculating the residuals
residuals <- train$logCharges - predictions
# (b) Calculating Root Mean Squared Error
rmse <- sqrt(mean(residuals^2))

rmse %>%
  round(digits=3)
