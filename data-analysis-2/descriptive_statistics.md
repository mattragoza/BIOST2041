M2 Data Analysis: Descriptive Statistics
================
Matthew Ragoza
9/18/2021

-   [Obstetrics and Periodontal Therapy (OPT) study
    dataset](#obstetrics-and-periodontal-therapy-opt-study-dataset)
    -   [Treatment group](#treatment-group)
    -   [Infant birthweight](#infant-birthweight)
    -   [Age of pregnant individual at
        baseline](#age-of-pregnant-individual-at-baseline)
    -   [Preterm birth](#preterm-birth)
    -   [Maximum and minimum infant
        birthweight](#maximum-and-minimum-infant-birthweight)
    -   [Relationship between preterm birth and treatment
        group](#relationship-between-preterm-birth-and-treatment-group)
    -   [Relationship between preterm birth and age of pregnant
        individual](#relationship-between-preterm-birth-and-age-of-pregnant-individual)
    -   [Relationship between age of pregnant individual and infant
        birthweight](#relationship-between-age-of-pregnant-individual-and-infant-birthweight)

## Obstetrics and Periodontal Therapy (OPT) study dataset

``` r
load('OPT.RData')
```

### Treatment group

Treatment group is categorical, so its distribution is shown in Figure 1
as a bar chart, and the frequency and relative frequency of each
treatment group category are listed in Table 1. As expected based on the
study description, there were 410 pregnant individuals in the control
group (49.8%) and 413 in the treatment group (50.2%), amounting to a
total of N=823.

**Figure 1.** Visualization of treatment group as bar chart

``` r
ggplot(opt %>%
    drop_na(Group),
    aes(x=Group)
) + geom_bar() + theme_classic() + ylab('Count') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
```

![](descriptive_statistics_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

**Table 1.** Numerical summary of treatment group as frequencies (Count)
and relative frequencies (Percent)

``` r
kable(opt %>%
    group_by(Group) %>%
    summarize(Count=n()) %>%
    mutate(Percent=Count/sum(Count)*100) %>%
    adorn_totals()
)
```

| Group     | Count |   Percent |
|:----------|------:|----------:|
| Control   |   410 |  49.81774 |
| Treatment |   413 |  50.18226 |
| Total     |   823 | 100.00000 |

### Infant birthweight

The next variable of interest is infant birthweight in grams, which is
quantitative. Therefore, it is visualized in Figure 2 as a histogram and
numerically summarized in Table 2 using measures of center and spread.
The distribution of birthweight is unimodal, and the fact that the mean
(3248) is less than the median (3280) suggests that the distribution is
slightly left-skewed. This is also supported by the longer left tail
than right tail seen in Figure 2. As such, the median is a better
estimate of central tendency than the mean, which is biased towards the
low outliers. Furthermore, the standard deviation is 580 and the IQR is
595.

**Figure 2.** Visualization of infant birthweight as histogram

``` r
ggplot(opt %>%
    drop_na(Birthweight),
    aes(x=Birthweight)
) + geom_histogram() + theme_classic() + xlab('Birthweight (g)') + ylab('Count')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](descriptive_statistics_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Table 2.** Numerical summary of infant birthweight using measures of
center and spread

``` r
kable(opt %>%
    summarize(
        N=n(),
        N_NA=sum(is.na(Birthweight)),
        Mean=mean(Birthweight, na.rm=TRUE),
        SD=sd(Birthweight, na.rm=TRUE),
        Min=min(Birthweight, na.rm=TRUE),
        Pct_25=quantile(Birthweight, 0.25, na.rm=TRUE),
        Median=median(Birthweight, na.rm=TRUE),
        Pct_75=quantile(Birthweight, 0.75, na.rm=TRUE),
        Max=max(Birthweight, na.rm=TRUE),
        IQR=quantile(Birthweight, 0.75, na.rm=TRUE)-quantile(Birthweight, 0.25, na.rm=TRUE),
        Range=max(Birthweight, na.rm=TRUE)-min(Birthweight, na.rm=TRUE),
    )
)
```

|   N | N_NA |     Mean |       SD | Min | Pct_25 | Median | Pct_75 |  Max | IQR | Range |
|----:|-----:|---------:|---------:|----:|-------:|-------:|-------:|-----:|----:|------:|
| 823 |   30 | 3248.288 | 579.9857 | 450 |   2985 |   3280 |   3580 | 5160 | 595 |  4710 |

### Age of pregnant individual at baseline

Next we examine the age of the pregnant individual in years at baseline.
This variable is also quantitative, so its distribution is seen in
Figure 3 as a histogram and summarized in Table 3 using measures of
center and spread. This time, viewing the shape in Figure 3 reveals a
unimodal right-skewed distribution, which is also indicated by the mean
(26) being greater than the median (25). In addition, the standard
deviation of birthweight is 5.6 and the IQR is 8.

**Figure 3.** Visualization of age of pregnant individual at baseline as
histogram

``` r
ggplot(opt %>%
    drop_na(Age),
    aes(x=Age)
) + geom_histogram() + theme_classic() + xlab('Age (years)') + ylab('Count')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](descriptive_statistics_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Table 3.** Numerical summary of age of pregnant individual at baseline
using measures of center and spread

``` r
kable(opt %>%
    summarize(
        N=n(),
        Mean=mean(Age, na.rm=TRUE),
        SD=sd(Age, na.rm=TRUE),
        Min=min(Age, na.rm=TRUE),
        Pct_25=quantile(Age, 0.25, na.rm=TRUE),
        Median=median(Age, na.rm=TRUE),
        Pct_75=quantile(Age, 0.75, na.rm=TRUE),
        Max=max(Age, na.rm=TRUE),
        IQR=quantile(Age, 0.75, na.rm=TRUE)-quantile(Age, 0.25, na.rm=TRUE),
        Range=max(Age, na.rm=TRUE)-min(Age, na.rm=TRUE),
    )
)
```

|   N |     Mean |       SD | Min | Pct_25 | Median | Pct_75 | Max | IQR | Range |
|----:|---------:|---------:|----:|-------:|-------:|-------:|----:|----:|------:|
| 823 | 25.97813 | 5.565973 |  16 |     22 |     25 |     30 |  44 |   8 |    28 |

### Preterm birth

The fourth variable to analyze is preterm birth, which is coded
categorically. As such, it is visualized in Figure 4 as a bar chart and
described numerically in Table 4 using frequencies and relative
frequencies of the categories. Out of the 823 total pregnancies in the
study, 82 were preterm births (10.0%), 711 were not preterm (86.4%), and
30 were missing values (3.6%).

**Figure 4.** Visualization of preterm birth as bar chart

``` r
ggplot(opt %>%
    drop_na(Preterm),
    aes(x=Preterm)
) + geom_bar() + theme_classic() + ylab('Count')
```

![](descriptive_statistics_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

**Table 4.** Numerical summary of preterm birth as frequencies (Count)
and relative frequencies (Percent)

``` r
kable(opt %>%
    group_by(Preterm) %>%
    summarize(Count=n()) %>%
    mutate(Percent=Count/sum(Count)*100) %>%
    adorn_totals()
)
```

| Preterm | Count |    Percent |
|:--------|------:|-----------:|
| N       |   711 |  86.391251 |
| Y       |    82 |   9.963548 |
| NA      |    30 |   3.645201 |
| Total   |   823 | 100.000000 |

### Maximum and minimum infant birthweight

As seen in Table 5, the maximum observed infant birthweight was 5160g,
and the minimum birthweight was 450g. These values correspond to
z-scores of 3.3 and -4.8, respectively. This means that the maximum
birthweight was 3.3 standard deviations above the mean of 3248, while
the minimum birthweight was 4.8 standard deviations below the mean. This
further supports that the birthweight distribution is left-skewed, since
the lowest observed value was more extreme than the highest observed
value.

**Table 5.** Z-scores for maximum and minimum infant birthweight

``` r
kable(opt %>%
    summarize(
        Min=min(Birthweight, na.rm=TRUE),
        Max=max(Birthweight, na.rm=TRUE),
        Mean=mean(Birthweight, na.rm=TRUE),
        SD=sd(Birthweight, na.rm=TRUE),
    ) %>%
    gather(' ', 'Value', Min, Max) %>%
    mutate(Z_Score=(Value-Mean)/SD) %>%
    subset(select=c(' ', 'Value', 'Z_Score'))
)
```

|     | Value |   Z_Score |
|:----|------:|----------:|
| Min |   450 | -4.824753 |
| Max |  5160 |  3.296137 |

### Relationship between preterm birth and treatment group

To analyze the relationship between preterm birth and treatment group,
which are both categorical, it is most appropriate to consider the
proportion of subjects in each group who had preterm births, which can
be seen in Table 6. Since there were 38 preterm births in the control
group of 391 total pregnant individuals, 9.7% of the control group had
preterm births. Since there were 44 preterm births in the treatment
group of total size 402, that means 10.9% of the treatment group had
preterm births. Therefore, the treatment group had a rate of preterm
births that was 1.2 percentage points higher than the control group.

I decided that this relationship would be best visualized using stacked
bar charts of the group proportions, where each bar has a total of 100%.
This is because the relative frequencies of preterm births within each
group can be easily compared side-by-side, whereas if I had used a
stacked or grouped bar chart of simple counts or proportions of the
total sample, it would be harder to see the difference in preterm birth
rates between the treatment groups, since the size of the treatment
groups are slightly different.

**Figure 5.** Visualization of relationship between preterm birth and
treatment group using stacked bar charts of group proportions

``` r
ggplot(opt %>%
    drop_na(Preterm) %>%
    group_by(Group, Preterm) %>%
    summarize(Proportion=n(), .groups='drop'),
    aes(x=Group, y=Proportion, fill=Preterm)
) + geom_bar(stat='identity', position='fill') + theme_classic() + scale_y_continuous(labels=percent_format())
```

![](descriptive_statistics_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

**Table 6.** Numerical summary of relationship between preterm birth and
treatment group using counts (N, Y, Total) and proportions (Pct_N,
Pct_Y, Pct_total) of preterm births within each treatment group

``` r
kable(opt %>%
    drop_na(Preterm) %>%
    group_by(Group, Preterm) %>%
    summarize(Count=n(), .groups='drop') %>%
    spread(key=Preterm, value=Count) %>%
    mutate(Total=Y+N) %>%
    mutate(Pct_N=N/Total*100, Pct_Y=Y/Total*100, Pct_Total=(N+Y)/Total*100)
)
```

| Group     |   N |   Y | Total |    Pct_N |    Pct_Y | Pct_Total |
|:----------|----:|----:|------:|---------:|---------:|----------:|
| Control   | 353 |  38 |   391 | 90.28133 |  9.71867 |       100 |
| Treatment | 358 |  44 |   402 | 89.05473 | 10.94527 |       100 |

### Relationship between preterm birth and age of pregnant individual

To understand the relationship between preterm birth, which is
categorical, and the age of the pregnant individual, which is
quantitative, we should look at differences in measures of center and
spread of the distributions of age for each category. These measures for
those with and without preterm births are shown numerically in Table 7.

Since it has been shown previously that the age distribution is
right-skewed, the optimal measure of center is the median, since the
mean will be biased by the high outliers. The median age of individuals
who had preterm births was 27, compared with a median age of 25 for
those that did not have preterm births. Therefore, the median age of
those with preterm births was 2 years greater than those without preterm
births. In addition, the IQR of the age of those who had preterm births
was 8.75 compared with 7.5 for those who did not, which indicates a
wider spread of the middle 50% of the age distribution by 1.25 years.
These differences in median and IQR suggest that the distribution of age
of those who have preterm births is both higher, and spans a wider
range, then those who do not have preterm births.

To visualize these distributional differences, I decided to display
side-by-side box plots in Figure 6, since this allows the medians and
IQRs of the two distributions to be readily compared.

**Figure 6.** Visualization of relationship between preterm birth and
age of pregnant individual as side-by-side box plots

``` r
ggplot(opt %>%
    drop_na(Preterm),
    aes(x=Age, y=Preterm)
) + geom_boxplot() + theme_classic()
```

![](descriptive_statistics_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

**Table 7.** Numerical summary of relationship between preterm birth and
age of pregnant individual using measures of center and spread of the
age distribution for those with or without preterm births

``` r
kable(opt %>%
    drop_na(Preterm) %>%
    group_by(Preterm) %>%
    summarize(
        N=n(),
        Mean=mean(Age, na.rm=TRUE),
        SD=sd(Age, na.rm=TRUE),
        Min=min(Age, na.rm=TRUE),
        Pct_25=quantile(Age, 0.25, na.rm=TRUE),
        Median=median(Age, na.rm=TRUE),
        Pct_75=quantile(Age, 0.75, na.rm=TRUE),
        Max=max(Age, na.rm=TRUE),
        IQR=quantile(Age, 0.75, na.rm=TRUE)-quantile(Age, 0.25, na.rm=TRUE),
        Range=max(Age, na.rm=TRUE)-min(Age, na.rm=TRUE),
    )
)
```

| Preterm |   N |     Mean |       SD | Min | Pct_25 | Median | Pct_75 | Max |  IQR | Range |
|:--------|----:|---------:|---------:|----:|-------:|-------:|-------:|----:|-----:|------:|
| N       | 711 | 25.81997 | 5.494550 |  16 |     22 |     25 |  29.50 |  44 | 7.50 |    28 |
| Y       |  82 | 27.54878 | 6.096249 |  18 |     23 |     27 |  31.75 |  44 | 8.75 |    26 |

### Relationship between age of pregnant individual and infant birthweight

The last relationship to analyze is between the age of the pregnant
individual and the infant birthweight, which are both quantitative. Thus
the relationship can be best visualized as a scatter plot, seen in
Figure 7. Since there were many pregnant individuals with nearly the
same age and infant birthweight, many of the points in the scatter plot
overlap, making it diffuclt to see clear patterns. To better display the
data, I decided to show the points with some tranparency, so that
darkness provides an indication of the number of overlapping points.
However, even this did not reveal a clear positive or negative
assocation between the points, suggesting that there would be little
correlation between the variables. To quantify this relationship, the
actual correlation was computed and displayed in Table 8. The
correlation between age and birthweight turned out to be just 0.04,
indicating virtually no correlation.

**Figure 7.** Visualization of relationship between age of pregnant
individual and infant birthweight as scatter plot

``` r
ggplot(opt %>%
    drop_na(Preterm),
    aes(x=Age, y=Birthweight)
) + geom_point(alpha=1/8) + theme_classic()
```

![](descriptive_statistics_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

**Table 8.** Numerical description of relationship between age of
pregnant individual and infant birthweight using correlation

``` r
kable(opt[c('Age', 'Birthweight')] %>%
    cor(use='complete.obs')
)
```

|             |       Age | Birthweight |
|:------------|----------:|------------:|
| Age         | 1.0000000 |   0.0381046 |
| Birthweight | 0.0381046 |   1.0000000 |
