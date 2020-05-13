Memo for Sara
================
2020-05-13

  - [Introducing the SIT-209](#introducing-the-sit-209)
  - [Our goals](#our-goals)
  - [Some problems with SIT-209 data](#some-problems-with-sit-209-data)
      - [County field](#county-field)
      - [Latitude and longitude fields](#latitude-and-longitude-fields)
      - [Duplicates](#duplicates)
      - [Inaccuracies](#inaccuracies)
      - [Omissions](#omissions)
  - [Joining to other datasets](#joining-to-other-datasets)
      - [Spatial joins](#spatial-joins)
      - [Unique ID joins](#unique-id-joins)
  - [Final thoughts](#final-thoughts)

``` r
# Libraries
library(tidyverse)
library(sf)

# Parameters

# SIT-209
file_sit209 <- here::here("data-raw", "wildfires_deduped.csv")

#===============================================================================

# Code

# Read in the SIT-209 data
sit209 <-
  file_sit209 %>% 
  read_csv(
    col_types = 
      cols(
        .default = col_character(),
        LATITUDE = col_double(),
        LONGITUDE = col_double(),
        ESTIMATED_COST = col_double(),
        REPORT_DATE = col_datetime(format = ""),
        MAX_PERSONNEL = col_double(),
        INCIDENT_AREA = col_double(),
        MAX_SFR_THREAT = col_double(),
        MAX_MFR_THREAT = col_double(),
        MAX_MU_THREAT = col_double(),
        MAX_COMM_THREAT = col_double(),
        MAX_OUTB_THREAT = col_double(),
        MAX_OTHER_THREAT = col_double(),
        SFR_DAMAGED = col_double(),
        MFR_DAMAGED = col_double(),
        MU_DAMAGED = col_double(),
        COMM_DAMAGED = col_double(),
        OTHER_DAMAGED = col_double(),
        SFR_DESTROYED = col_double(),
        MFR_DESTROYED = col_double(),
        MU_DESTROYED = col_double(),
        COMM_DESTROYED = col_double(),
        OUTB_DESTROYED = col_double(),
        OTHER_DESTROYED = col_double(),
        INJURIES = col_double(),
        FATALITIES = col_double(),
        REPORT_ID = col_character(),
        WILDFIRE = col_logical(),
        STATE_FIPS = col_character(),
        DESTROYED = col_logical(),
        THREATENED = col_logical(),
        CLIMATE_REGION = col_character()
      )
  )
```

## Introducing the SIT-209

The primary dataset we’re working with is derived from ICS-209 reports,
which are filed by fire agencies during wildfires and other disasters.
The database is a crucial way for wildfire responders to share
information about a given wildifre, including the costs of fire
suppression, the resources that have been used to get the fire under
control, the fire’s size, the property the fire has damaged or destroyed
and the outlook for the fire’s spread going forward. Incident reports
are not filed for every fire in the U.S., but for large wildfires on
federal land and some other fires, depending on the size of the fire and
the state.

The raw archived data can be found [here](https://fam.nwcg.gov/fam-web/)
in the form of Microsoft Access databases covering the years between
1999 and 2018. Technically, the archive includes information drawn from
two different kinds of reports, daily incident reports and dispatcher
reports. There is [some
documentation](https://gacc.nifc.gov/predictive_services/intelligence/niop/programs/sit_209/Help/index.htm)
available to describe SIT-209, but in general, little information about
the data archive is available even though the format of the data has
changed several times over the years.

This quarter, we’ve been working with a table of aggregated statistics
compiled by Big Local News journalist Eric Sagara, in which each record
represents a different fire. This table, [available for download on the
Big Local News platform](https://biglocalnews.org), includes just a
subset of the many fields recorded in the original SIT-209 data.

## Our goals

As I see it, there are two fundamental and interelated problems we have
faced while working with SIT-209:

  - Identifying unique fire incidents in a reasonable and consistent way
  - Joining SIT-209 to other datasets.

Without solving these problems for all or part of these datasets, it’s
difficult to accurately calculate even basic summary statistics, such as
the number of homes destroyed per fire in each county in the country, or
to characterize the nature of places that suffer destructive fires.

## Some problems with SIT-209 data

Our understanding is that SIT-209 is the most comprehensive database
tracking critical statistics about wildfires, including the number of
homes destroyed in significant fire incidents and the costs of fire
suppression. However, a number of omissions, inaccuracies and lack of
geographic clarity can make it difficult to wrangle.

To start, whenever I work with this data, I usually start by making a
few changes:

  - I filter to wildfire incidents only, since some non-wildfires are
    included in the data.
  - I transform latitude and longitude to correct any points that need
    to be standardized.
  - I attempt to standardize the COUNTY and STATE\_FIPS columns to
    correspond to other datasets with FIPS codes and county names. (Note
    that other transformations are needed in order to generate uniform
    county names and county FIPS codes, as described further below.)

<!-- end list -->

``` r
sit209_prepped <-
  sit209 %>% 
  filter(WILDFIRE == TRUE) %>% 
  mutate( # Standardized latitude and longitude into the correct hemisphere
    LATITUDE = if_else(LATITUDE < 0, LATITUDE * -1, LATITUDE),
    LONGITUDE = if_else(LONGITUDE > 0, LONGITUDE * -1, LONGITUDE),
    COUNTY = str_to_title(COUNTY),
    STATE_FIPS = map(STATE_FIPS, ~ if_else(length(.) < 2, str_c("0", .),.)),
    STATE_FIPS = pluck(STATE_FIPS, 1)
  ) 
```

### County field

The county field, as far as I can tell, refers to either the county
where the fire started, the county where it was primarily located or,
sometimes, more than one county.

It’s missing from 3,939 records, although upon further inspection, this
might not be such a huge problem if we only focus on destructive fires
only.

``` r
sit209 %>% count(is.na(COUNTY))
```

    ## # A tibble: 2 x 2
    ##   `is.na(COUNTY)`     n
    ##   <lgl>           <int>
    ## 1 FALSE           29702
    ## 2 TRUE             3939

But for destructive fires:

``` r
sit209 %>% filter(DESTROYED == TRUE) %>% count(is.na(COUNTY))
```

    ## # A tibble: 2 x 2
    ##   `is.na(COUNTY)`     n
    ##   <lgl>           <int>
    ## 1 FALSE            4725
    ## 2 TRUE              257

When county is recorded in the data, it’s either in the form of the
county name or the fips codes. In a few cases, more than one county is
listed.

``` r
sit209 %>% count(COUNTY, STATE, sort = TRUE) %>% drop_na(COUNTY)
```

    ## # A tibble: 4,083 x 3
    ##    COUNTY         STATE     n
    ##    <chr>          <chr> <int>
    ##  1 RIVERSIDE      CA      412
    ##  2 IDAHO          ID      367
    ##  3 290.0          AK      296
    ##  4 113.0          OK      208
    ##  5 PITTSBURG      OK      208
    ##  6 SAN DIEGO      CA      204
    ##  7 49.0           ID      173
    ##  8 COCONINO       AZ      167
    ##  9 SAN BERNARDINO CA      162
    ## 10 LATIMER        OK      161
    ## # … with 4,073 more rows

### Latitude and longitude fields

In addition to `COUNTY` and `STATE`, the other major location fields in
the data are the latitude and longitude points. Like the county field,
these points ostensibly refer to the fire’s “point of origin” – that is,
where it started. That said, when we’ve looked at specific points, it
seems possible that the point of origin is not, in fact, where the fire
started but the location of the person who first reported seeing the
fire.

As noted above, the points are frequently in the wrong hemisphere, but
can be corrected by flipping them into the hemisphere that contains U.S.
states. A larger problem are the cases in which the points are missing
or incorrect.

Here are the missing points, although they are few:

``` r
sit209 %>% count(is.na(LATITUDE)|is.na(LONGITUDE))
```

    ## # A tibble: 2 x 2
    ##   `is.na(LATITUDE) | is.na(LONGITUDE)`     n
    ##   <lgl>                                <int>
    ## 1 FALSE                                32519
    ## 2 TRUE                                  1122

And here’s an example of some points that are supposed to refer to fires
in California, but refer to points located outside of the state
according to this geospatial analysis:

``` r
sit209_prepped %>% 
  filter(STATE == "CA") %>% 
  # Use lat/long to make an sf object for spatial operations
  drop_na(LONGITUDE, LATITUDE) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269) %>% 
  st_difference(
    ussf::boundaries(geography = c("state"), projection = c("longlat")) %>% 
    st_transform(crs = 4269) %>% 
    filter(STATEFP == "06")
  ) %>% 
  count(INCIDENT_NAME, INCIDENT_NUMBER, sort = TRUE)
```

    ## although coordinates are longitude/latitude, st_difference assumes that they are planar

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

    ## Simple feature collection with 159 features and 3 fields
    ## geometry type:  GEOMETRY
    ## dimension:      XY
    ## bbox:           xmin: -177.2667 ymin: 3.035 xmax: -0.8025 ymax: 66.94611
    ## epsg (SRID):    4269
    ## proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs
    ## # A tibble: 159 x 4
    ##    INCIDENT_NAME     INCIDENT_NUMBER     n                              geometry
    ##    <chr>             <chr>           <int>                        <GEOMETRY [°]>
    ##  1 BARRETT           CA-MVU-001494       2 MULTIPOINT ((-116.6858 32.11694), (-…
    ##  2 BUZZ              CA-NOD-2261         2 MULTIPOINT ((-119.865 41.415), (-119…
    ##  3 ALAMO             007624              1            POINT (-82.32333 30.57444)
    ##  4 ALPHA 1           000928              1              POINT (-119.5132 45.051)
    ##  5 APACHE            000994              1            POINT (-84.25111 36.15278)
    ##  6 ATLAS             010046              1            POINT (-112.6906 47.02083)
    ##  7 BAJA CALIFORNIA-… CA-MVU-5394         1            POINT (-115.3167 30.86667)
    ##  8 BARREL            CA-NOD-3444         1            POINT (-119.9167 41.93667)
    ##  9 BATTLE            000415              1            POINT (-105.0982 36.79217)
    ## 10 BEALE             021927              1            POINT (-93.25139 35.55833)
    ## # … with 149 more rows

### Duplicates

Even though the table we’re working with has been aggregated and
de-duplicated, it’s important to note that there is no unique
identifier, so far as I know, in the table.

`INTERNAL_DB_ID` is missing from many records, although it is not
repeated in the dataset where it does exist:

``` r
sit209 %>% 
  count(INTERNAL_DB_ID, sort = TRUE) %>% 
  filter(n > 1)
```

    ## # A tibble: 1 x 2
    ##   INTERNAL_DB_ID     n
    ##   <chr>          <int>
    ## 1 <NA>           24649

Fires with the same `REPORT_ID` appear multiple times in the data:

``` r
sit209 %>% 
  count(REPORT_ID, sort = TRUE) %>% 
  filter(n > 1)
```

    ## # A tibble: 955 x 2
    ##    REPORT_ID         n
    ##    <chr>         <int>
    ##  1 AR-ARS-D7         6
    ##  2 LA-KIF-           4
    ##  3 MA-MAS-000001     4
    ##  4 MI-MIS-001        4
    ##  5 MN-MNS-           4
    ##  6 MO-MTF-000045     4
    ##  7 MO-MTF-000050     4
    ##  8 ND-NDS-001        4
    ##  9 NM-GNF-005        4
    ## 10 NM-N5S-           4
    ## # … with 945 more rows

Even combinations of `INCIDENT_NUMBER`, `COUNTY` and `REPORT_DATE` are
not unique.

``` r
sit209 %>% 
  count(INCIDENT_NUMBER, COUNTY, REPORT_DATE, sort = TRUE) %>% 
  filter(n > 1)
```

    ## # A tibble: 53 x 4
    ##    INCIDENT_NUMBER COUNTY REPORT_DATE             n
    ##    <chr>           <chr>  <dttm>              <int>
    ##  1 000122          27.0   2016-04-08 09:00:00     2
    ##  2 000160          1.0    2016-06-23 17:00:00     2
    ##  3 000163          1.0    2015-10-27 07:00:00     2
    ##  4 000207          71.0   2015-09-07 22:00:00     2
    ##  5 000224          240.0  2015-07-27 23:00:00     2
    ##  6 000336          139.0  2016-11-25 16:00:00     2
    ##  7 000573          290.0  2015-07-28 03:00:00     2
    ##  8 000800          79.0   2015-09-15 17:00:00     2
    ##  9 001046          375.0  2016-12-30 22:00:00     2
    ## 10 001159          1.0    2015-10-19 20:00:00     2
    ## # … with 43 more rows

For this reason, knowing which records to keep and which records to
discard has been a challenge. Determining what kind of an incident
should count as a unique fire has also been confusing at times.

### Inaccuracies

Data quality is also suspect at times. Even though some of the following
examples can be weeded out because they are outliers, other inaccuracies
are plausibly incorrect and therefore harder to identify, given that
most fires in SIT-209 are small but a few are extremely large and
costly.

``` r
sit209 %>% 
  filter(WILDFIRE == TRUE) %>% 
  arrange(desc(INCIDENT_AREA)) %>% 
  head(100) %>% 
  filter(DESTROYED == FALSE)
```

    ## # A tibble: 54 x 42
    ##    REPORT_YEAR FORM_TYPE INTERNAL_DB_ID INCIDENT_NUMBER INCIDENT_NAME
    ##    <chr>       <chr>     <chr>          <chr>           <chr>        
    ##  1 2006        b         <NA>           CA-SQF-2511     BRODER/BECK  
    ##  2 2012        b         <NA>           OR-VAD-193      JUNIPER CREEK
    ##  3 2009        b         <NA>           CA-YNP-2968     WILDCAT      
    ##  4 2013        c         <NA>           CO-RMP-217      BIG MEADOWS  
    ##  5 2011        b         <NA>           AZ-NAA-0023     SHIPROCK-5   
    ##  6 2012        b         <NA>           WY-SH1X-12192   PUSSAIC COMP…
    ##  7 2003        b         <NA>           WY-MB2-081      GRAMM COMPLEX
    ##  8 2008        b         <NA>           CA-INF-282      HONEY BEE    
    ##  9 2011        b         <NA>           NV-EKD-001337   CHUKKAR CANY…
    ## 10 2002        b         <NA>           WY-BTF-014      MULE         
    ## # … with 44 more rows, and 37 more variables: INCIDENT_TYPE <chr>, STATE <chr>,
    ## #   COUNTY <chr>, LATITUDE <dbl>, LONGITUDE <dbl>, START_DATE <chr>,
    ## #   END_DATE <chr>, CAUSE <chr>, ESTIMATED_COST <dbl>, REPORT_DATE <dttm>,
    ## #   MAX_PERSONNEL <dbl>, INCIDENT_AREA <dbl>, MAX_SFR_THREAT <dbl>,
    ## #   MAX_MFR_THREAT <dbl>, MAX_MU_THREAT <dbl>, MAX_COMM_THREAT <dbl>,
    ## #   MAX_OUTB_THREAT <dbl>, MAX_OTHER_THREAT <dbl>, SFR_DAMAGED <dbl>,
    ## #   MFR_DAMAGED <dbl>, MU_DAMAGED <dbl>, COMM_DAMAGED <dbl>,
    ## #   OTHER_DAMAGED <dbl>, SFR_DESTROYED <dbl>, MFR_DESTROYED <dbl>,
    ## #   MU_DESTROYED <dbl>, COMM_DESTROYED <dbl>, OUTB_DESTROYED <dbl>,
    ## #   OTHER_DESTROYED <dbl>, INJURIES <dbl>, FATALITIES <dbl>, REPORT_ID <chr>,
    ## #   WILDFIRE <lgl>, STATE_FIPS <chr>, DESTROYED <lgl>, THREATENED <lgl>,
    ## #   CLIMATE_REGION <chr>

Consider incidents CA-SQF-2511, OR-VAD-193 and CA-YNP-2968. Each of
these list the wrong acreage, which you can discover by Googling and
finding the original reports used to feed the SIT-209 database. Here’s
[CA-YNP-2968](https://fam.nwcg.gov/fam-web/hist_209/hist_r_209_cheetah_data_2009?button=Southern+California&v_gaid=SO&v_209_number=CA-YNP-2968)
as an example.

It’s not just the largest fires. Problems persist for slightly smaller
fires in this ranking. See CA-MVU-0110025, where the estimated acreage =
fire final cost even though, from this form, [we can see that’s
incorrect.](https://fam.nwcg.gov/fam-web/hist_209/hist_r_print_209_head_2013?v_number=CA-MVU-011025&v_report_date=05/31/2013&v_hour=1700&v_gaid=SO)

There are other totally goofy data-entry issues, like these cases in
which someone added an extra digit at the end of the acreage entry:

  - [AK-UYD-000359](https://fam.nwcg.gov/fam-web/hist_209/hist_r_print_209_head_2009?v_number=AK-UYD-000359&v_report_date=10/01/2009&v_hour=0927&v_gaid=AK)

  - [NV-EKD-000682](https://fam.nwcg.gov/fam-web/hist_209/hist_r_209_cheetah_data_2007?button=Western+Great+Basin&v_gaid=WB&v_209_number=NV-EKD-000682)

It seems to me, from some spot checks, that data quality beginning in
2014 is better. The data is also more complete for those years. Finally,
my hunch is that we might be better off ranking fires by destructiveness
as opposed to acreage, since my spot checks have found no major
inaccuracies in that field when it is included.

### Omissions

Records of some of the most-significant fires are often missing critical
information. Consider the 100 largest fires in our SIT-209 table by
acreage:

``` r
sit209 %>% 
  filter(WILDFIRE == TRUE) %>% 
  arrange(desc(INCIDENT_AREA)) %>% 
  head(100) %>% 
  summarize_all(~ sum(is.na(.))) %>% 
  select_if(. > 0)
```

    ## # A tibble: 1 x 27
    ##   INTERNAL_DB_ID COUNTY LATITUDE LONGITUDE END_DATE ESTIMATED_COST MAX_PERSONNEL
    ##            <int>  <int>    <int>     <int>    <int>          <int>         <int>
    ## 1             78     24        1         1       31             31             6
    ## # … with 20 more variables: MAX_SFR_THREAT <int>, MAX_MFR_THREAT <int>,
    ## #   MAX_MU_THREAT <int>, MAX_COMM_THREAT <int>, MAX_OUTB_THREAT <int>,
    ## #   MAX_OTHER_THREAT <int>, SFR_DAMAGED <int>, MFR_DAMAGED <int>,
    ## #   MU_DAMAGED <int>, COMM_DAMAGED <int>, OTHER_DAMAGED <int>,
    ## #   SFR_DESTROYED <int>, MFR_DESTROYED <int>, MU_DESTROYED <int>,
    ## #   COMM_DESTROYED <int>, OUTB_DESTROYED <int>, OTHER_DESTROYED <int>,
    ## #   INJURIES <int>, FATALITIES <int>, CLIMATE_REGION <int>

In addition to the inaccuracies noted above, which mean these aren’t
actually the largest fires, these records are missing enough information
to make it difficult gauge whether they are plausible values or
data-entry errors. For example:

  - 31 of the fires do not have estimated cost
  - 6 do not estimate the maximum personnel fighting the fire
  - 33 do not estimate the number of single-family homes threatened
  - 96 do not estimate the number of multifamily homes threatened

## Joining to other datasets

There are many other rich sources of wildfire data and demographic data
that we’d love to overlay on top of our SIT-209 data in order to pose
questions about where fires are the most-destructive and costly. But
given the limitations above, we’ve had trouble finding valid ways to
join SIT-209 to other datasets. In general, our approach has been to
attempt to identify SIT-209 fires in a fire perimeters database, and
then spatially join this combined dataset with other information we’d
like to know about the places that have suffered fires.

There are a few wildfire-related datasets we’ve aimed to use for this
purpose:

  - Monitoring Trends in Burn Severity: Burned Areas Boundary Dataset,
    or [MTBS](https://www.mtbs.gov/direct-download) - A fire perimeter
    dataset, which does not sharea common ID with SIT-209.
  - Data collected by a Forest Service researcher, available for
    download
    [here](https://www.fs.usda.gov/rds/archive/catalog/RDS-2013-0009.4),
    which includes a crosswalk between some SIT-209 data and MTBS.
  - GEOMAC - A more comprehensive fire perimeters database than MTBS.
    It’s quite extensive, and we’ve not explored it thoroughly. It
    might have a crosswalk to SIT-209, but if it does, we’ve not found
    it.
  - FRAP - CALFIRE’s database of fire perimeters, and plenty of other
    interesting datasets, including a fire risk map. [Available
    here.](https://frap.fire.ca.gov/frap-projects/fire-perimeters/)

In many cases, in order to minimize these problems and other
variability, we have tried to join only a subset of SIT-209 – for
example, just fires in California, and only for a subset of years, and
only if they destroyed one or more structures.

### Spatial joins

The most significant pitfall of any spatial join is that we can have
limited confidence in the accuracy and interpretation of location data
in in SIT-209. (For example, does county refer to the county where the
fire was primarily located or the county where it started? What does
latitude and longitude mean, and is it even accurate? We won’t know
without spot-checking the records we join.) We have nonetheless used the
following procedures to attempt to join SIT-209 to different
geographies:

  - **California-only join**: We spatially joined FRAP perimeter data to
    annual Census tract data downloaded from IPUMS, and then joined this
    dataset spatially to SIT-209. (Please note that this join had to be
    executed in many small steps, as a result of computational
    limitations…ideally it could be done in 2-3 pipes.) The result was
    not promising. [See this notebook for further
    detail](https://github.com/DiPierro/wildfires-2020/blob/master/eda/fires-2020-05-07.Rmd).
    (Sorry it’s not knitted. My computer crashed when I attempted to
    knit.)

  - **Hybrid approach**: In [this
    notebook](https://github.com/DiPierro/wildfires-2020/blob/master/eda/sit209-%2B-recovery.md),
    I attempted to assign each fire in the SIT-209 to a county first
    using latitude and longitude, and then used the county field for
    remaining fires. In this case, I limited myself to only fires that
    destroyed structures.

### Unique ID joins

Another option is finding, or creating, a unique identifier that would
allow us to join SIT-209 data to fire perimeter data. Here’s how we’ve
attempted this kind of join.

  - In [this
    notebook](https://github.com/DiPierro/wildfires-2020/blob/master/eda/counties-2020-04-01.md),
    I attempted to use the crosswalk data from the Forest Service
    researcher Karen Short to combine MTBS fire perimeters with SIT-209
    data. However, upon further inspection, the datasets had a many to
    many relationship which was difficult to untangle. Some SIT-209
    fires appeared connected to multiple MTBS IDs ; some MTBS fires
    appeared to be assigned to multiple SIT-209 fires. We’re attempting
    to contact Short in order to understand what’s really happening.
    This crosswalk seems like one of the most promising approaches we
    can take, given the lack of clarity in SIT-209 location data.

  - At the very end of the end of [this
    notebook](https://github.com/DiPierro/wildfires-2020/blob/master/eda/fires-2020-05-07.Rmd),
    I made some attempts to create a unique ID that would connect FRAP
    data to SIT-209 data (ideally, some combination of geography and
    date-time would do the trick), but met with little success. In
    particular, more detailed de-duplicating of both the FRAP data and
    the SIT-209 data would be necessary in order of this approach to
    work, and even then, it’s possible that small differences in the two
    datasets (for example, if the date recorded in FRAP is a day later
    or earlier than the date in SIT-209) would prevent us from finding
    matches.

## Final thoughts

Thanks so much for your help\! Please don’t hesitate to call / email /
Slack us with questions. We’re looking forward to working with you\!
