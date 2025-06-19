



```r
records_wide <- readRDS("../sober_rubric/raw_data/preprocessed_records.rds")
psyctests_info <- readRDS("../sober_rubric/raw_data/psyctests_info.rds")
psyctests_info <- psyctests_info %>% 
  left_join(records_wide %>% select(DOI, Acronym = first_acronym), by = "DOI") %>% 
  mutate(shortName = coalesce(Acronym, Name)) %>% 
  mutate(shortName = case_when(
    Name == "trail making test" ~ "TMT",
    Name == "alcohol use disorders identification test" ~ "AUDIT",
    Name == "perceived stress scale" ~ "PSS",
    Name == "perceived stress scale" ~ "PSS",
    Name == "beck anxiety inventory" ~ "BAI",
    Name == "positive and negative affect scale" ~ "PANAS-B",
    Name == "center for epidemiological studies depression scale" ~ "CESD",
    Name == "stroop color and word test" ~ "SCWT",
    Name == "clinician-administered ptsd scale" ~ "CAPS",
    shortName == "WHO WMH-CIDI" ~ "WMH-CIDI",
    Name == "barthel index" ~ "ADL",
    Name == "stroop color and word test" ~ "SCWT",
    TRUE ~ shortName
  ))
```

## By subdiscipline

```r
psyctests_info %>% group_by(subdiscipline_1, DOI, shortName, Name) %>%
  summarise(usage_count = sum(usage_count, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(usage_count)) %>% 
  head(50) %>% 
  kable()
```



|subdiscipline_1                          |DOI                |shortName   |Name                                                | usage_count|
|:----------------------------------------|:------------------|:-----------|:---------------------------------------------------|-----------:|
|Health and Clinical Psychology           |10.1037/t00741-000 |BDI         |beck depression inventory                           |       25118|
|Health and Clinical Psychology           |10.1037/t04100-000 |HRSD        |hamilton rating scale for depression                |       18484|
|Health and Clinical Psychology           |10.1037/t00742-000 |BDI-II      |beck depression inventory-ii                        |       14268|
|Health and Clinical Psychology           |10.1037/t05056-000 |PANSS       |positive and negative syndrome scale                |       13316|
|Health and Clinical Psychology           |10.1037/t03589-000 |HADS        |hospital anxiety and depression scale               |       12134|
|Personality and Social Psychology        |10.1037/t01038-000 |RSES        |rosenberg self-esteem scale                         |       10896|
|Health and Clinical Psychology           |10.1037/t06165-000 |PHQ-9       |patient health questionnaire-9                      |        9492|
|Health and Clinical Psychology           |10.1037/t00930-000 |GDS         |geriatric depression scale                          |        8709|
|Health and Clinical Psychology           |10.1037/t01528-000 |AUDIT       |alcohol use disorders identification test           |        8513|
|Health and Clinical Psychology           |10.1037/t00757-000 |TMT         |trail making test                                   |        8420|
|Health and Clinical Psychology           |10.1037/t02025-000 |BAI         |beck anxiety inventory                              |        7597|
|Health and Clinical Psychology           |10.1037/t01554-000 |BPRS        |brief psychiatric rating scale                      |        7504|
|Health and Clinical Psychology           |10.1037/t04111-000 |MADRS       |montgomery-asberg depression rating scale           |        7238|
|Health and Clinical Psychology           |10.1037/t01210-000 |SCL-90-R    |symptom checklist-90-revised                        |        7095|
|Cognitive Psychology                     |10.1037/t01069-000 |SWLS        |satisfaction with life scale                        |        7023|
|Health and Clinical Psychology           |10.1037/t02889-000 |PSS         |perceived stress scale                              |        6494|
|Health and Clinical Psychology           |10.1037/t05178-000 |PSQI        |pittsburgh sleep quality index                      |        6408|
|Health and Clinical Psychology           |10.1037/t00789-000 |BSI         |brief symptom inventory                             |        6365|
|Personality and Social Psychology        |10.1037/t03592-000 |PANAS       |positive and negative affect schedule               |        6313|
|Educational and Developmental Psychology |10.1037/t00540-000 |SDQ         |strengths and difficulties questionnaire            |        6257|
|Personality and Social Psychology        |10.1037/t06070-000 |PANAS-B     |positive and negative affect scale                  |        5147|
|Health and Clinical Psychology           |10.1037/t02121-000 |WMH-CIDI    |composite international diagnostic interview        |        4888|
|Health and Clinical Psychology           |10.1037/t02942-000 |CESD        |center for epidemiological studies depression scale |        4876|
|Health and Clinical Psychology           |10.1037/t02824-000 |HAM-A       |hamilton anxiety rating scale                       |        4484|
|Health and Clinical Psychology           |10.1037/t57982-000 |YBOCS       |yale-brown obsessive compulsive scale               |        4410|
|Health and Clinical Psychology           |10.1037/t18128-000 |ADI-R       |autism diagnostic interview-revised                 |        4096|
|Health and Clinical Psychology           |10.1037/t07081-000 |ESS         |epworth sleepiness scale                            |        4041|
|Health and Clinical Psychology           |10.1037/t54175-000 |ADOS        |autism diagnostic observation schedule              |        3790|
|Health and Clinical Psychology           |10.1037/t01756-000 |EPDS        |edinburgh postnatal depression scale                |        3750|
|Cognitive Psychology                     |10.1037/t05449-000 |stroop test |stroop test                                         |        3675|
|Health and Clinical Psychology           |10.1037/t06065-000 |SCWT        |stroop color and word test                          |        3637|
|Health and Clinical Psychology           |10.1037/t02080-000 |CTQ         |childhood trauma questionnaire                      |        3500|
|Health and Clinical Psychology           |10.1037/t00072-000 |CAPS        |clinician-administered ptsd scale                   |        3487|
|Health and Clinical Psychology           |10.1037/t02591-000 |GAD-7       |generalized anxiety disorder 7                      |        3157|
|Health and Clinical Psychology           |10.1037/t00025-000 |ASI         |addiction severity index                            |        3134|
|Health and Clinical Psychology           |10.1037/t07023-000 |SF-36       |36-item short form health survey                    |        3067|
|Health and Clinical Psychology           |10.1037/t10132-000 |COWAT       |controlled oral word association test               |        3053|
|Cognitive Psychology                     |10.1037/t48844-000 |CLVT        |california verbal learning test                     |        2847|
|Cognitive Psychology                     |10.1037/t03782-000 |IAT         |implicit association test                           |        2700|
|Health and Clinical Psychology           |10.1037/t02598-000 |PHQ         |patient health questionnaire                        |        2678|
|Health and Clinical Psychology           |10.1037/t02366-000 |ADL         |barthel index                                       |        2584|
|Health and Clinical Psychology           |10.1037/t48377-000 |SAPS        |scale for the assessment of positive symptoms       |        2352|
|Health and Clinical Psychology           |10.1037/t07021-000 |SF-12       |12-item short form health survey                    |        2304|
|Educational and Developmental Psychology |10.1037/t49755-000 |WAIS-III    |wechsler adult intelligence scale--third edition    |        2254|
|Personality and Social Psychology        |10.1037/t01093-000 |IRI         |interpersonal reactivity index                      |        2200|
|Health and Clinical Psychology           |10.1037/t01760-000 |PSWQ        |penn state worry questionnaire                      |        2149|
|Health and Clinical Psychology           |10.1037/t03974-000 |EDE-Q       |eating disorder examination questionnaire           |        2132|
|Health and Clinical Psychology           |10.1037/t27207-000 |WMS         |wechsler memory scale                               |        2130|
|Health and Clinical Psychology           |10.1037/t15120-000 |MMPI-2      |minnesota multiphasic personality inventory-2       |        2121|
|Personality and Social Psychology        |10.1037/t05257-000 |M-C SDS     |marlowe-crowne social desirability scale            |        2102|

## By subdiscipline (2015-2023)

```r
psyctests_info %>% group_by(subdiscipline_1, DOI, shortName, Name) %>%
  filter(Year > 2015) %>% 
  summarise(usage_count = sum(usage_count, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(usage_count)) %>% 
  head(50) %>% 
  kable()
```



|subdiscipline_1                          |DOI                |shortName          |Name                                                | usage_count|
|:----------------------------------------|:------------------|:------------------|:---------------------------------------------------|-----------:|
|Health and Clinical Psychology           |10.1037/t06165-000 |PHQ-9              |patient health questionnaire-9                      |        6955|
|Health and Clinical Psychology           |10.1037/t00741-000 |BDI                |beck depression inventory                           |        6131|
|Health and Clinical Psychology           |10.1037/t00742-000 |BDI-II             |beck depression inventory-ii                        |        6033|
|Health and Clinical Psychology           |10.1037/t04100-000 |HRSD               |hamilton rating scale for depression                |        5207|
|Health and Clinical Psychology           |10.1037/t03589-000 |HADS               |hospital anxiety and depression scale               |        4776|
|Health and Clinical Psychology           |10.1037/t05056-000 |PANSS              |positive and negative syndrome scale                |        4326|
|Health and Clinical Psychology           |10.1037/t01528-000 |AUDIT              |alcohol use disorders identification test           |        4025|
|Personality and Social Psychology        |10.1037/t01038-000 |RSES               |rosenberg self-esteem scale                         |        3729|
|Health and Clinical Psychology           |10.1037/t05178-000 |PSQI               |pittsburgh sleep quality index                      |        3529|
|Personality and Social Psychology        |10.1037/t03592-000 |PANAS              |positive and negative affect schedule               |        3440|
|Health and Clinical Psychology           |10.1037/t02889-000 |PSS                |perceived stress scale                              |        3393|
|Cognitive Psychology                     |10.1037/t01069-000 |SWLS               |satisfaction with life scale                        |        3383|
|Educational and Developmental Psychology |10.1037/t00540-000 |SDQ                |strengths and difficulties questionnaire            |        3099|
|Health and Clinical Psychology           |10.1037/t02591-000 |GAD-7              |generalized anxiety disorder 7                      |        2964|
|Health and Clinical Psychology           |10.1037/t00757-000 |TMT                |trail making test                                   |        2870|
|Health and Clinical Psychology           |10.1037/t00930-000 |GDS                |geriatric depression scale                          |        2775|
|Health and Clinical Psychology           |10.1037/t02025-000 |BAI                |beck anxiety inventory                              |        2758|
|Health and Clinical Psychology           |10.1037/t04111-000 |MADRS              |montgomery-asberg depression rating scale           |        2286|
|Health and Clinical Psychology           |10.1037/t02942-000 |CESD               |center for epidemiological studies depression scale |        2200|
|Health and Clinical Psychology           |10.1037/t02080-000 |CTQ                |childhood trauma questionnaire                      |        1849|
|Health and Clinical Psychology           |10.1037/t01756-000 |EPDS               |edinburgh postnatal depression scale                |        1753|
|Health and Clinical Psychology           |10.1037/t07023-000 |SF-36              |36-item short form health survey                    |        1729|
|Health and Clinical Psychology           |10.1037/t00789-000 |BSI                |brief symptom inventory                             |        1706|
|Health and Clinical Psychology           |10.1037/t02824-000 |HAM-A              |hamilton anxiety rating scale                       |        1669|
|Personality and Social Psychology        |10.1037/t06070-000 |PANAS-B            |positive and negative affect scale                  |        1600|
|Health and Clinical Psychology           |10.1037/t07081-000 |ESS                |epworth sleepiness scale                            |        1592|
|Health and Clinical Psychology           |10.1037/t54175-000 |ADOS               |autism diagnostic observation schedule              |        1541|
|Health and Clinical Psychology           |10.1037/t18128-000 |ADI-R              |autism diagnostic interview-revised                 |        1481|
|Health and Clinical Psychology           |10.1037/t01554-000 |BPRS               |brief psychiatric rating scale                      |        1460|
|Cognitive Psychology                     |10.1037/t05449-000 |stroop test        |stroop test                                         |        1404|
|Personality and Social Psychology        |10.1037/t01029-000 |DERS               |difficulties in emotion regulation scale            |        1394|
|Health and Clinical Psychology           |10.1037/t07021-000 |SF-12              |12-item short form health survey                    |        1326|
|Health and Clinical Psychology           |10.1037/t01210-000 |SCL-90-R           |symptom checklist-90-revised                        |        1278|
|Educational and Developmental Psychology |10.1037/t49755-000 |WAIS-III           |wechsler adult intelligence scale--third edition    |        1262|
|Health and Clinical Psychology           |10.1037/t03974-000 |EDE-Q              |eating disorder examination questionnaire           |        1252|
|Health and Clinical Psychology           |10.1037/t02121-000 |WMH-CIDI           |composite international diagnostic interview        |        1223|
|Health and Clinical Psychology           |10.1037/t57982-000 |YBOCS              |yale-brown obsessive compulsive scale               |        1178|
|Personality and Social Psychology        |10.1037/t20676-000 |interview schedule |interview schedule                                  |        1142|
|Health and Clinical Psychology           |10.1037/t00072-000 |CAPS               |clinician-administered ptsd scale                   |        1140|
|Personality and Social Psychology        |10.1037/t01093-000 |IRI                |interpersonal reactivity index                      |        1126|
|Health and Clinical Psychology           |10.1037/t31330-000 |IPAQ               |international physical activity questionnaire       |        1080|
|Cognitive Psychology                     |10.1037/t15082-000 |D–KEFS             |delis-kaplan executive function system              |        1071|
|Personality and Social Psychology        |10.1037/t06463-000 |ERQ                |emotion regulation questionnaire                    |        1047|
|Health and Clinical Psychology           |10.1037/t00297-000 |GHQ-12             |general health questionnaire-12                     |        1018|
|Personality and Social Psychology        |10.1037/t01408-000 |WHOQOL--BREF       |world health organization quality of life-bref      |        1004|
|Health and Clinical Psychology           |10.1037/t06065-000 |SCWT               |stroop color and word test                          |         932|
|Health and Clinical Psychology           |10.1037/t02598-000 |PHQ                |patient health questionnaire                        |         916|
|Health and Clinical Psychology           |10.1037/t01760-000 |PSWQ               |penn state worry questionnaire                      |         910|
|Health and Clinical Psychology           |10.1037/t10132-000 |COWAT              |controlled oral word association test               |         888|
|Cognitive Psychology                     |10.1037/t03782-000 |IAT                |implicit association test                           |         886|

## By type 

```r
psyctests_info %>% group_by(instrument_type_broad, DOI, shortName, Name) %>%
  summarise(usage_count = sum(usage_count, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(usage_count)) %>% 
  head(50) %>% 
  kable()
```



|instrument_type_broad |DOI                |shortName   |Name                                                | usage_count|
|:---------------------|:------------------|:-----------|:---------------------------------------------------|-----------:|
|questionnaire         |10.1037/t00741-000 |BDI         |beck depression inventory                           |       25118|
|questionnaire         |10.1037/t04100-000 |HRSD        |hamilton rating scale for depression                |       18484|
|questionnaire         |10.1037/t00742-000 |BDI-II      |beck depression inventory-ii                        |       14268|
|questionnaire         |10.1037/t05056-000 |PANSS       |positive and negative syndrome scale                |       13316|
|questionnaire         |10.1037/t03589-000 |HADS        |hospital anxiety and depression scale               |       12134|
|questionnaire         |10.1037/t01038-000 |RSES        |rosenberg self-esteem scale                         |       10896|
|questionnaire         |10.1037/t06165-000 |PHQ-9       |patient health questionnaire-9                      |        9492|
|questionnaire         |10.1037/t00930-000 |GDS         |geriatric depression scale                          |        8709|
|test                  |10.1037/t01528-000 |AUDIT       |alcohol use disorders identification test           |        8513|
|test                  |10.1037/t00757-000 |TMT         |trail making test                                   |        8420|
|questionnaire         |10.1037/t02025-000 |BAI         |beck anxiety inventory                              |        7597|
|questionnaire         |10.1037/t01554-000 |BPRS        |brief psychiatric rating scale                      |        7504|
|questionnaire         |10.1037/t04111-000 |MADRS       |montgomery-asberg depression rating scale           |        7238|
|other-rating          |10.1037/t01210-000 |SCL-90-R    |symptom checklist-90-revised                        |        7095|
|questionnaire         |10.1037/t01069-000 |SWLS        |satisfaction with life scale                        |        7023|
|questionnaire         |10.1037/t02889-000 |PSS         |perceived stress scale                              |        6494|
|questionnaire         |10.1037/t05178-000 |PSQI        |pittsburgh sleep quality index                      |        6408|
|questionnaire         |10.1037/t00789-000 |BSI         |brief symptom inventory                             |        6365|
|other-rating          |10.1037/t03592-000 |PANAS       |positive and negative affect schedule               |        6313|
|questionnaire         |10.1037/t00540-000 |SDQ         |strengths and difficulties questionnaire            |        6257|
|questionnaire         |10.1037/t06070-000 |PANAS-B     |positive and negative affect scale                  |        5147|
|other-rating          |10.1037/t02121-000 |WMH-CIDI    |composite international diagnostic interview        |        4888|
|questionnaire         |10.1037/t02942-000 |CESD        |center for epidemiological studies depression scale |        4876|
|questionnaire         |10.1037/t02824-000 |HAM-A       |hamilton anxiety rating scale                       |        4484|
|questionnaire         |10.1037/t57982-000 |YBOCS       |yale-brown obsessive compulsive scale               |        4410|
|other-rating          |10.1037/t18128-000 |ADI-R       |autism diagnostic interview-revised                 |        4096|
|questionnaire         |10.1037/t07081-000 |ESS         |epworth sleepiness scale                            |        4041|
|task                  |10.1037/t54175-000 |ADOS        |autism diagnostic observation schedule              |        3790|
|questionnaire         |10.1037/t01756-000 |EPDS        |edinburgh postnatal depression scale                |        3750|
|test                  |10.1037/t05449-000 |stroop test |stroop test                                         |        3675|
|test                  |10.1037/t06065-000 |SCWT        |stroop color and word test                          |        3637|
|questionnaire         |10.1037/t02080-000 |CTQ         |childhood trauma questionnaire                      |        3500|
|questionnaire         |10.1037/t00072-000 |CAPS        |clinician-administered ptsd scale                   |        3487|
|questionnaire         |10.1037/t02591-000 |GAD-7       |generalized anxiety disorder 7                      |        3157|
|questionnaire         |10.1037/t00025-000 |ASI         |addiction severity index                            |        3134|
|questionnaire         |10.1037/t07023-000 |SF-36       |36-item short form health survey                    |        3067|
|task                  |10.1037/t10132-000 |COWAT       |controlled oral word association test               |        3053|
|NA                    |10.1037/t48844-000 |CLVT        |california verbal learning test                     |        2847|
|test                  |10.1037/t03782-000 |IAT         |implicit association test                           |        2700|
|questionnaire         |10.1037/t02598-000 |PHQ         |patient health questionnaire                        |        2678|
|questionnaire         |10.1037/t02366-000 |ADL         |barthel index                                       |        2584|
|questionnaire         |10.1037/t48377-000 |SAPS        |scale for the assessment of positive symptoms       |        2352|
|questionnaire         |10.1037/t07021-000 |SF-12       |12-item short form health survey                    |        2304|
|test                  |10.1037/t49755-000 |WAIS-III    |wechsler adult intelligence scale--third edition    |        2254|
|questionnaire         |10.1037/t01093-000 |IRI         |interpersonal reactivity index                      |        2200|
|questionnaire         |10.1037/t01760-000 |PSWQ        |penn state worry questionnaire                      |        2149|
|questionnaire         |10.1037/t03974-000 |EDE-Q       |eating disorder examination questionnaire           |        2132|
|questionnaire         |10.1037/t27207-000 |WMS         |wechsler memory scale                               |        2130|
|questionnaire         |10.1037/t15120-000 |MMPI-2      |minnesota multiphasic personality inventory-2       |        2121|
|questionnaire         |10.1037/t05257-000 |M-C SDS     |marlowe-crowne social desirability scale            |        2102|


## By type (2015-2023)

```r
psyctests_info %>% group_by(instrument_type_broad, DOI, shortName, Name) %>%
  filter(Year > 2015) %>% 
  summarise(usage_count = sum(usage_count, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(usage_count)) %>% 
  head(50) %>% 
  kable()
```



|instrument_type_broad |DOI                |shortName          |Name                                                | usage_count|
|:---------------------|:------------------|:------------------|:---------------------------------------------------|-----------:|
|questionnaire         |10.1037/t06165-000 |PHQ-9              |patient health questionnaire-9                      |        6955|
|questionnaire         |10.1037/t00741-000 |BDI                |beck depression inventory                           |        6131|
|questionnaire         |10.1037/t00742-000 |BDI-II             |beck depression inventory-ii                        |        6033|
|questionnaire         |10.1037/t04100-000 |HRSD               |hamilton rating scale for depression                |        5207|
|questionnaire         |10.1037/t03589-000 |HADS               |hospital anxiety and depression scale               |        4776|
|questionnaire         |10.1037/t05056-000 |PANSS              |positive and negative syndrome scale                |        4326|
|test                  |10.1037/t01528-000 |AUDIT              |alcohol use disorders identification test           |        4025|
|questionnaire         |10.1037/t01038-000 |RSES               |rosenberg self-esteem scale                         |        3729|
|questionnaire         |10.1037/t05178-000 |PSQI               |pittsburgh sleep quality index                      |        3529|
|other-rating          |10.1037/t03592-000 |PANAS              |positive and negative affect schedule               |        3440|
|questionnaire         |10.1037/t02889-000 |PSS                |perceived stress scale                              |        3393|
|questionnaire         |10.1037/t01069-000 |SWLS               |satisfaction with life scale                        |        3383|
|questionnaire         |10.1037/t00540-000 |SDQ                |strengths and difficulties questionnaire            |        3099|
|questionnaire         |10.1037/t02591-000 |GAD-7              |generalized anxiety disorder 7                      |        2964|
|test                  |10.1037/t00757-000 |TMT                |trail making test                                   |        2870|
|questionnaire         |10.1037/t00930-000 |GDS                |geriatric depression scale                          |        2775|
|questionnaire         |10.1037/t02025-000 |BAI                |beck anxiety inventory                              |        2758|
|questionnaire         |10.1037/t04111-000 |MADRS              |montgomery-asberg depression rating scale           |        2286|
|questionnaire         |10.1037/t02942-000 |CESD               |center for epidemiological studies depression scale |        2200|
|questionnaire         |10.1037/t02080-000 |CTQ                |childhood trauma questionnaire                      |        1849|
|questionnaire         |10.1037/t01756-000 |EPDS               |edinburgh postnatal depression scale                |        1753|
|questionnaire         |10.1037/t07023-000 |SF-36              |36-item short form health survey                    |        1729|
|questionnaire         |10.1037/t00789-000 |BSI                |brief symptom inventory                             |        1706|
|questionnaire         |10.1037/t02824-000 |HAM-A              |hamilton anxiety rating scale                       |        1669|
|questionnaire         |10.1037/t06070-000 |PANAS-B            |positive and negative affect scale                  |        1600|
|questionnaire         |10.1037/t07081-000 |ESS                |epworth sleepiness scale                            |        1592|
|task                  |10.1037/t54175-000 |ADOS               |autism diagnostic observation schedule              |        1541|
|other-rating          |10.1037/t18128-000 |ADI-R              |autism diagnostic interview-revised                 |        1481|
|questionnaire         |10.1037/t01554-000 |BPRS               |brief psychiatric rating scale                      |        1460|
|test                  |10.1037/t05449-000 |stroop test        |stroop test                                         |        1404|
|questionnaire         |10.1037/t01029-000 |DERS               |difficulties in emotion regulation scale            |        1394|
|questionnaire         |10.1037/t07021-000 |SF-12              |12-item short form health survey                    |        1326|
|other-rating          |10.1037/t01210-000 |SCL-90-R           |symptom checklist-90-revised                        |        1278|
|test                  |10.1037/t49755-000 |WAIS-III           |wechsler adult intelligence scale--third edition    |        1262|
|questionnaire         |10.1037/t03974-000 |EDE-Q              |eating disorder examination questionnaire           |        1252|
|other-rating          |10.1037/t02121-000 |WMH-CIDI           |composite international diagnostic interview        |        1223|
|questionnaire         |10.1037/t57982-000 |YBOCS              |yale-brown obsessive compulsive scale               |        1178|
|other-rating          |10.1037/t20676-000 |interview schedule |interview schedule                                  |        1142|
|questionnaire         |10.1037/t00072-000 |CAPS               |clinician-administered ptsd scale                   |        1140|
|questionnaire         |10.1037/t01093-000 |IRI                |interpersonal reactivity index                      |        1126|
|questionnaire         |10.1037/t31330-000 |IPAQ               |international physical activity questionnaire       |        1080|
|NA                    |10.1037/t15082-000 |D–KEFS             |delis-kaplan executive function system              |        1071|
|questionnaire         |10.1037/t06463-000 |ERQ                |emotion regulation questionnaire                    |        1047|
|questionnaire         |10.1037/t00297-000 |GHQ-12             |general health questionnaire-12                     |        1018|
|questionnaire         |10.1037/t01408-000 |WHOQOL--BREF       |world health organization quality of life-bref      |        1004|
|test                  |10.1037/t06065-000 |SCWT               |stroop color and word test                          |         932|
|questionnaire         |10.1037/t02598-000 |PHQ                |patient health questionnaire                        |         916|
|questionnaire         |10.1037/t01760-000 |PSWQ               |penn state worry questionnaire                      |         910|
|task                  |10.1037/t10132-000 |COWAT              |controlled oral word association test               |         888|
|test                  |10.1037/t03782-000 |IAT                |implicit association test                           |         886|
