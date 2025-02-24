get_name_base <- function(Name_base) {
  # stringi::stri_trim_right("yadda, :-", "[\\p{Wspace}:,-;]", negate = TRUE)
  right_trim_punct_space <- function(x) { stringi::stri_trim_right(x, "[\\p{Wspace}:,-;]", negate = TRUE) }

  for_x <- c(" for adolescent athletes", " for adults and adolescents",
             " for adults who stutter", " for brazilian adolescents", " for british adolescents",
             " for caregivers of adults with mental health issues", " for chinese",
             " for chinese adolescents", " for chinese seniors", " for community care",
             " for elderly people", " for emerging adults", " for experience sampling method",
             " for fatigue", " for future time orientation", " for hospital diskharge scale for older people",
             " for humanity", " for incarcerated women", " for informal caregivers",
             " for learning questionnaire for chinese adult learners", " for living inventory for adolescents",
             " for lower limb differences", " for manual wheelchair users",
             " for men", " for mental health problems", " for obsessions and compulsions",
             " for older adults", " for parkinson disease", " for patients with systemic lupus erythematosus",
             " for pediatric obsessive", " for pediatrics", " for people with learning disabilities",
             " for pre", " for prescription drugs", " for schizophrenia",
             " for sexual disorders", " for spanish adolescents", " for spanish gay, lesbian and bisexual people",
             " for spanish kids", " for sport", " for sport and exercise",
             " for students", " for substance abuse", " for swedish", " for teachers",
             " for the german armed forces", " for third", " for young adults",
             " for young children", " for writing", " for couples", " for obsessive",
             " for service providers", " for adults", " for youth", " for children and adolescents",
             " for adolescents", " for children")
  for_x <- setNames(rep("", length(for_x)), paste0(for_x, "$"))
  Name_base <- right_trim_punct_space(str_replace_all(Name_base, for_x))
  n_distinct(Name_base)


  roman_numerals  <- c('i','ii','iii','iv','v','vi','vii','viii','ix','x')
  roman_numerals <- setNames(rep("", length(roman_numerals)), paste0("(version ",
                                                                     roman_numerals,"$|part ",
                                                                     roman_numerals,"$|form ",
                                                                     roman_numerals,"$|( |—|–|-|:|,)",
                                                                     roman_numerals,"$)|",
                                                                     roman_numerals,
                                                                     "(—|–|-| )edition$"))
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, roman_numerals)))
  n_distinct(Name_base)


  english_number_abbreviations <- c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
  english_number_abbreviations <- setNames(rep("", length(english_number_abbreviations)), paste0(english_number_abbreviations," (edition(|™)|ed.)$"))
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, english_number_abbreviations)))
  n_distinct(Name_base)

  english_ordinal <- c("first","second","third","fourth","fifth"
                       ,"sixth","seventh","eighth","ninth","tenth")
  english_ordinal <- setNames(rep("", length(english_ordinal)), paste0(english_ordinal," version$"))
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, english_ordinal)))
  n_distinct(Name_base)


  english_numbers <- c("1",'2','3','4','5','6','7','8','9','10')
  english_numbers <- setNames(rep("", length(english_numbers)), paste0("(version ",english_numbers,"(|\\.[0-9])$)|(( |—|–|-|:|;)",english_numbers,"$)|(( |,|—|–|-)",english_numbers,"\\.[0-9]$)"))
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, english_numbers)))
  n_distinct(Name_base)


  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, "revised(| version| edition| form)$|(—|–|-| )r$|revised( edition of the| |—|–|-)", "")))
  n_distinct(Name_base)

  table_language<- readRDS("../sober_rubric/other_data/table_language")
  language_names <- table_language$name
  language_names <- paste0(language_names,"( version| adaptation| edition| revision| form)$|^",language_names,"( |—|–|-)|((—|–|-|)",language_names,")$")
  language_names <- setNames(rep("", length(language_names)),language_names)
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, language_names)))
  n_distinct(Name_base)

  language_names <- table_language$abbrevation
  language_names <- paste0("(—|-)",language_names,"$")
  language_names <- setNames(rep("", length(language_names)),language_names)
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, language_names)))
  n_distinct(Name_base)

  subforms <- c("subtest","subscale","abbreviated","substitution task","substitution","super-short form","short form","short-form","long form", "adult form", "international form","brief form","teacher form","expanded form","form 5x","form c","form s","form y","adolescent form","parent form","parent report form","observer form","form a","short form (adapted)","modified version","modified","partner form","shortened form","form ab","adult version","-adult","restructured form","patient version","community form","form-e","client form","reduced form","-bref")
  subforms_r <- setNames(rep("", length(subforms)), paste0("^",subforms))
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, subforms_r)))
  n_distinct(Name_base)

  subforms_r <- setNames(rep("", length(subforms)), paste0(subforms,"$"))
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, subforms_r)))
  n_distinct(Name_base)


  ## item numbers
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, "^\\d++-item", "")))
  n_distinct(Name_base)

  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, "\\d++(| |-)(item(|s))(| questionnaire| scale| version|)$", "")))
  n_distinct(Name_base)

  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, "^sf( |-)\\d++ ", "")))
  n_distinct(Name_base)

  ## xx version
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, "--[:.,/0-9&a-z- ;'()]+ versions?$", "")))
  n_distinct(Name_base)

  ## xx version; x translation
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, "--[:.,/0-9&a-z- ;'()]+ version; [a-z]+ translation$", "")))
  n_distinct(Name_base)

  ## xx version; x translation
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base, "--[:.,/0-9&a-z- ;'()]+ versions?;.+$", "")))
  n_distinct(Name_base)

  ## xx (version x), nuclear option
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"[-;:].+\\bversion\\b.+$", "")))
  n_distinct(Name_base)

  ## xx (version x), nuclear option
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"\\bversion$", "")))
  n_distinct(Name_base)

  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"--? ?adapted$", "")))
  n_distinct(Name_base)

  ## xx (version of), nuclear option
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"^version of", "")))
  n_distinct(Name_base)

  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"^short[- ]version ", "")))
  n_distinct(Name_base)

  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"-short$", "")))
  n_distinct(Name_base)

  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base," short$", "")))
  n_distinct(Name_base)

  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"--international$", "")))
  n_distinct(Name_base)


  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"-mandarin$", "")))
  n_distinct(Name_base)

  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"-adolescent$", "")))
  n_distinct(Name_base)

  ## short form, nuclear option
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"\\bshort form$", "")))
  n_distinct(Name_base)

  ## --, nuclear option
  Name_base <- right_trim_punct_space(str_trim(str_replace_all(Name_base,"--.+$", "")))
  n_distinct(Name_base)

  Name_base[str_detect(Name_base, "\\bversion\\b")]
  Name_base[str_detect(Name_base, "--.+ version")]
  Name_base[str_detect(Name_base, "short version")]

  Name_base
}


calc_norm_entropy <- function(n) {
  n <- n[n>0]
  entropy::entropy(n)/log(length(n))
}

treemap_graph <- function(tests, colors = c("#69D2E7", "#A7DBD8", "#E0E4CC", "#F38630", "#FA6900")) {
  bgcolor <-  colors[1]
  colors <- colors[-1]
  fig <- plotly::plot_ly(
    type='treemap',
    # labels = tests$shortName,
    labels = str_c('<a href="https://dx.doi.org/',tests$DOI,'">',tests$shortName,'</a>'),
    parents = tests$parent,
    values= tests$n,
    text = str_c(if_else(tests$Name == tests$shortName, "", str_c(tests$Name, "<br>")), tests$DOI),
    marker = list(line = list(width = 0.1)
                  , colors = rep(colors, length.out = nrow(tests))
    ),
    tiling = list(pad = 0.1,
                  packing = "squarify",
                  squarifyratio = (1 + sqrt(5)) / 2),
    # insidetextfont = list(size = I(50), color = "green", mode = "hide"),
    hoverinfo="label+value+text+percent root",
    textinfo="label") %>%
    plotly::config(displaylogo = FALSE, displayModeBar = F)

  fig %>% plotly::layout(
    autosize = TRUE,
    paper_bgcolor = "white",
    plot_bgcolor = bgcolor,
    uniformtext=list(minsize=15, mode='hide'),
    margin = list(l = 0, t = 0, r = 0 , b = 0)
  )
}
