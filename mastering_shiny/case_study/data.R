dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
injuries
#> # A tibble: 255,064 × 10
#>   trmt_date    age sex   race  body_part   diag      location    prod_code weight
#>   <date>     <dbl> <chr> <chr> <chr>       <chr>     <chr>           <dbl>  <dbl>
#> 1 2017-01-01    71 male  white Upper Trunk Contusio… Other Publ…      1807   77.7
#> 2 2017-01-01    16 male  white Lower Arm   Burns, T… Home              676   77.7
#> 3 2017-01-01    58 male  white Upper Trunk Contusio… Home              649   77.7
#> 4 2017-01-01    21 male  white Lower Trunk Strain, … Home             4076   77.7
#> 5 2017-01-01    54 male  white Head        Inter Or… Other Publ…      1807   77.7
#> 6 2017-01-01    21 male  white Hand        Fracture  Home             1884   77.7
#> # … with 255,058 more rows, and 1 more variable: narrative <chr>

products <- vroom::vroom("neiss/products.tsv")
products
#> # A tibble: 38 × 2
#>   prod_code title
#>       <dbl> <chr>
#> 1       464 knives, not elsewhere classified
#> 2       474 tableware and accessories
#> 3       604 desks, chests, bureaus or buffets
#> 4       611 bathtubs or showers
#> 5       649 toilets
#> 6       676 rugs or carpets, not specified
#> # … with 32 more rows

population <- vroom::vroom("neiss/population.tsv")
population
#> # A tibble: 170 × 3
#>     age sex    population
#>   <dbl> <chr>       <dbl>
#> 1     0 female    1924145
#> 2     0 male      2015150
#> 3     1 female    1943534
#> 4     1 male      2031718
#> 5     2 female    1965150
#> 6     2 male      2056625
#> # … with 164 more rows

selected <- injuries %>% filter(prod_code == 649) # toilets
nrow(selected)

selected %>% count(location, wt = weight, sort = TRUE)
#> # A tibble: 6 × 2
#>   location                         n
#>   <chr>                        <dbl>
#> 1 Home                       99603.
#> 2 Other Public Property      18663.
#> 3 Unknown                    16267.
#> 4 School                       659.
#> 5 Street Or Highway             16.2
#> 6 Sports Or Recreation Place    14.8

selected %>% count(body_part, wt = weight, sort = TRUE)
#> # A tibble: 24 × 2
#>   body_part        n
#>   <chr>        <dbl>
#> 1 Head        31370.
#> 2 Lower Trunk 26855.
#> 3 Face        13016.
#> 4 Upper Trunk 12508.
#> 5 Knee         6968.
#> 6 N.S./Unk     6741.
#> # … with 18 more rows

selected %>% count(diag, wt = weight, sort = TRUE)
#> # A tibble: 20 × 2
#>   diag                       n
#>   <chr>                  <dbl>
#> 1 Other Or Not Stated   32897.
#> 2 Contusion Or Abrasion 22493.
#> 3 Inter Organ Injury    21525.
#> 4 Fracture              21497.
#> 5 Laceration            18734.
#> 6 Strain, Sprain         7609.
#> # … with 14 more rows

summary <- selected %>%
  count(age, sex, wt = weight)
summary
#> # A tibble: 208 × 3
#>     age sex         n
#>   <dbl> <chr>   <dbl>
#> 1     0 female   4.76
#> 2     0 male    14.3
#> 3     1 female 253.
#> 4     1 male   231.
#> 5     2 female 438.
#> 6     2 male   632.
#> # … with 202 more rows

summary %>%
  ggplot(aes(age, n, colour = sex)) +
  geom_line() +
  labs(y = "Estimated number of injuries")


summary <- selected %>%
  count(age, sex, wt = weight) %>%
  left_join(population, by = c("age", "sex")) %>%
  mutate(rate = n / population * 1e4)

summary
#> # A tibble: 208 × 5
#>     age sex         n population   rate
#>   <dbl> <chr>   <dbl>      <dbl>  <dbl>
#> 1     0 female   4.76    1924145 0.0247
#> 2     0 male    14.3     2015150 0.0708
#> 3     1 female 253.      1943534 1.30
#> 4     1 male   231.      2031718 1.14
#> 5     2 female 438.      1965150 2.23
#> 6     2 male   632.      2056625 3.07
#> # … with 202 more rows

summary %>%
  ggplot(aes(age, rate, colour = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people")

#Note that the rates only go up to age 80 because I couldn’t find population data for ages over 80.


selected %>%
  sample_n(10) %>%
  pull(narrative)
#>  [1] "CHI. 75 YOF WAS SITTING ON A TOILET WHEN SHE LOST BALANCE AND FELL OFFAND ONTO THE FLOOR."
#>  [2] "67 YOM - FX LT TIBIA - PT WAS GETTING UP FROM TOILET AND TWISTED LTANKLE AND LEG."
#>  [3] "71YOF WAS GETTING OFF THE TOILET AND LOST BALANCE AND FELL ONTO BUTTOCKS FRACTURED PELVIS"
#>  [4] "81YOF LAC LWR LEG, FELL OFF TOILET"
#>  [5] "29YOF FELL OFF THE TOILET STRUCK HAD ON THE SIDE OF THE BATHTUB AND SUSTAINED A  LACERATION TO SCALP"
#>  [6] "49 YO M LAC FINGER-STRUCK AGAINST TOILET"
#>  [7] "83 YOF C/O BILATERAL SHOULDER PAIN AFTER FALL LAST NIGHT, PT STATES WHILE SITTING ON TOILET HER R FOOT \"GAVE OUT\" CAUSING FALL. DX FALL"
#>  [8] "A 49YOF TO ER WITH HEROIN ABUSE, PASSED OUT AND FELL OFF TOILET, \"FLOPPED AROUND ON GROUND,\", HEROIN ABUSE WITH POSS COCAINE & METH ING"
#>  [9] "PT WAS WALKING WITH WALKER AT NURSING HOME LOST FOOTING FELL AND HIT CHEST ON TOILET   R RIB PAIN   83YOM"
#> [10] "24YOM SYNCOPE STANDING FROM TOILET"



