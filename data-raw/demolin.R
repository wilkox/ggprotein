# A demonstration protein, demolin, and its secondary structure. Demolin is 100
# AAs long and has three domains

library(tibble)

demolin_domains <- tribble(
  ~start, ~end, ~name,
       0,   20,   "I",
      25,   60,  "II",
      60,  100,  "III"
)
usethis::use_data(demolin_domains, overwrite = TRUE)

demolin_ss <- tribble(
  ~start, ~end,    ~type,
       5,   11, "strand",
      12,   15,  "helix",
      21,   24,   "turn",
      29,   40, "strand",
      41,   45, "strand",
      45,   59,  "helix",
      63,   71,   "turn",
      75,   91,  "helix"
)
usethis::use_data(demolin_ss, overwrite = TRUE)
