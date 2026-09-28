
library(ggplot2)

# --- Run a script only once per session ---
# 02 and 03 both source 01, and 03 sources 02, so an unguarded chain
# re-runs the NOMIS pull and the EES downloads several times.
.sourced_files <- character(0)


# --- Analysis years ---
# Single source of truth for the growth baseline and latest year.
# 2018/19 is the earliest year in the DfE plans data; note that the
# tribunal datasets only start at 2019/20, so 2018/19 rows carry NA
# for every tribunal column.
BASE_YEAR   <- "2018/19"
LATEST_YEAR <- "2025/26"

mhclg_teal   <- "#00625E"
mhclg_plum   <- "#932A71"
mhclg_orange <- "#BF491D"   
mhclg_green  <- "#40611F"   
mhclg_navy   <- "#205083"   
mhclg_red    <- "#85282A"   
mhclg_grey   <- "#E5E5E5"   

mhclg_palette <- c(mhclg_teal, mhclg_plum, mhclg_orange,
                   mhclg_navy, mhclg_green, mhclg_red)

# --- Template font is Arial ---
# Arial is available by default on Windows (MHCLG standard machines).
# On Mac/Linux it may fall back silently to sans - fine for drafts;
# for publication-perfect output on those platforms use the showtext
# package to register Arial explicitly.
mhclg_font <- "Arial"

# --- Base theme ---
theme_mhclg <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = mhclg_font) +
    theme(
      plot.title    = element_text(face = "bold", colour = mhclg_teal),
      plot.subtitle = element_text(colour = "grey30"),
      axis.title    = element_text(colour = "grey20"),
      panel.grid.minor = element_blank(),
      legend.title  = element_text(face = "bold")
    )
}

# --- Discrete fill/colour scales (years, categories) ---
scale_fill_mhclg_d   <- function(...) scale_fill_manual(values = mhclg_palette, ...)
scale_colour_mhclg_d <- function(...) scale_colour_manual(values = mhclg_palette, ...)

# Single-hue light-grey -> brand teal: sequential data reads correctly
# and matches the template.
scale_fill_mhclg_map <- function(name = NULL, limits = NULL) {
  scale_fill_gradient(low = mhclg_grey, high = mhclg_teal,
                      na.value = "grey92", name = name, limits = limits)
}

