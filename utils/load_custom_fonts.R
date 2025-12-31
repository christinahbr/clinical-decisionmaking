library(showtext)

# Check which fonts are installed
# systemfonts::system_fonts() |> View()

# Set your font folder path once
font_dir <- "C:/Users/chuber/AppData/Local/Microsoft/Windows/Fonts"

# Create a named list of font families with regular and bold file names
fonts_to_add <- list(
  Figtree = list(
    regular = "Figtree-Regular.ttf",
    bold    = "Figtree-SemiBold.ttf"
  ),
  Karla = list(
    regular = "Karla-VariableFont_wght.ttf",
    bold    = "Karla-Bold.ttf"
  ),
  DMSans = list(
    regular = "DMSans_18pt-Regular.ttf",
    bold    = "DMSans_24pt-Bold.ttf"
  ),
  Oxygen = list(
    regular = "Oxygen-Regular.ttf",
    bold    = "Oxygen-Bold.ttf"
  ),
  Jost = list(
    regular = "Jost-VariableFont_wght.ttf",
    bold    = "Jost-Bold.ttf"
  ),
  Mulish = list(
    regular = "Mulish-Regular.ttf",
    bold    = "Mulish-Bold.ttf"
  ),
  Nunito = list(
    regular = "Nunito-Regular.ttf",
    bold    = "Nunito-Bold.ttf"
  ),
  RedHatDisplay = list(
    regular = "RedHatDisplay-VariableFont_wght.ttf",
    bold    = "RedHatDisplay-Bold.ttf"
  )
)

# Register each font family
purrr::iwalk(fonts_to_add, function(paths, name) {
  font_add(
    family = name,
    regular = file.path(font_dir, paths$regular),
    bold    = file.path(font_dir, paths$bold)
  )
})

# Turn on showtext
showtext_auto()