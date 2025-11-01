formdata <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/19J9P-is9eJWoDN7_agmSeu5S5YgD4JZIoe_FtZ-G3EQ/edit?usp=sharing")
source("R/miembros_functions.R")

for (i in 1:nrow(formdata)) {
  name <- formdata$`Nombre y apellidos`[i] |> 
    gsub(pattern = " ", replacement = "_") |> 
    iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT')
  # if (file.exists(paste0("miembros/", name, ".qmd"))) {next} #always re-render for updates
  create_profile_qmd(formdata, i)
}
