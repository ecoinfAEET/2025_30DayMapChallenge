

create_profile_qmd <- function(form.table, row){
  
  
  pers.table <- as.data.frame(form.table[row,])
  
  name   <- pers.table$`Nombre y apellidos`
  email  <- pers.table$`Email`
  inst   <- pers.table$`Institución`
  ciudad <- pers.table$`Ciudad`
  pais   <- pers.table$`País`
  inves  <- pers.table$`Resumen investigación (max. 3 frases)`
  inter  <- pers.table$`Intereses dentro del grupo (max. 4 intereses)`
  web    <- pers.table$`Pagina web`
  github <- pers.table$`Cuenta de GitHub`
  social <- pers.table$`Cuenta de Bluesky o Twitter`
  
  if(!is.na(github)){
    github <- ifelse(grepl("https://", github), github, paste0("https://github.com/", github))
    }
  
  gh.icon <- fontawesome::fa(name = "github",  fill = "#71706F", height = "1em")
  at.icon <- fontawesome::fa(name = "at", fill = "#71706F", height = "1em")
  web.icon <- fontawesome::fa(name = "user", fill = "#71706F", height = "1em")
  mail.icon <- fontawesome::fa(name = "envelope", fill = "#71706F", height = "1em")
  
  icons <- " "    
  
  if (!is.na(social)) {icons <- paste0(icons, at.icon,"\t<", social, ">\t\t\n")}
  if (!is.na(github)) {icons <- paste0(icons, gh.icon,"\t<", github , ">\t\t\n")} 
  if (!is.na(web)) {icons <- paste0(icons, web.icon,"\t<", web , ">\t\t\n")} 
  if (!is.na(email)) {icons <- paste0(icons, mail.icon,"\t", email , "\t\t\n")} 
  
  if (is.na(github)){
    image <- "../images/avatar.jpg"
  }else{
    ghuser <- gsub("https://", "", github)
    ghuser <- gsub("github.com/", "", ghuser)
    image <- paste0("https://github.com/", ghuser, ".png")
  }
  
  # ord    <- which(sort(formdata$`Nombre y apellidos`) == name)[1]
  # qmdname <- paste0(gsub(" ", "_", name), ".qmd")
  
  # if(!(file.exists(paste0("miembros/",qmdname)))){
  # mejor re-renderizar para asegurar el orden ?
  
  txt <- paste0(
    "---", "\n",
    "title: ", name,  "\n",
    # "sortby: ", ord,  "\n",
    "subtitle: ", paste0(inst, ", ", ciudad, " (", pais, ")"), "\n",
    "image: ", image, "\n",
    "toc: false",  "\n",
    "about: ", "\n",
    "  id: person-profile",  "\n",
    "  template: jolla",  "\n",
    "",  "\n",
    "---",  "\n",
    "",  "\n",
    ":::{#person-profile}",  "\n",
    "\n",
    "# Resumen de investigación:\n",
    inves, "\n\n",
    "#  Interés profesional dentro del grupo:\n",    
    inter, "\n",
    "\n","\n",
    icons,"\n",
    ":::"
  )
  
  writeLines(txt, paste0("miembros/", 
                         iconv(gsub(" ", "_", name), from = 'UTF-8', to = 'ASCII//TRANSLIT'), 
                         ".qmd"))
  
  # }
}    


# for(i in 1:nrow(formdata)){
# create_profile_qmd(formdata, i)
# }
