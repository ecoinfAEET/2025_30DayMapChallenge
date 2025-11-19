plot_d3_map <- function(sf_data,
                        projection = "Winkel3",
                        bg_color = "#cce5ff",
                        grid = TRUE,
                        country_fill = "#7dc",
                        country_stroke = "#333",
                        country_stroke_width = 0.3,
                        main_title = NULL,
                        title_margin = 10,
                        title_font_size = 16,
                        save_file = NULL) {
  
  acc_projections <- c("Airy", "Aitoff", "August", "Baker", "Boggs", "Bonne", 
  "Bottomley", "Bromley", "ChamberlinAfrica", "Collignon", "Craig", "Craster", 
  "CylindricalEqualArea", "CylindricalStereographic", "Eckert1", "Eckert2", 
  "Eckert3", "Eckert4", "Eckert5", "Eckert6", "Eisenlohr", "Fahey", "Foucaut",
  "FoucautSinusoidal", "Gilbert", "Ginzburg4", "Ginzburg5", "Ginzburg6", "Ginzburg8", 
  "Ginzburg9", "Gringorten", "Guyou", "Hammer", "Hill", "Homolosine", "Hufnagel", 
  "Hyperelliptical", "Kavrayskiy7", "Lagrange", "Larrivee", "Laskowski", "Loximuthal", 
  "Miller", "ModifiedStereographicAlaska", "ModifiedStereographicGs50", "Mollweide", 
  "MtFlatPolarParabolic", "MtFlatPolarQuartic", "MtFlatPolarSinusoidal", "NaturalEarth1", 
  "NaturalEarth2", "NellHammer", "Nicolosi", "Patterson", "PeirceQuincuncial", "Polyconic", 
  "PolyhedralButterfly", "PolyhedralCollignon", "PolyhedralWaterman", "RectangularPolyconic", 
  "Robinson", "Satellite", "Sinusoidal", "Stereographic", "Times", "VanDerGrinten", 
  "VanDerGrinten2", "VanDerGrinten3", "VanDerGrinten4", "Wagner", "Wagner4", 
  "Wagner6", "Wagner7", "Winkel3")
  
  if(!projection %in% acc_projections){
    stop("projection should be a value from :", paste0("\n - ", acc_projections))
  }
  
  geojson_list <- geojsonio::geojson_list(sf_data)
  
  js_code <- sprintf('
    var script = document.createElement("script");
    script.src = "https://cdn.jsdelivr.net/npm/d3-geo-projection@3/dist/d3-geo-projection.min.js";
    script.onload = function() {

      const projection = d3.geo%s().fitSize([width, height], data);
      const path = d3.geoPath().projection(projection);
      const graticule = d3.geoGraticule();

      // Background fill
      svg.append("path")
         .datum(graticule.outline())
         .attr("d", path)
         .attr("fill", "%s");

      // Optional internal grid lines
      if (%s) {
        svg.append("path")
           .datum(graticule())
           .attr("d", path)
           .attr("fill", "none")
           .attr("stroke", "#999")
           .attr("stroke-width", 0.3)
           .attr("stroke-opacity", 0.5);
      }

      // Draw countries
      svg.selectAll("path.country")
         .data(data.features)
         .enter().append("path")
         .attr("class", "country")
         .attr("d", path)
         .attr("fill", "%s")
         .attr("stroke", "%s")
         .attr("stroke-width", %f);

      // Add main title in top-right corner if provided
      %s
    };
    document.head.appendChild(script);
  ',
                     projection,
                     bg_color,
                     tolower(grid),
                     country_fill,
                     country_stroke,
                     country_stroke_width,
                     if (!is.null(main_title)) {
                       sprintf('svg.append("text")\n  .attr("x", width - %f)\n  .attr("y", %f)\n  .attr("text-anchor", "end")\n  .attr("font-size", %f)\n  .attr("font-weight", "bold")\n  .text("%s");',
                               title_margin, title_margin + title_font_size, title_font_size, main_title)
                     } else {
                       ""
                     }
  )
  
  map <-r2d3(data = geojson_list,
             script = js_code,
             d3_version = 6)
  
  if(!is.null(save_file)){
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(map, html_file, selfcontained = TRUE)
    
    webshot2::webshot(html_file, save_file, vwidth = 800, vheight = 600)
  }
  
  return(map)
}




