library(rggobi)

ggobi.geozoo <- function(data, ...) {
	gg <- ggobi(data$points)
	glyph_colour(gg[1]) <- 6
	glyph_size(gg[1]) <- 3

	if(!is.null(data$edges)) {
		edges(gg) <- data$edges
		glyph_colour(gg[2]) <- 6
	}
	close(displays(gg)[[1]])
	g.display <- display(gg[1], "2D Tour")
	edges(g.display) <- gg[2]
	
	invisible(gg)
}





















