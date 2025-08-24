#all sizes in mm
feather_width_mm <- 10
plywood_thickness_mm <- 3

# the below specs must be even multiplicities of feather length
case_width_in_feathers <- 26
case_depth_in_feathers <- 16
bottom_height_in_feathers <- 8
lid_height_in_feathers <- 2

if (feather_width_mm <= plywood_thickness_mm)
  stop("Feather length must be greater than plywood thickness")

if (case_depth_in_feathers %% 2 != 0 | case_width_in_feathers %% 2 !=0 |
    bottom_height_in_feathers %% 2 != 0 | lid_height_in_feathers %% 2 != 0)
  stop("The dimensions must be even multiplicities of the feather length")

if (case_depth_in_feathers <= 0 | case_width_in_feathers <=0 |
    bottom_height_in_feathers <= 0 | lid_height_in_feathers <= 0)
  stop("The dimensions must be positive multiplicities of the feather length")

draw_feathered_side_line <- function(segment_cnt, coordinate, current_pos, starting_full, increase_on_full_retrace, increase_on_feather_width) {
  new_pos <- current_pos
  full <- starting_full
  for (i in 1:segment_cnt) {
    new_pos[coordinate] <- current_pos[coordinate] + (-1)^(increase_on_feather_width + 1) * feather_width_mm
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
    
    new_pos[3-coordinate] <- current_pos[3-coordinate] + (-1)^(increase_on_full_retrace + 1) * (-1)^(full + 1) * plywood_thickness_mm
    lines(c(current_pos[1], new_pos[1]), c(current_pos[2], new_pos[2]))
    current_pos <- new_pos
    full <- !full
  }
  return(current_pos)
}


draw_side_wall <- function(length_segment_count, width_segment_count) {
  current_pos <- c(0,0)
  
  coordinate <- 1
  increase_on_full_retrace <- TRUE
  full <- TRUE
  increase_on_feather_width <- TRUE
  current_pos <- draw_feathered_side_line(length_segment_count, coordinate, current_pos, full, increase_on_full_retrace, increase_on_feather_width)
  
  coordinate <- 2
  full <- FALSE
  increase_on_full_retrace <- FALSE
  increase_on_feather_width <- TRUE
  current_pos <- draw_feathered_side_line(width_segment_count, coordinate, current_pos, full, increase_on_full_retrace, increase_on_feather_width)
  
  coordinate <- 1
  full <- TRUE
  increase_on_full_retrace <- FALSE
  increase_on_feather_width <- FALSE
  current_pos <- draw_feathered_side_line(length_segment_count, coordinate, current_pos, full, increase_on_full_retrace, increase_on_feather_width)

  coordinate <- 2
  full <- FALSE
  increase_on_full_retrace <- TRUE
  increase_on_feather_width <- FALSE
  current_pos <- draw_feathered_side_line(width_segment_count, coordinate, current_pos, full, increase_on_full_retrace, increase_on_feather_width)
  
} 

open.pdf <- function(title, width_in_mm, height_in_mm, margin_in_mm) {
  pdf(file=title, width=(width_in_mm + 2 * margin_in_mm) / 25.4, height=(height_in_mm + 2 * margin_in_mm) / 25.4 )   #units: inches
  plot.new()
  par(mai=c(margin_in_mm / 25.4, margin_in_mm / 25.4, margin_in_mm / 25.4, margin_in_mm / 25.4))  #mai - margins in inces
  plot.window(c(0, width_in_mm), c(0, height_in_mm), asp=1, xaxs="i", yaxs="i")  #="i" to avoid scale by 4%
}

close.pdf <- function() {
  dev.off()
}


open.pdf("design_PDFs/bottom_side.pdf", bottom_height_in_feathers*feather_width_mm, case_depth_in_feathers*feather_width_mm, 10)
draw_side_wall(bottom_height_in_feathers, case_depth_in_feathers)
close.pdf()

