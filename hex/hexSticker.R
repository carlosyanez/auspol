# make sure that {librarian} and is there
if(!require(librarian)) install.packages("librarian")

librarian::shelf("tidyverse",
                 "here",
                 "ggtext",
                 "ggfx",
                 "hexSticker",
                 "png",
                 "magick",                   #for twitter card
                 "dmi3kno/bunny"             #helper for magick

)

#<div> Icons made by <a href="https://www.flaticon.com/authors/pixel-perfect" title="Pixel perfect"> Pixel perfect </a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com'</a></div>

#https://www.reshot.com/free-svg-icons/item/download-6CWHK4RJU2/
ds_logo <- image_read(here("hex","manual-voting_white.png"))


height <- 1

s  <- sticker(ds_logo, package="auspol", p_size=18, p_x=1, p_y=0.50,
              s_x=1.1,s_y=1.2,
              s_width=1.3*height, s_height=height,
              p_color="white",
                  h_color="#FFCD00", h_fill="#00843D",
                  filename=here("img","hexSticker.png"))




#https://www.ddrive.no/post/making-hex-and-twittercard-with-bunny-and-magick/


img_hex_gh <- image_read(here("img","hexSticker.png")) %>%
  image_scale("400x400")

# https://www.pngfind.com/download/hobbwm_github-clipart-github-logo-cartoon-hd-png-download/
gh_logo <- image_read(here("hex","gh.png")) %>%
  image_scale("50x50")

bg_colour <-"white"

gh <- image_canvas_ghcard(bg_colour) %>%
  image_compose(img_hex_gh, gravity = "East", offset = "+100+0") %>%
  image_annotate("auspol", gravity = "West", location = "+100-30",
                 color="black", size=60, font="Aller", weight = 700) %>%
  image_compose(gh_logo, gravity="West", offset = "+100+40") %>%
  image_annotate("carlosyanez/auspol", gravity="West", location="+160+45",
                 size=50, font="Ubuntu Mono",color="black") %>%
  image_border_ghcard(bg_colour)

gh %>%
  image_write(here::here("img", "bbox_ghcard.png"))


