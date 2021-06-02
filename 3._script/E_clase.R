require(magick)

bird <- image_read('1._images/birdA.jpg')
bird <- image_scale(bird, 50)

reptil <- image_read('1._images/lizard.jpg')
reptil <- image_scale(reptil, 50)

mamifero <- image_read('1._images/coyote.jpg')
mamifero <- image_scale(mamifero, 50)
  
pez <- image_read('1._images/fish.jpg')
pez <- image_scale(pez, 50)

anfibio <- image_read('1._images/frog.jpg')
anfibio <- image_scale(anfibio, 50)

x <- data.frame(x=1:5, y=1:5)

