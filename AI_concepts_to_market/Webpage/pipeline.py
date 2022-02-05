import os
import random
from functions import upscaled, generator
def pipe():
    
    seed = generator()
    upscaled()
    img = os.listdir('static/assets/upscaled_images')[0]
    path = 'static/assets/upscaled_images/{}'.format(img)
    return seed,path
