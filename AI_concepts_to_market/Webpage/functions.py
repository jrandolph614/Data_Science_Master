from PIL import Image  
import cv2
import os
import random
import shutil
gen_path = 'static/assets/generated/images'
upscaler = cv2.dnn_superres.DnnSuperResImpl_create()
models = os.listdir('/Users/jrand/Documents/masters/AI Concepts/Semester Project/upscale_models')
upscaler.readModel('../upscale_models/{}'.format(models[2]))
upscaler.setModel('edsr',4)
def upscaled():
    name = os.listdir('static/assets/generated_images')[0]
    path = 'static/assets/generated_images/{}'.format(os.listdir('static/assets/generated_images/')[0])
    image = cv2.imread(path)
    upscaled_im = upscaler.upsample(image)
    cv2.imwrite('static/assets/upscaled_images/{}'.format(name),upscaled_im)
    shutil.move(path,'static/assets/user_generated/{}'.format(name))
def generator():
    try:
        old = os.listdir('static/assets/upscaled_images')[0]
        os.remove('static/assets/upscaled_images/{}'.format(old))
    except:
        pass
    network = 'static/assets/model/model.pkl'
    seed = random.randint(0,(2**32-1))
    outdir = 'static/assets/generated_images'
    os.system('python ../stylegan2-ada-pytorch/generate.py --network={network} --seeds={seed} --outdir={outdir}'.format(network=network,seed=seed,outdir=outdir))
    return seed
