from os import listdir
from PIL import Image as PImage

class loader():
    def loadImages(path):
        # return array of images

        imagesList = listdir(path)
        loadedImages = []
        for image in imagesList:
            img = PImage.open(path + image)
            loadedImages.append(np.array(img))
        return loadedImages
    def loader(directory, label, app_list = []):
        new_list = app_list
        imgs = loadImages(directory)
        for i in imgs:
            new_list.append([i,label])
        return new_list