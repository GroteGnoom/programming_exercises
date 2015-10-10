import matplotlib.pyplot as plt
import numpy
from sklearn import datasets
from sklearn import svm

digits = datasets.load_digits()

print( digits.data)

print (digits.target)

print (digits.images[0])

clf = svm.SVC(gamma=0.0001, C=100)

x,y = digits.data[:-10], digits.target[:-10]

clf.fit(x,y)

theone=-4

print('prediction:', clf.predict(digits.data[theone]))

plt.imshow(digits.images[theone], cmap=plt.cm.gray_r, interpolation="nearest")

plt.show()
