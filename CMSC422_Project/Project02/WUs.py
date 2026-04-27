from numpy import *
import time
import multiclass
from datasets import *
from sklearn.tree import DecisionTreeClassifier, export_text

t = multiclass.makeBalancedTree(range(20))
h = multiclass.MCTree(t, lambda: DecisionTreeClassifier(max_depth=3))
h.train(WineData.X, WineData.Y)
P = h.predictAll(WineData.Xte)
print(mean(P == WineData.Yte))