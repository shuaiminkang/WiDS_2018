import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn import preprocessing
import lightgbm as lgb

from sklearn.model_selection import GridSearchCV, KFold
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.pipeline import Pipeline
from sklearn import ensemble
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import PolynomialFeatures
from sklearn.metrics import roc_auc_score

train1 = pd.read_csv("train0224_1.csv")
test1 = pd.read_csv("test0224_1.csv")


Y = train1["is_female"]
X = train1.drop("is_female",1)
#X.head(2)

X1 = X.apply(lambda x: x.astype("category"))
X1_dummy = pd.get_dummies(X1)
print(X1_dummy.shape)

test1_X = test1.apply(lambda x: x.astype("category"))
test1_X_dummy = pd.get_dummies(test1_X)
X_train, X_test, y_train, y_test = train_test_split(X1_dummy, Y, test_size=0.5, random_state=42)

clf_pip = Pipeline([("scale",StandardScaler()),("clf",lgb.LGBMClassifier())]) #("PCA",PCA())
param_grid={'clf__bagging_fraction':[1],
            'clf__bagging_freq':[20],
            'clf__feature_fraction':[0.8],
            'clf__learning_rate':[0.1],
            'clf__max_bin':[20], #0.96: 0.04,300,4,50
                       
           }


grid_search = GridSearchCV(clf_pip, param_grid=param_grid,scoring="roc_auc",cv=5)
grid_search.fit(X1_dummy, Y)
print(grid_search.cv_results_)

y_pred = grid_search.predict_proba(test1_X_dummy)
pd.DataFrame(y_pred).to_csv("y_pred3.csv")

