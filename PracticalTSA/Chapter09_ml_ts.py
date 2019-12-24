# Chapter 9. Machine Learning for Time Series
from cesium import datasets
from cesium import featurize
import time

eeg = datasets.fetch_andrzejak()

import matplotlib.pyplot as plt
plt.subplot(3, 1, 1)
plt.plot(eeg["measurements"][0])
plt.legend(eeg['classes'][0])
plt.subplot(3, 1, 2)
plt.plot(eeg["measurements"][300])
plt.legend(eeg['classes'][300])
plt.subplot(3, 1, 3)
plt.plot(eeg["measurements"][450])
plt.legend(eeg['classes'][450])
# plt.show()

# a = [1, 2, 3, 4, 5]
# b = [1, 2, 3, 4, 5]
# import matplotlib.pyplot as plt
# plt.scatter(a, b)
# plt.show()

# feature selection
features_to_use = ["amplitude", "percent_beyond_1_std",
                    "percent_close_to_median",
                    "skew",
                    "max_slope"]

fset_cesium = featurize.featurize_time_series(
                 times = eeg["times"],
                 values = eeg["measurements"],
                 errors = None,
                 features_to_use = features_to_use,
                 scheduler = None)
print(fset_cesium.head())

# print(np.std(eeg_small["measurements"][0]))
# print(np.mean(eeg_small["measurements"][0]))

# machine learning on the data set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(
    fset_cesium.values, eeg["classes"], random_state = 21)

from sklearn.ensemble import RandomForestClassifier
rf_clf = RandomForestClassifier(n_estimators = 10,
                                max_depth = 3,
                                random_state = 21)

rf_clf.fit(X_train, y_train)
print('accuracy based on rf: ', rf_clf.score(X_test, y_test))

# xgboost
import xgboost as xgb
xgb_clf = xgb.XGBClassifier(n_estimators = 10,
                           max_depth = 3,
                           random_state = 21)

xgb_clf.fit(X_train, y_train)
print('accuracy based on xgb: ', xgb_clf.score(X_test, y_test))

# computational time / speed
start = time.time()
xgb_clf.fit(X_train, y_train)
end = time.time()
print('start - end = ', end - start)
