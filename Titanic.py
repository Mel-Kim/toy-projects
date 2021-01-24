#!/usr/bin/env python
# coding: utf-8

# # Titanic: Machine Learning from Diaster
# 
# ## predict survival on the Titanic
# * Defining the problem statement
# * Collectiong the data
# * Exploratory data analysis
# * Feature engineering
# * Feature selection
# * Modelling
# * Testing

# # 1. Defining the problem statement
# Complete the analysis of what sorts of people were likely to survive.
# In particular, we ask you to apply the tools of machine learning to predict which passengers survived the tragedy.

# # 2. Collectiong the data

# ## load train, test dataset using Pandas

# In[110]:


import pandas as pd

train=pd.read_csv('./data/train.csv')
test=pd.read_csv('./data/test.csv')


# # 3. Exploratory data analysis
# Printing first 5 rows of the train dataset

# In[50]:


train.head()


# In[4]:


test.head()


# In[5]:


print(train.shape) #row; col
print(test.shape)


# In[6]:


train.info() # NAN


# In[7]:


test.info()


# In[8]:


train.isnull().sum()  # num of nan


# In[9]:


test.isnull().sum()  # num of nan


# ## import python lib for visualization

# In[111]:


import matplotlib.pyplot as plt
get_ipython().run_line_magic('matplotlib', 'inline')
import seaborn as sns
sns.set() # setting seaborn default for plots


# ## Bar Chart for Categorical Features
# * Pclass
# * Sex
# * SibSp(# of siblings and spouse)
# * Parch(# of parents and children)
# * Embarked
# * Cabin

# In[112]:


def bar_chart(feature):
    survived=train[train['Survived']==1][feature].value_counts()
    dead=train[train['Survived']==0][feature].value_counts()
    df=pd.DataFrame([survived,dead])
    df.index=['Survived', 'Dead']
    df.plot(kind='bar',stacked=True, figsize=(10,5))


# In[25]:


bar_chart('Sex')


# The Chart confirms Women more likey survived than Men

# In[26]:


bar_chart('Pclass')


# The Chart comfirms 1st class more likely survived than other clas

# The Chart confrims 3rd class more likely dead than other classes

# In[27]:


bar_chart('SibSp')


# In[28]:


bar_chart('Parch')


# # 4. Feature engineering
# #### Feature engineering is the process of using domain knowledge of the data to create feautres(feature vectors)that make machine learning algorithms work.
# 
# ##### feature vector is an n-dimensional vector of numerical features that represent some object.
# ##### Many algorithms in machine learning requrie a numerical representation of objects, since such represntation facilitate processing and statistical analysis.

# In[32]:


train.head() 


# ## 4.1 how titanic sank?
# #### sank from the bow of the where third class rooms located
# #### Conclusion, Pclass is key feature for classifier!!!

# ## 4.2 Name

# In[113]:


train_test_data=[train,test] # combining

for dataset in train_test_data:
    dataset['Title']=dataset['Name'].str.extract('([A-Za-z]+)\.', expand=False)


# In[114]:


train['Title'].value_counts()


# In[115]:


title_mapping = {"Mr": 0, "Miss": 1, "Mrs": 2, 
                 "Master": 3, "Dr": 3, "Rev": 3, "Col": 3, "Major": 3, "Mlle": 3,"Countess": 3,
                 "Ms": 3, "Lady": 3, "Jonkheer": 3, "Don": 3, "Dona" : 3, "Mme": 3,"Capt": 3,"Sir": 3 }
for dataset in train_test_data:
    dataset['Title'] = dataset['Title'].map(title_mapping)


# In[107]:


train.isnull().sum() 


# In[46]:


train.head()


# In[60]:


bar_chart('Title')


# In[116]:


# delete unnecessary feature from dataset
train.drop('Name',axis=1, inplace=True)
test.drop('Name',axis=1, inplace=True)


# In[63]:


train.head()


# In[64]:


test.head()


# ## 4.3 Sex
# male;0 female;1

# In[117]:


sex_mapping={"male":0, "female":1}
for dataset in train_test_data:
    dataset['Sex']=dataset['Sex'].map(sex_mapping)


# In[66]:


bar_chart('Sex')


# ## 4.4 Age

# ### 4.4.1 some age is missing(nan)
# Let's use Title's median age for missing age

# In[67]:


train.head(100)


# In[118]:


train['Age'].fillna(train.groupby("Title")["Age"].transform("median"),inplace=True) # 타이틀 별로 나이의 중앙값 사용
test['Age'].fillna(train.groupby("Title")["Age"].transform("median"),inplace=True)


# In[119]:


facet=sns.FacetGrid(train, hue="Survived",aspect=4)
facet.map(sns.kdeplot, 'Age', shade=True)
facet.set(xlim=(0, train['Age'].max()))
facet.add_legend()

plt.show()


# ### 4.2.2 Binning
# Binning/Converting Numerical Age to Categorical Variable
# 
# feature vector map:
# child: 0
# young: 1
# adult: 2
# mid-age: 3
# senior: 4

# In[121]:


for dataset in train_test_data:
    dataset.loc[ dataset['Age'] <= 16, 'Age'] = 0,
    dataset.loc[ (dataset['Age'] > 16) & (dataset['Age'] <= 26), 'Age'] = 1,
    dataset.loc[ (dataset['Age'] > 26) & (dataset['Age'] <= 36), 'Age'] = 2,
    dataset.loc[ (dataset['Age'] > 36) & (dataset['Age'] <= 62), 'Age'] = 3,
    dataset.loc[ dataset['Age'] > 62, 'Age'] = 4


# In[14]:


train.head()


# In[15]:


bar_chart('Age')


# ## 4.5 Embarked

# ### 4.5.1 filling missing values

# In[122]:


Pclass1 = train[train['Pclass']==1]['Embarked'].value_counts()
Pclass2 = train[train['Pclass']==2]['Embarked'].value_counts()
Pclass3 = train[train['Pclass']==3]['Embarked'].value_counts()

df=pd.DataFrame([Pclass1, Pclass2, Pclass3])
df.index=['1st class', '2nd class', '3rd class']
df.plot(kind='bar', stacked=True, figsize=(10,5))


# fill out missing embark with S embark(Since more than 50% of 1st, 2nd, 3rd classes are from S embrak)

# In[123]:


for dataset in train_test_data:
    dataset['Embarked'] = dataset['Embarked'].fillna('S')


# In[19]:


train.head()


# In[20]:


train.isnull().sum()


# In[124]:


embarked_mapping = {"S": 0, "C": 1, "Q": 2}
for dataset in train_test_data:
    dataset['Embarked'] = dataset['Embarked'].map(embarked_mapping)


# In[23]:


train.head()


# ## 4.6 Fare(ticket price)

# In[125]:


# fill missing Fare with median fare for each Pclass
train["Fare"].fillna(train.groupby("Pclass")["Fare"].transform("median"), inplace=True)
test["Fare"].fillna(train.groupby("Pclass")["Fare"].transform("median"), inplace=True)


# In[126]:


facet=sns.FacetGrid(train, hue="Survived", aspect=4)
facet.map(sns.kdeplot, 'Fare', shade=True)
facet.set(xlim=(0, train['Fare'].max()))
facet.add_legend()

plt.show()


# In[66]:


facet=sns.FacetGrid(train, hue="Survived", aspect=4)
facet.map(sns.kdeplot, 'Fare', shade=True)
facet.set(xlim=(0, train['Fare'].max()))
facet.add_legend()

plt.xlim(0,20)


# In[127]:


for dataset in train_test_data:
    dataset.loc[ dataset['Fare'] <= 17, 'Fare'] = 0,
    dataset.loc[ (dataset['Fare'] > 17) & (dataset['Fare'] <= 30), 'Fare'] = 1,
    dataset.loc[ (dataset['Fare'] > 30) & (dataset['Fare'] <= 100), 'Fare'] = 2,
    dataset.loc[ dataset['Fare'] > 100, 'Fare'] = 3


# In[29]:


train.head()


# ## 4.7 Cabin

# In[30]:


train.Cabin.value_counts()


# In[128]:


for dataset in train_test_data:
    dataset['Cabin'] = dataset['Cabin'].str[:1]


# In[129]:


Pclass1 = train[train['Pclass']==1]['Cabin'].value_counts()
Pclass2 = train[train['Pclass']==2]['Cabin'].value_counts()
Pclass3 = train[train['Pclass']==3]['Cabin'].value_counts()

df=pd.DataFrame([Pclass1, Pclass2, Pclass3])
df.index=['1st class', '2nd class', '3rd class']
df.plot(kind='bar', stacked=True, figsize=(10,5))


# In[130]:


cabin_mapping={"A":0, "B":0.4, "C":0.8, "D":1.2, "E":1.6, "F":2, "G":2.4, "T":2.8} # feature scaling
for dataset in train_test_data:
    dataset['Cabin'] = dataset['Cabin'].map(cabin_mapping)


# In[131]:


# fill missing Cabin with median Cabin fir each Pclass
train["Cabin"].fillna(train.groupby("Pclass")["Cabin"].transform("median"), inplace=True)
test["Cabin"].fillna(train.groupby("Pclass")["Cabin"].transform("median"), inplace=True)


# ## 4.8 FamilySize

# In[132]:


train["FamilySize"] = train["SibSp"] + train["Parch"] + 1
test["FamilySize"] = test["SibSp"] + test["Parch"] + 1


# In[133]:


facet=sns.FacetGrid(train, hue="Survived", aspect=4)
facet.map(sns.kdeplot, 'FamilySize', shade=True)
facet.set(xlim=(0, train['FamilySize'].max()))
facet.add_legend()

plt.show()


# In[134]:


family_mapping= {1:0, 2:0.4, 3:0.8, 4:12, 5:1.6, 6:2, 7:2.4, 8:2.8, 9:3.2, 10:3.6, 11:4}
for dataset in train_test_data:
    dataset['FamilySize'] = dataset['FamilySize'].map(family_mapping)


# In[38]:


train.head()


# In[39]:


test.head()


# In[135]:


features_drop=['Ticket','SibSp','Parch']
train=train.drop(features_drop, axis=1)
test=test.drop(features_drop, axis=1)
train=train.drop(['PassengerId'], axis=1)


# In[136]:


train_data=train.drop('Survived', axis=1)
target=train['Survived']

train_data.shape, target.shape


# In[76]:


train_data.head(10)


# In[77]:


target.head()


# # 5. Modelling

# In[137]:


# Importing Classifier Modules
from sklearn.neighbors import KNeighborsClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.svm import SVC

import numpy as np


# In[102]:


train.info()


# ## 6.2 Cross Validation(K-fold)

# In[138]:


from sklearn.model_selection import KFold
from sklearn.model_selection import cross_val_score
k_fold = KFold(n_splits=10, shuffle=True, random_state=0)


# In[139]:


train_data.isnull().sum() 


# In[140]:


target.isnull().sum() 


# ### 6.2.1 kNN

# In[141]:


clf = KNeighborsClassifier(n_neighbors = 13)
scoring = 'accuracy'
score = cross_val_score(clf, train_data, target, cv=k_fold, n_jobs=1, scoring=scoring)
print(score)


# In[142]:


# kNN Score
round(np.mean(score)*100, 2)


# ### 6.2.2 Decision Tree

# In[143]:


clf = DecisionTreeClassifier()
scoring = 'accuracy'
score = cross_val_score(clf, train_data, target, cv=k_fold, n_jobs=1, scoring=scoring)
print(score)


# In[144]:


# decision tree Score
round(np.mean(score)*100, 2)


# ### 6.2.3 Random Forest

# In[146]:


clf = RandomForestClassifier(n_estimators=13)
scoring = 'accuracy'
score = cross_val_score(clf, train_data, target, cv=k_fold, n_jobs=1, scoring=scoring)
print(score)


# In[147]:


# Random Forest Score
round(np.mean(score)*100, 2)


# ### 6.2.4 Naive Bayes

# In[148]:


clf = GaussianNB()
scoring = 'accuracy'
score = cross_val_score(clf, train_data, target, cv=k_fold, n_jobs=1, scoring=scoring)
print(score)


# In[149]:


# Naive Bayes Score
round(np.mean(score)*100, 2)


# ### 6.2.5 SVM

# In[150]:


clf = SVC()
scoring = 'accuracy'
score = cross_val_score(clf, train_data, target, cv=k_fold, n_jobs=1, scoring=scoring)
print(score)


# In[151]:


# SVM Score
round(np.mean(score)*100, 2)


# # 7. Testing

# In[162]:


clf = SVC()
clf.fit(train_data, target)

test_data = test.drop("PassengerId", axis=1).copy()
prediction = clf.predict(test_data)


# In[170]:


submission = pd.DataFrame({
        "PassengerId": test["PassengerId"],
        "Survived": prediction
    })

submission.to_csv('submission.csv', index=False)


# In[171]:


submission = pd.read_csv('submission.csv')
submission.head()

