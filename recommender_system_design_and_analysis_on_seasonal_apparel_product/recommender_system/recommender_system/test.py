import os
import pandas as pd

os.chdir('..')
print(os.getcwd())

print('reading user-item...')
user_item_matrix = pd.read_csv('./item_recommend/data/model_data/item2vec/user_item_matrix.csv')
user_item_matrix.index = user_item_matrix['Unnamed: 0']
user_item_matrix = user_item_matrix.iloc[:, 1:]
# B007PSJLN2
print('reading new user-item ...')
new_user_item_matrix = pd.read_csv('./item_recommend/data/model_data/item2vec/new_user_item_matrix.csv')
new_user_item_matrix.index = new_user_item_matrix['Unnamed: 0']
new_user_item_matrix = new_user_item_matrix.iloc[:, 1:]
users_list = list(user_item_matrix.index)
top_k = 10

print(user_item_matrix)
print(new_user_item_matrix)
print(users_list)

user_liked = {}
user_may_like = {}
for user in users_list:
    temp = user_item_matrix.loc[user, :]
    print(temp)
    temp = temp.to_dict('records')
    print(temp)
    user_records = user_item_matrix.loc[user, :].to_dict('records')

    user_liked[user] = []
    for key, value in user_records.items():
        if value == 1:
            user_liked[user].append(key)

    user_favor = new_user_item_matrix.loc[user, :].to_dict('records')
    user_favor = sorted(user_favor, key=user_favor.get, reverse=True)
    user_may_like[user] = []
    count = 0
    for key, value in user_favor.items():
        if count == top_k:
            break
        if value not in user_liked[user]:
            user_may_like[user].append(value)
            count += 1
