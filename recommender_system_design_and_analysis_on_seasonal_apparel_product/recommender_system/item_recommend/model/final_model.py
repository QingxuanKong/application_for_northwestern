import os
import sys

sys.path.insert(0, '/Users/qingxuankong/PycharmProjects/amazon/recommender_system')

import django

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'recommender_system.settings')
os.environ["DJANGO_ALLOW_ASYNC_UNSAFE"] = "true"
os.environ['TF_XLA_FLAGS'] = '--tf_xla_enable_xla_devices'
django.setup()

import operator
import pandas as pd
import numpy as np
import itertools
from gensim.models import Word2Vec
from keras.preprocessing import sequence
from keras.layers.recurrent import LSTM
from keras.layers.embeddings import Embedding
from keras.models import Sequential
from keras.layers import Dense, Activation, Bidirectional, Flatten
from keras_self_attention import SeqSelfAttention
from item_recommend.models import Items, Customers, Ratings, User_Recommendations, Item_Recommendations


def recommend_items(len_thres, top_k):
    print('preparing item dict...')
    item_dict = create_item_dict()

    print('preparing user item dict...')
    user_item_dict = create_user_item_dict(len_thres)
    total_users_list = list(user_item_dict.keys())
    total_items_list = list(set(itertools.chain.from_iterable(user_item_dict.values())))

    print('training item2vec model...')
    item2vec_model = train_item2vec_model(user_item_dict.values(), total_items_list)

    print('building user-item matrix...')
    user_item_matrix = create_user_item_matrix(user_item_dict, total_users_list, total_items_list)

    print('building item-item matrix...')
    item_item_matrix = create_item_item_matrix(item2vec_model, total_items_list)

    print('calculating new user-item matrix...')
    new_user_item_matrix = user_item_matrix.dot(item_item_matrix)

    print('predict using item2vec model...')
    predict_item_sequences_item2vec = predict_item2vec_model(top_k, total_users_list, user_item_matrix,
                                                             new_user_item_matrix, item_dict)

    print('preparing embedding layer...')
    pretrained_weights = item2vec_model.wv.vectors
    vocab_size, embedding_size = pretrained_weights.shape
    print('embedding shape:', pretrained_weights.shape)

    train_item_sequences_x, train_item_sequences_y, test_item_sequences = split_user_item_dict(user_item_dict)

    print('preparing train user item sequence...')
    train_users, train_x_wv_array, train_y_wv_array = create_item_sequence(item2vec_model, train_item_sequences_x,
                                                                           train_item_sequences_y)

    print('training bi-lstm model...')
    bi_lstm_model = train_bi_lstm_model(train_x_wv_array, train_y_wv_array, vocab_size, embedding_size,
                                        pretrained_weights)

    print('predict using bi-lstm model...')
    predict_item_sequences_bilstm = predict_bi_lstm_model(bi_lstm_model, item2vec_model, train_users, train_x_wv_array,
                                                          top_k, item_dict)

    print('fetch top selling items list...')
    top_selling_items = fetch_top_selling_items(top_k)

    print('creating user recommendation list...')
    predict_item_sequences = create_user_recommendations(total_users_list, predict_item_sequences_bilstm,
                                                         predict_item_sequences_item2vec, top_selling_items)

    print('creating item recommendation list...')
    similar_item_sequences = create_items_recommendations(total_items_list, item2vec_model, top_selling_items, top_k)

    print('inserting user recommendation list...')
    insert_user_recommendations(predict_item_sequences)

    print('inserting item recommendation list...')
    insert_item_recommendations(similar_item_sequences)

    return predict_item_sequences, similar_item_sequences


def create_item_dict():
    df = pd.DataFrame(list(Items.objects.all().values()))
    df_shrt = df[['asin', 'title']]
    item_dict = dict(zip(df_shrt.asin, df_shrt.title))

    return item_dict


def create_user_item_dict(len_thres):
    df = pd.DataFrame(list(Ratings.objects.all().values()))

    # read raw rating file
    df_shrt = df[['reviewerID_id', 'asin_id', 'reviewTime']]
    df_shrt = df_shrt.drop_duplicates()
    df_shrt = df_shrt.sort_values(by='reviewTime')
    df_shrt.reset_index(inplace=True)

    # create user rating records
    user_rating_records = {}
    for i in range(df_shrt.shape[0]):
        try:
            if df_shrt.loc[i, 'reviewerID_id'] not in user_rating_records.keys():
                user_rating_records[df_shrt.loc[i, 'reviewerID_id']] = []
            user_rating_records[df_shrt.loc[i, 'reviewerID_id']].append(str(df_shrt.loc[i, 'asin_id']))
        except:
            pass

    # remove one item record
    user_rating_records_cleaned = {}
    for key, value in user_rating_records.items():
        if len(value) >= len_thres:
            user_rating_records_cleaned[key] = value
    print('user item dict size:', len(user_rating_records_cleaned))

    return user_rating_records_cleaned


def train_item2vec_model(item_sentences, items_list):
    model = Word2Vec(item_sentences, size=32, window=3, min_count=2, sample=1e-3,
                     negative=5, workers=12, iter=1000, sg=1, hs=0)
    model.build_vocab(items_list, update=True)
    model.train(item_sentences, total_examples=model.corpus_count, epochs=22)
    model.init_sims()
    word_vectors = model.wv
    word_vectors.save('./data/model/item_recommendations')

    return model


def create_user_item_matrix(item_sequences, users_list, items_list):
    user_item_matrix = pd.DataFrame(np.zeros((len(users_list), len(items_list))))
    user_item_matrix.columns = items_list
    user_item_matrix.index = users_list

    for key, values in item_sequences.items():
        for value in values:
            user_item_matrix.at[key, value] = 1

    return user_item_matrix


def create_item_item_matrix(item2vec_model, items_list):
    item_item_matrix = pd.DataFrame(np.zeros((len(items_list), len(items_list))))
    item_item_matrix.columns = items_list
    item_item_matrix.index = items_list

    for i in items_list:
        print(items_list.index(i), ' of ', len(items_list))
        for j in items_list:
            if i in item2vec_model.wv.vocab and j in item2vec_model.wv.vocab:
                item_item_matrix.at[i, j] = item2vec_model.wv.similarity(i, j)

    return item_item_matrix


def predict_item2vec_model(top_k, users_list, user_item_matrix, new_user_item_matrix, item_dict):
    user_liked = {}
    user_may_like = {}
    for user in users_list:
        user_records = user_item_matrix.loc[user, :].to_dict()
        user_liked[user] = []
        for key, value in user_records.items():
            if value == 1:
                user_liked[user].append(key)

        user_favor = new_user_item_matrix.loc[user, :].to_dict()
        user_favor = sorted(user_favor, key=user_favor.get, reverse=True)
        user_may_like[user], titles_list = [], []
        count = 0
        for item in user_favor:
            item_title = item_dict[item]
            if item not in user_liked[user] and item_title not in titles_list:
                user_may_like[user].append(item)
                titles_list.append(item_title)
                count += 1
            if count == top_k:
                break

    return user_may_like


def split_user_item_dict(user_item_dict):
    max_len = max([len(i) for i in user_item_dict.values()])
    min_len = min([len(i) for i in user_item_dict.values()])

    for i in range(5, 5 + 1):
        user_slctd = [k for k, v in user_item_dict.items() if len(v) >= i]
        item_sequences_slctd = [v for k, v in user_item_dict.items() if len(v) >= i]
        train_item_sequences_x = [j[:i - 1] for j in item_sequences_slctd]
        train_item_sequences_y = [j[i - 1:i] for j in item_sequences_slctd]
        test_item_sequences = [j[i - 1:] for j in item_sequences_slctd]

        train_item_sequences_x = dict(zip(user_slctd, train_item_sequences_x))
        train_item_sequences_y = dict(zip(user_slctd, train_item_sequences_y))
        test_item_sequences = dict(zip(user_slctd, test_item_sequences))

    return train_item_sequences_x, train_item_sequences_y, test_item_sequences


def create_item_sequence(item2vec_model, train_x, train_y):
    train_users, train_x_wv, train_y_wv = [], [], []
    max_len = 0

    for i in range(len(train_x)):
        vocab_flag_x = min([j in item2vec_model.wv.vocab for j in list(train_x.values())[i]])
        vocab_flag_y = min([j in item2vec_model.wv.vocab for j in list(train_y.values())[i]])
        vocab_flag = min(vocab_flag_x, vocab_flag_y)
        if vocab_flag == 1:
            if max_len < len(list(train_x.values())[i]):
                max_len = len(list(train_x.values())[i])
            user = list(train_x.keys())[i]
            x_wv = [item2vec_model.wv.vocab[j].index for j in list(train_x.values())[i]]
            y_wv = [item2vec_model.wv.vocab[j].index for j in list(train_y.values())[i]]
            train_users.append(user)
            train_x_wv.append(x_wv)
            train_y_wv.append(y_wv)

    train_x_wv = sequence.pad_sequences(train_x_wv, maxlen=max_len)
    train_x_wv_array = np.asarray(train_x_wv).astype('int32')
    train_y_wv_array = np.asarray(train_y_wv).astype('int32')
    print('train shape:', train_x_wv_array.shape, train_y_wv_array.shape)

    return train_users, train_x_wv_array, train_y_wv_array


def train_bi_lstm_model(train_x_wv_array, train_y_wv_array, vocab_size, embedding_size,
                        pretrained_weights):
    model = Sequential()
    model.add(Embedding(input_dim=vocab_size, output_dim=embedding_size, weights=[pretrained_weights]))
    model.add(Bidirectional(LSTM(units=embedding_size, return_sequences=True)))
    model.add(SeqSelfAttention(attention_activation='softmax'))
    model.add(Dense(units=vocab_size))
    model.add(Flatten())
    model.add(Activation('softmax'))
    model.compile(optimizer='adam', loss='sparse_categorical_crossentropy')
    model.summary()

    model.fit(train_x_wv_array, train_y_wv_array, epochs=100, verbose=2)

    model.save('./data/model/user_recommendations')

    return model


def predict_bi_lstm_model(bi_lstm_model, item2vec_model, train_users, train_x_wv_array, top_k, item_dict):
    predict_item_sequences = {}
    for i in range(len(train_x_wv_array)):
        predict_dict = {v: k for v, k in enumerate(bi_lstm_model.predict(train_x_wv_array[i])[-1])}
        predict_dict_sorted = dict(sorted(predict_dict.items(), key=operator.itemgetter(1), reverse=True))

        titles_list, user_sequences = [], []
        item_cnt = 0
        for item_wv in predict_dict_sorted.keys():
            item_asin = item2vec_model.wv.index2word[item_wv]
            item_title = item_dict[item_asin]

            if item_title not in titles_list:
                titles_list.append(item_title)
                user_sequences.append(item_asin)
                item_cnt += 1
            if item_cnt == top_k:
                break

        predict_item_sequences[train_users[i]] = user_sequences

    return predict_item_sequences


def fetch_top_selling_items(top_k):
    df = pd.DataFrame(list(Items.objects.all().values()))
    df = df.sort_values(by=['category_rank']).reset_index(drop=True)
    top_selling_items = df.loc[:top_k - 1, 'asin'].tolist()

    return top_selling_items


def create_user_recommendations(total_users_list, predict_item_sequences_bilstm, predict_item_sequences_item2vec,
                                top_selling_items):
    predict_item_sequences = {}
    for user in total_users_list:
        if user in predict_item_sequences_bilstm.keys():
            predict_item_sequences[user] = predict_item_sequences_bilstm[user]
        elif user in predict_item_sequences_item2vec.keys():
            predict_item_sequences[user] = predict_item_sequences_item2vec[user]
    #         else:
    #             predict_item_sequences[user] = top_selling_items

    return predict_item_sequences


def create_items_recommendations(total_items_list, item2vec_model, top_selling_items, top_k):
    similar_item_sequences = {}
    for item in total_items_list:
        if item in item2vec_model.wv.vocab:
            similar_item_sequences[item] = []
            similar_item_cnt = 0
            for tuple in item2vec_model.wv.most_similar(item, topn=top_k + 10):
                if tuple[0] in total_items_list:
                    similar_item_sequences[item].append(tuple[0])
                    similar_item_cnt += 1
                if similar_item_cnt == top_k:
                    break
    #         else:
    #             similar_item_sequences[item]=top_selling_items

    return similar_item_sequences


def insert_user_recommendations(predict_item_sequences):
    User_Recommendations.objects.all().delete()
    for key, value in predict_item_sequences.items():
        user_recommendation = User_Recommendations()
        user_recommendation.reviewerID = Customers.objects.filter(reviewerID=key)[0]
        user_recommendation.recomm_asin_1 = Items.objects.filter(asin=value[0])[0]
        user_recommendation.recomm_asin_2 = Items.objects.filter(asin=value[1])[0]
        user_recommendation.recomm_asin_3 = Items.objects.filter(asin=value[2])[0]
        user_recommendation.recomm_asin_4 = Items.objects.filter(asin=value[3])[0]
        user_recommendation.recomm_asin_5 = Items.objects.filter(asin=value[4])[0]
        user_recommendation.recomm_asin_6 = Items.objects.filter(asin=value[5])[0]
        user_recommendation.recomm_asin_7 = Items.objects.filter(asin=value[6])[0]
        user_recommendation.recomm_asin_8 = Items.objects.filter(asin=value[7])[0]
        user_recommendation.recomm_asin_9 = Items.objects.filter(asin=value[8])[0]
        user_recommendation.recomm_asin_10 = Items.objects.filter(asin=value[9])[0]
        user_recommendation.save()


def insert_item_recommendations(similar_item_sequences):
    Item_Recommendations.objects.all().delete()
    for key, value in similar_item_sequences.items():
        item_recommendation = Item_Recommendations()
        item_recommendation.asin = Items.objects.filter(asin=key)[0]
        item_recommendation.recomm_asin_1 = Items.objects.filter(asin=value[0])[0]
        item_recommendation.recomm_asin_2 = Items.objects.filter(asin=value[1])[0]
        item_recommendation.recomm_asin_3 = Items.objects.filter(asin=value[2])[0]
        item_recommendation.recomm_asin_4 = Items.objects.filter(asin=value[3])[0]
        item_recommendation.recomm_asin_5 = Items.objects.filter(asin=value[4])[0]
        item_recommendation.recomm_asin_6 = Items.objects.filter(asin=value[5])[0]
        item_recommendation.recomm_asin_7 = Items.objects.filter(asin=value[6])[0]
        item_recommendation.recomm_asin_8 = Items.objects.filter(asin=value[7])[0]
        item_recommendation.recomm_asin_9 = Items.objects.filter(asin=value[8])[0]
        item_recommendation.recomm_asin_10 = Items.objects.filter(asin=value[9])[0]
        item_recommendation.save()


if __name__ == '__main__':
    len_thres = 3
    top_k = 10
    predict_item_sequences, similar_item_sequences = recommend_items(len_thres, top_k)
