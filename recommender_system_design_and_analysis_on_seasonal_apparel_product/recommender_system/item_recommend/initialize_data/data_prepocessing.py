import pandas as pd
import os
from datetime import datetime
import numpy as np
import re


def clean_item_data(meta_file):
    df_meta = pd.read_json(meta_file, lines=True)
    df_meta = df_meta.groupby('asin').sample(1).reset_index()

    df_meta['rank'] = df_meta['rank'].str.replace('amp;', ' & ')
    df_meta['rank'] = df_meta['rank'].str.replace(', ', ',')
    df_meta['rank'] = df_meta['rank'].str.replace(',', ', ')
    df_meta['rank'] = df_meta['rank'].str.replace('Clothing,ShoesJewelry', 'Clothing, Shoes & Jewelry')
    df_meta['rank'] = df_meta['rank'].str.replace('Clothing, ShoesJewelry', 'Clothing, Shoes & Jewelry')
    df_meta['rank'] = df_meta['rank'].str.replace('SportsOutdoors', 'Sports & Outdoors')
    df_meta['rank'] = df_meta['rank'].str.replace('KitchenDining', 'Kitchen & Dining')
    df_meta['rank'] = df_meta['rank'].str.replace('HomeKitchen', 'Home & Kitchen')
    df_meta['rank'] = df_meta['rank'].str.replace('HealthHousehold', 'Health & Household')
    df_meta['rank'] = df_meta['rank'].str.replace('ToolsHomeImprovement', 'Tools & Home Improvement')
    df_meta['rank'] = df_meta['rank'].str.replace('IndustrialScientific', 'Industrial & Scientific')
    df_meta['rank'] = df_meta['rank'].str.replace('GroceryGourmetFood', 'Grocery & Gourmet Food')
    df_meta['rank'] = df_meta['rank'].str.replace('ToysGames', 'Toys & Games')
    df_meta['rank'] = df_meta['rank'].str.replace('PetSupplies', 'Pet Supplies')
    df_meta['rank'] = df_meta['rank'].str.replace('BeautyPersonalCare', 'Beauty & Personal Care')
    df_meta['rank'] = df_meta['rank'].str.replace('CellPhonesAccessories', 'Cell Phones & Accessories')
    df_meta['rank'] = df_meta['rank'].str.replace('AmazonLaunchpad', 'Amazon Launchpad')
    df_meta['rank'] = df_meta['rank'].str.replace('OfficeProducts', 'Office Products')
    df_meta['rank'] = df_meta['rank'].str.replace('Crafts Sewing', 'Crafts & Sewing')
    df_meta['rank'] = df_meta['rank'].str.replace('CraftsSewing', 'Crafts & Sewing')
    df_meta['rank'] = df_meta['rank'].str.replace('LawnGarden', 'Lawn & Garden')

    df_meta['category'] = ''
    df_meta['category_rank'] = ''
    for i in range(df_meta.shape[0]):
        # if i % 5000 == 0:
        #     print(i)

        df_meta.at[i, 'price'] = str(df_meta.loc[i, 'price']).split('-')[0].strip()

        df_meta.at[i, 'category'] = str(df_meta.loc[i, 'rank'])[str(df_meta.loc[i, 'rank']).find('in') + 2:]
        df_meta.at[i, 'category'] = str(df_meta.loc[i, 'category']).split('(')[0].strip()
        df_meta.at[i, 'category'] = str(df_meta.loc[i, 'category']).split('>')[0].strip()
        df_meta.at[i, 'category_rank'] = str(df_meta.loc[i, 'rank'])[:str(df_meta.loc[i, 'rank']).find('in')]
        if '>#' in df_meta.loc[i, 'category_rank']:
            df_meta.at[i, 'category_rank'] = str(df_meta.loc[i, 'category_rank'])[
                                             str(df_meta.loc[i, 'category_rank']).find('>#') + 2:]

        if type(df_meta.loc[i, 'description']) is list:
            desc_list = df_meta.loc[i, 'description']
            desc_str = ''
            for desc in desc_list:
                # desc = desc.replace(' < br / > ', '\n')
                # desc = desc.replace('< br / > ', '\n')
                # if j != "'" and j != "." and j != "*":
                desc_str += desc.strip() + ' '
            df_meta.at[i, 'description'] = desc_str.strip()
        else:
            df_meta.at[i, 'description'] = ''

        if type(df_meta.loc[i, 'image']) is list:
            image = df_meta.loc[i, 'image']
            df_meta.at[i, 'image'] = image[0]
        else:
            df_meta.at[i, 'image'] = ''

        title = df_meta.loc[i, 'title']
        if type(title) is str and 'var aPageStart' in title:
            df_meta.drop(index=i, inplace=True)

    df_meta['category_rank'] = df_meta['category_rank'].replace('na', '')
    df_meta['category_rank'] = df_meta['category_rank'].replace('na', '')
    df_meta['category_rank'] = df_meta['category_rank'].replace('', np.nan)
    df_meta['category_rank'] = df_meta['category_rank'].str.replace(', ', '').astype(np.float).astype("Int32")

    df_meta['price'] = df_meta['price'].str.replace('$', '')
    df_meta['price'] = df_meta['price'].str.replace(',', '')
    df_meta['price'] = df_meta['price'].astype(float)

    df_meta['category'] = df_meta['category'].replace('an', '')
    df_meta['description'] = df_meta['description'].replace('', np.nan)

    df_meta = df_meta[['asin', 'title', 'image', 'brand', 'category', 'category_rank', 'description', 'price']]
    df_meta = df_meta[df_meta['category'] == 'Clothing, Shoes & Jewelry']
    df_meta = df_meta.reset_index(drop=True)
    #     df_meta = df_meta.fillna('')

    item_list = df_meta.to_dict('records')
    return df_meta


def clean_customer_data(rating_file):
    df_rating = pd.read_json(rating_file, lines=True)
    df_shrt = df_rating[['reviewerID', 'reviewerName', 'unixReviewTime']].drop_duplicates()
    df_shrt['rank'] = df_shrt.groupby('reviewerID')['unixReviewTime'].rank(method='first', ascending=False)
    df_customer = df_shrt[df_shrt['rank'] == 1][['reviewerID', 'reviewerName']]
    df_customer = df_customer.reset_index(drop=True)
    #     df_customer = df_customer.fillna('')

    customer_list = df_customer.to_dict('records')
    return df_customer


def clean_rating_data(rating_file, item_file, customer_file):
    df_rating = pd.read_json(rating_file, lines=True)
    df_rating['reviewTime'] = [datetime.fromtimestamp(x) for x in
                               df_rating['unixReviewTime']]

    df_rating = df_rating[
        ['overall', 'verified', 'reviewTime', 'reviewerID', 'asin', 'reviewText', 'summary', 'vote']]
    df_rating = df_rating.merge(item_file, how='inner')
    df_rating = df_rating.merge(customer_file, how='inner')
    df_rating['overall'] = df_rating['overall'].astype(np.float).astype('Int32')
    df_rating['vote'] = df_rating['vote'].fillna(0).astype('Int32')
    df_rating = df_rating.reset_index(drop=True)
    #     df_rating = df_rating.fillna('')

    rating_list = df_rating.to_dict('records')
    return df_rating


if __name__ == '__main__':
    os.chdir('..')
    print(os.getcwd())

    df_item = clean_item_data('./data/raw_data/meta_AMAZON_FASHION.json')
    df_customer = clean_customer_data('./data/raw_data/AMAZON_FASHION.json')
    df_rating = clean_rating_data('./data/raw_data/AMAZON_FASHION.json', df_item, df_customer)

    # df_item.to_csv('./data/model_data/item2vec/items_cleaned.csv')
    # df_customer.to_csv('./data/model_data/item2vec/customer_cleaned.csv')
    # df_rating.to_csv('./data/model_data/item2vec/rating_cleaned.csv')
