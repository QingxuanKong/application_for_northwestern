import os
import sys

sys.path.insert(0, '/Users/qingxuankong/PycharmProjects/amazon/recommender_system')

import django

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'recommender_system.settings')
django.setup()

from data_prepocessing import clean_item_data, clean_customer_data, clean_rating_data
from item_recommend.models import Items, Customers, Ratings, User_Recommendations, Item_Recommendations
from django.conf import settings
from django.utils.timezone import make_aware


os.chdir('..')
print(os.getcwd())

User_Recommendations.objects.all().delete()
Item_Recommendations.objects.all().delete()
Ratings.objects.all().delete()
Items.objects.all().delete()
Customers.objects.all().delete()

df_item = clean_item_data('./data/raw_data/meta_AMAZON_FASHION.json')
for i in range(df_item.shape[0]):
    item = Items()
    item.asin = df_item.loc[i, 'asin']
    item.title = df_item.loc[i, 'title']
    item.image = df_item.loc[i, 'image']
    item.brand = df_item.loc[i, 'brand']
    item.category = df_item.loc[i, 'category']
    item.category_rank = df_item.loc[i, 'category_rank']
    item.description = df_item.loc[i, 'description']
    item.price = df_item.loc[i, 'price']
    print(item)
    item.save()

df_customer = clean_customer_data('./data/raw_data/AMAZON_FASHION.json')
for i in range(df_customer.shape[0]):
    customer = Customers()
    customer.reviewerID = df_customer.loc[i, 'reviewerID']
    customer.reviewerName = df_customer.loc[i, 'reviewerName']
    print(customer)
    customer.save()

settings.TIME_ZONE
df_rating = clean_rating_data('./data/raw_data/AMAZON_FASHION.json', df_item, df_customer)
for i in range(df_rating.shape[0]):
    rating = Ratings()
    rating.overall = df_rating.loc[i, 'overall']
    rating.verified = df_rating.loc[i, 'verified']
    rating.reviewTime = make_aware(df_rating.loc[i, 'reviewTime'])
    rating.reviewerID = Customers.objects.filter(reviewerID=df_rating.loc[i, 'reviewerID'])[0]
    rating.asin = Items.objects.filter(asin=df_rating.loc[i, 'asin'])[0]
    rating.reviewText = df_rating.loc[i, 'reviewText']
    rating.summary = df_rating.loc[i, 'summary']
    rating.vote = df_rating.loc[i, 'vote']
    print(rating)
    rating.save()
