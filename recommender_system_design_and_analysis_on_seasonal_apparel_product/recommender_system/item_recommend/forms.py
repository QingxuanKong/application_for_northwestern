from django import forms
from item_recommend.models import Items, Customers, Ratings, User_Recommendations, Item_Recommendations


class SearchForm(forms.Form):
    search = forms.CharField()


class OrderByForm(forms.Form):
    order_by = forms.CharField()
