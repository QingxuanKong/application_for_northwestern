from django.urls import path, re_path
from . import views
from django.conf.urls.static import static
from django.conf import settings

urlpatterns = [
    path('homepage', views.homepage, name="homepage"),
    path('register', views.register, name="register"),
    path('', views.logout, name="logout"),
    path('catalog', views.catalog, name="catalog"),
    path('catalog/<str:search_item>', views.catalog, name="catalog_search"),
    path('catalog/<str:search_item>/<str:order_by>/<int:page>', views.catalog, name="catalog_search_order"),
    path('item/<str:asin>', views.single_product, name="item"),
    path('wishlist', views.wishlist, name="wishlist"),
    path('wishlist/add/<str:asin>', views.add_to_wishlist, name="add_to_wishlist"),
    path('wishlist/remove/<str:asin>', views.remove_from_wishlist, name="remove_from_wishlist")
]
