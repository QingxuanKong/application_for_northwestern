from django.shortcuts import render, get_object_or_404, redirect
from django.core.paginator import Paginator
from .models import Items, Customers, Ratings, User_Recommendations, Item_Recommendations, User_Wishlist
import pandas as pd
from django.db.models import Q
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth import authenticate, login, logout
from django.contrib.auth.decorators import login_required
from django.http import HttpResponseRedirect, JsonResponse


def fetch_top_selling_items(top_k):
    df = pd.DataFrame(list(Items.objects.all().values()))
    df = df.sort_values(by=['category_rank']).reset_index(drop=True)
    top_selling_items_list, titles_list = [], []
    item_cnt = 0
    for i in range(df.shape[0]):
        if df.loc[i, 'title'] not in titles_list:
            titles_list.append(df.loc[i, 'title'])
            top_selling_items_list.append(df.loc[i, 'asin'])
            item_cnt += 1
        if item_cnt == top_k:
            break

    top_selling_items = []
    for i in top_selling_items_list:
        top_selling_item = get_object_or_404(Items, pk=i)
        top_selling_items.append(top_selling_item)

    return top_selling_items_list


def user_logged_in(request):
    if request.user.is_authenticated:
        return request.user.username
    else:
        return None


def homepage(request):
    user = user_logged_in(request)
    print(user)

    if user:
        if User_Recommendations.objects.filter(reviewerID=user).exists():
            recommendation_items_q = User_Recommendations.objects.get(reviewerID=user)
            recommendation_items_list = [recommendation_items_q.recomm_asin_1_id,
                                         recommendation_items_q.recomm_asin_2_id,
                                         recommendation_items_q.recomm_asin_3_id,
                                         recommendation_items_q.recomm_asin_4_id,
                                         recommendation_items_q.recomm_asin_5_id,
                                         recommendation_items_q.recomm_asin_6_id,
                                         recommendation_items_q.recomm_asin_7_id,
                                         recommendation_items_q.recomm_asin_8_id,
                                         recommendation_items_q.recomm_asin_9_id,
                                         recommendation_items_q.recomm_asin_10_id]
            print('model recommend exists')
        else:
            recommendation_items_list = fetch_top_selling_items(10)
            print('model recommend not exists')
    else:
        recommendation_items_list = fetch_top_selling_items(10)
        print('model recommend not exists')

    recommendation_items, wishlist_status_items = [], []
    for i in recommendation_items_list:
        recommendation_item = get_object_or_404(Items, pk=i)
        recommendation_items.append(recommendation_item)
        wishlist_status_items.append(check_wishlist_status(user, i))
    recommendation_items_zip = zip(recommendation_items, wishlist_status_items)

    return render(
        request,
        'item_recommend/index.html',
        {
            'user': user,
            'recommendation_items_zip': recommendation_items_zip,
        }
    )


def register(request):
    if request.method == 'POST':
        form = UserCreationForm(request.POST)

        if form.is_valid():
            form.save()
            username = form.cleaned_data['username']
            password = form.cleaned_data['password1']
            user = authenticate(username=username, password=password)
            login(request, user)
            return redirect('homepage')
        else:
            for msg in form.error_messages:
                print(msg)

    else:
        form = UserCreationForm()

    return render(request, 'registration/register.html', {'form': form})


def logout_view(request):
    logout(request)


def catalog(request, search_item=None, order_by=None, page=1):
    user = user_logged_in(request)

    if request.method == 'GET' and request.GET:
        if request.GET.get('search'):
            search_item = request.GET.get('search')
        if request.GET.get('order-by'):
            order_by = request.GET.get('order-by')
        if request.GET.get('page'):
            page = request.GET.get('page')

    if order_by is None or order_by == 'default-sorting':
        order_by = 'default-sorting'
        items = Items.objects.all().order_by('asin')
    elif order_by == 'price-low-to-high':
        items = Items.objects.all().order_by('price')
    elif order_by == 'price-high-to-low':
        items = Items.objects.all().order_by('-price')
    elif order_by == 'by-popularity':
        items = Items.objects.all().order_by('category_rank')

    if search_item and search_item != 'None':
        items = items.filter(Q(title__icontains=search_item))

    top_selling_items_list = fetch_top_selling_items(10)
    top_selling_items = []
    for i in top_selling_items_list:
        top_selling_item = get_object_or_404(Items, pk=i)
        top_selling_items.append(top_selling_item)


    paginator = Paginator(items, 12)
    page_num = paginator.num_pages
    page_items = paginator.page(page)

    if page_items.has_next():
        next_page = page + 1
    else:
        next_page = page
    if page_items.has_previous():
        previous_page = page - 1
    else:
        previous_page = page

    return render(
        request,
        'item_recommend/shop-catalog.html',
        {
            'user': user,
            'order_by': order_by,
            'search_item': search_item,

            'top_selling_items': top_selling_items,

            'items': page_items,
            'total_item_cnt': len(items),

            'item_lower_range': page * 12 - 11,
            'item_upper_range': min(page * 12, len(items)),
            'page_num': range(page, min(page + 6, page_num + 1)),
            'curr_page': page,
            'next_page': next_page,
            'previous_page': previous_page
        }
    )


def single_product(request, asin):
    user = user_logged_in(request)
    item = get_object_or_404(Items, pk=asin)
    wishlist_status = check_wishlist_status(user, asin)

    ratings = Ratings.objects.filter(asin=asin).order_by('-reviewTime')
    rating_count = len(ratings)

    try:
        similar_items_q = Item_Recommendations.objects.get(asin=asin)
        similar_items_list = [similar_items_q.recomm_asin_1_id, similar_items_q.recomm_asin_2_id,
                              similar_items_q.recomm_asin_3_id,
                              similar_items_q.recomm_asin_4_id, similar_items_q.recomm_asin_5_id,
                              similar_items_q.recomm_asin_6_id,
                              similar_items_q.recomm_asin_7_id, similar_items_q.recomm_asin_8_id,
                              similar_items_q.recomm_asin_9_id,
                              similar_items_q.recomm_asin_10_id]
    except:
        similar_items_list = fetch_top_selling_items(10)

    similar_items, wishlist_status_items = [], []
    for i in similar_items_list:
        similar_item = get_object_or_404(Items, pk=i)
        similar_items.append(similar_item)
        wishlist_status_items.append(check_wishlist_status(user, i))
    similar_items_zip = zip(similar_items, wishlist_status_items)

    return render(
        request,
        'item_recommend/shop-single-product.html',
        {
            'user': user,
            'item': item,
            'wishlist_status': wishlist_status,
            'rating_count': rating_count,
            'ratings': ratings,
            'similar_items_zip': similar_items_zip
        }
    )


def wishlist(request, search_item=None, order_by=None):
    user = user_logged_in(request)

    wishlist_items_list = User_Wishlist.objects.filter(reviewerID=user).order_by('asin')
    wishlist_items = []
    for i in wishlist_items_list:
        wishlist_item = Items.objects.get(asin=i.asin_id)
        wishlist_items.append(wishlist_item)

    curr_page = request.GET.get('page')
    if curr_page:
        page = int(curr_page)
    else:
        page = 1

    paginator = Paginator(wishlist_items, 12)
    page_num = paginator.num_pages
    page_items = paginator.page(page)

    if page_items.has_next():
        next_page = page + 1
    else:
        next_page = page
    if page_items.has_previous():
        previous_page = page - 1
    else:
        previous_page = page

    return render(
        request,
        'item_recommend/shop-wishlist.html',
        {
            'user': user,
            'order_by': order_by,
            'search_item': search_item,

            'items': page_items,
            'total_item_cnt': len(wishlist_items),

            'item_lower_range': page * 12 - 11,
            'item_upper_range': min(page * 12, len(wishlist_items)),
            'page_num': range(page, min(page + 6, page_num + 1)),
            'curr_page': page,
            'next_page': next_page,
            'previous_page': previous_page
        }
    )


def check_wishlist_status(user, asin):
    if User_Wishlist.objects.filter(reviewerID=user, asin=asin):
        return True
    else:
        return False


@login_required
def add_to_wishlist(request, asin):
    user = user_logged_in(request)
    item = get_object_or_404(Items, pk=asin)

    if request.method == 'POST':
        if request.POST.get('add-to-wishlist'):
            print(user, item.asin)
            if User_Wishlist.objects.filter(reviewerID=user, asin=item.asin):
                print('already exists')
            else:
                user_wishlist = User_Wishlist()
                user_wishlist.reviewerID = Customers.objects.filter(reviewerID=user)[0]
                user_wishlist.asin = Items.objects.filter(asin=item.asin)[0]
                user_wishlist.save()
                print('insert success')
            return HttpResponseRedirect(request.META.get('HTTP_REFERER'))


@login_required
def remove_from_wishlist(request, asin):
    user = user_logged_in(request)
    item = get_object_or_404(Items, pk=asin)

    if request.method == 'POST':
        if request.POST.get('remove-from-wishlist'):
            print(user, item.asin)
            if User_Wishlist.objects.filter(reviewerID=user, asin=item.asin):
                User_Wishlist.objects.filter(reviewerID=user, asin=item.asin).delete()
                print('delete success')
            else:
                print('not exist')
            return HttpResponseRedirect(request.META.get('HTTP_REFERER'))
