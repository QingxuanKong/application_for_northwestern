from django.db import models


# Create your models here.
class Items(models.Model):
    asin = models.TextField(unique=True, primary_key=True)
    title = models.TextField(null=True)
    image = models.ImageField(null=True)
    brand = models.TextField(null=True)
    category = models.TextField(null=True)
    category_rank = models.IntegerField(null=True)
    description = models.TextField(null=True)
    price = models.FloatField(null=True)

    def __str__(self):
        return str(self.title)

    def get_image(self):
        return self.image if self.image else 'item_default.jpg'


class Customers(models.Model):
    reviewerID = models.TextField(unique=True, primary_key=True)
    reviewerName = models.TextField(null=True)

    def __str__(self):
        return str(self.reviewerName)


class Ratings(models.Model):
    overall = models.IntegerField(null=True)
    verified = models.TextField(null=True)
    reviewTime = models.DateTimeField(null=True)
    reviewerID = models.ForeignKey('Customers', on_delete=models.CASCADE)
    asin = models.ForeignKey('Items', on_delete=models.CASCADE)
    reviewText = models.TextField(null=True)
    summary = models.TextField(null=True)
    vote = models.IntegerField(null=True)

    def __str__(self):
        return str(self.summary)


class User_Recommendations(models.Model):
    reviewerID = models.ForeignKey('Customers', on_delete=models.CASCADE)
    recomm_asin_1 = models.ForeignKey('Items', related_name='user_recomm_asin_1', on_delete=models.CASCADE)
    recomm_asin_2 = models.ForeignKey('Items', related_name='user_recomm_asin_2', on_delete=models.CASCADE)
    recomm_asin_3 = models.ForeignKey('Items', related_name='user_recomm_asin_3', on_delete=models.CASCADE)
    recomm_asin_4 = models.ForeignKey('Items', related_name='user_recomm_asin_4', on_delete=models.CASCADE)
    recomm_asin_5 = models.ForeignKey('Items', related_name='user_recomm_asin_5', on_delete=models.CASCADE)
    recomm_asin_6 = models.ForeignKey('Items', related_name='user_recomm_asin_6', on_delete=models.CASCADE)
    recomm_asin_7 = models.ForeignKey('Items', related_name='user_recomm_asin_7', on_delete=models.CASCADE)
    recomm_asin_8 = models.ForeignKey('Items', related_name='user_recomm_asin_8', on_delete=models.CASCADE)
    recomm_asin_9 = models.ForeignKey('Items', related_name='user_recomm_asin_9', on_delete=models.CASCADE)
    recomm_asin_10 = models.ForeignKey('Items', related_name='user_recomm_asin_10', on_delete=models.CASCADE)


class Item_Recommendations(models.Model):
    asin = models.ForeignKey('Items', on_delete=models.CASCADE)
    recomm_asin_1 = models.ForeignKey('Items', related_name='item_recomm_asin_1', on_delete=models.CASCADE)
    recomm_asin_2 = models.ForeignKey('Items', related_name='item_recomm_asin_2', on_delete=models.CASCADE)
    recomm_asin_3 = models.ForeignKey('Items', related_name='item_recomm_asin_3', on_delete=models.CASCADE)
    recomm_asin_4 = models.ForeignKey('Items', related_name='item_recomm_asin_4', on_delete=models.CASCADE)
    recomm_asin_5 = models.ForeignKey('Items', related_name='item_recomm_asin_5', on_delete=models.CASCADE)
    recomm_asin_6 = models.ForeignKey('Items', related_name='item_recomm_asin_6', on_delete=models.CASCADE)
    recomm_asin_7 = models.ForeignKey('Items', related_name='item_recomm_asin_7', on_delete=models.CASCADE)
    recomm_asin_8 = models.ForeignKey('Items', related_name='item_recomm_asin_8', on_delete=models.CASCADE)
    recomm_asin_9 = models.ForeignKey('Items', related_name='item_recomm_asin_9', on_delete=models.CASCADE)
    recomm_asin_10 = models.ForeignKey('Items', related_name='item_recomm_asin_10', on_delete=models.CASCADE)

class User_Wishlist(models.Model):
    reviewerID = models.ForeignKey('Customers', on_delete=models.CASCADE)
    asin = models.ForeignKey('Items', on_delete=models.CASCADE)
    class Meta:
        unique_together = ["reviewerID", "asin"]
