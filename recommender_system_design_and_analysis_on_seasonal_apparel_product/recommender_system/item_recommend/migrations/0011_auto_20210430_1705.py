# Generated by Django 3.0.5 on 2021-04-30 17:05

from django.db import migrations


class Migration(migrations.Migration):

    dependencies = [
        ('item_recommend', '0010_auto_20210430_1704'),
    ]

    operations = [
        migrations.AlterUniqueTogether(
            name='user_cart',
            unique_together={('reviewerID', 'asin')},
        ),
    ]