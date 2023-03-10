# Generated by Django 3.0.5 on 2021-04-27 12:14

from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        ('item_recommend', '0004_auto_20210319_1803'),
    ]

    operations = [
        migrations.CreateModel(
            name='User_Recommendations',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('recomm_asin_1', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_1', to='item_recommend.Items')),
                ('recomm_asin_10', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_10', to='item_recommend.Items')),
                ('recomm_asin_2', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_2', to='item_recommend.Items')),
                ('recomm_asin_3', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_3', to='item_recommend.Items')),
                ('recomm_asin_4', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_4', to='item_recommend.Items')),
                ('recomm_asin_5', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_5', to='item_recommend.Items')),
                ('recomm_asin_6', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_6', to='item_recommend.Items')),
                ('recomm_asin_7', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_7', to='item_recommend.Items')),
                ('recomm_asin_8', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_8', to='item_recommend.Items')),
                ('recomm_asin_9', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='user_recomm_asin_9', to='item_recommend.Items')),
                ('reviewerID', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='item_recommend.Customers')),
            ],
        ),
        migrations.CreateModel(
            name='Item_Recommendations',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('asinID', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, to='item_recommend.Items')),
                ('recomm_asin_1', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_1', to='item_recommend.Items')),
                ('recomm_asin_10', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_10', to='item_recommend.Items')),
                ('recomm_asin_2', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_2', to='item_recommend.Items')),
                ('recomm_asin_3', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_3', to='item_recommend.Items')),
                ('recomm_asin_4', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_4', to='item_recommend.Items')),
                ('recomm_asin_5', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_5', to='item_recommend.Items')),
                ('recomm_asin_6', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_6', to='item_recommend.Items')),
                ('recomm_asin_7', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_7', to='item_recommend.Items')),
                ('recomm_asin_8', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_8', to='item_recommend.Items')),
                ('recomm_asin_9', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='item_recomm_asin_9', to='item_recommend.Items')),
            ],
        ),
    ]
