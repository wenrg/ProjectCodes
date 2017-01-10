# coding: utf-8
import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.preprocessing import LabelEncoder

print 'Reading om_train.csv'
df = pd.read_csv('om_train_201605.csv')
print 'Reading test_test.csv'
test_set = pd.read_csv('om_test_lag4.csv')

# label_encoder = LabelEncoder()
# label_encoder.fit(df['value'])
# df['value'] = label_encoder.transform(df['value'])
# test_output_features = label_encoder.classes_

test_output_features = ['ind_cco_fin_ult1', 'ind_cno_fin_ult1', 'ind_ctma_fin_ult1',
                        'ind_ctop_fin_ult1', 'ind_ctpp_fin_ult1', 'ind_dela_fin_ult1',
                        'ind_ecue_fin_ult1', 'ind_fond_fin_ult1', 'ind_reca_fin_ult1',
                        'ind_tjcr_fin_ult1', 'ind_valo_fin_ult1',
                        'ind_nomina_ult1', 'ind_nom_pens_ult1',
                        'ind_recibo_ult1']
mapping_dict = {'ind_cco_fin_ult1': 0, 'ind_cno_fin_ult1': 1, 'ind_ctma_fin_ult1': 2,
                'ind_ctop_fin_ult1': 3, 'ind_ctpp_fin_ult1': 4, 'ind_dela_fin_ult1': 5,
                'ind_ecue_fin_ult1': 6, 'ind_fond_fin_ult1': 7, 'ind_reca_fin_ult1': 8,
                'ind_tjcr_fin_ult1': 9, 'ind_valo_fin_ult1': 10,
                'ind_nomina_ult1': 11, 'ind_nom_pens_ult1': 12,
                'ind_recibo_ult1': 13}
df['value'] = df['value'].map(lambda x: mapping_dict[x])


input_features = ['ncodpers', 'ind_empleado',
                  'pais_residencia', 'sexo', 'age', 'ind_nuevo', 'antiguedad',
                  'indrel', 'indrel_1mes', 'tiprel_1mes', 'indresi', 'indext',
                  'canal_entrada', 'indfall', 'nomprov', 'ind_actividad_cliente',
                  'renta', 'segmento','ind0ult1', 'ind1ult1', 'ind2ult1', 'ind3ult1',
                  'ind4ult1', 'ind5ult1', 'ind6ult1', 'ind7ult1', 'ind8ult1',
                  'ind9ult1', 'ind10ult1', 'ind11ult1', 'ind12ult1', 'ind13ult1',
                  'ind14ult1', 'ind15ult1', 'ind16ult1', 'ind17ult1', 'ind18ult1',
                  'ind19ult1', 'ind20ult1', 'ind21ult1', 'ind22ult1','ind23ult1',
                  'n_nom']
output_feature = ['value']
test_features = ['ind_empleado', 'pais_residencia', 'sexo', 'age', 'ind_nuevo', 'antiguedad',
                 'indrel', 'indrel_1mes', 'tiprel_1mes', 'indresi', 'indext',
                 'canal_entrada', 'indfall', 'nomprov', 'ind_actividad_cliente',
                 'renta', 'segmento', 'ind0ult1', 'ind1ult1', 'ind2ult1', 'ind3ult1',
                 'ind4ult1', 'ind5ult1', 'ind6ult1', 'ind7ult1', 'ind8ult1',
                 'ind9ult1', 'ind10ult1', 'ind11ult1', 'ind12ult1', 'ind13ult1',
                 'ind14ult1', 'ind15ult1', 'ind16ult1', 'ind17ult1', 'ind18ult1',
                 'ind19ult1', 'ind20ult1', 'ind21ult1', 'ind22ult1','ind23ult1',
                 'n_nom']
rm_features = ['ind2ult1', 'ind4ult1', 'ind6ult1', 'ind7ult1', 'ind8ult1',
               'ind11ult1', 'ind12ult1', 'ind13ult1', 'ind17ult1', 'ind18ult1',
               'ind19ult1', 'ind21ult1', 'ind22ult1','ind23ult1']


train_x = df[input_features]
train_y = df[output_feature]
test_x = test_set[input_features]
test_id = np.array(test_set['ncodpers'])


xg_train = xgb.DMatrix(train_x[test_features], label = train_y)
xg_test = xgb.DMatrix(test_x[test_features])


param = {'max_depth':8, 'eta':0.05, 'silent':1, 'objective':'multi:softprob',
         'num_class':14, 'eval_metric':'mlogloss', 'min_child_weight':1,
         'colsample_bytree':0.7, 'seed':0, 'subsample':0.7,
         'early_stopping_rounds': 50}
num_round = 10000

print "xgboost modeling ..."
bst = xgb.train(param, xg_train,num_round)

print "xgboost predicting ..."
preds = bst.predict(xg_test)

print "remove old products ..."
for i in range(len(test_x)):
    for j in range(len(rm_features)):
        if test_x[rm_features[j]].iloc[i]:
            preds[i][j] = 0

print 'prediction sorting ...'
preds = np.argsort(preds, axis = 1)
preds = np.fliplr(preds)[:,:7]
final_preds = map(lambda y: " ".join(y),
                  [map(lambda x: test_output_features[x],
                       pred) for pred in preds])

print 'output organizing ...'
output_df = pd.DataFrame({'ncodpers':test_id, 'added_products':final_preds})
output_df.to_csv('submit_results.csv', index = False, columns = ['ncodpers', 'added_products'])

print 'Completed!!!'
