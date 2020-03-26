#%%
import csv
import pymongo

import json

import os
dbUsername= "loladmin"
dbPassword= "jeanDB"
dbURL = "loldb-9slby.mongodb.net"
# Connect to database
try: 
    client = pymongo.MongoClient("mongodb+srv://"+dbUsername+":"+dbPassword+"@"+dbURL)
    print("Connected successfully!!!") 
    #%%
    print("Available databases:")
    print(client.list_database_names())
except Exception as e:
    print(e)   
    print("Could not connect to MongoDB") 
#%%
print(client.list_database_names())

#%%
for filename in os.listdir("../data/lolDB/champion_data/"):
    with open("../data/lolDB/champion_data/"+filename, "r", encoding='utf-8') as jsonFile:
        jsonobj = json.load(jsonFile) # load le json complet
        data = jsonobj["data"] # entre dans le data
        data = data[list(data.keys())[0]] # entre dans l unique key
        print(client.Champions.fulldata.insert_one(data)) # insere dans la db

#%%
champion = "Alistar"
result = client.Champions.fulldata.find_one({"name":champion})

print(list(result.keys()))
for key in result.keys():
    print(key)

#%%
import cv2

img = cv2.imread("../data/lolDB/img/champion_icon/"+result["image"]["full"]) 
cv2.imshow("icone", img)
cv2.waitKey(0)
#%%
client.close()



#%%

for data  in client.Champions.fulldata.find({}):
    name = data['name']
    del data['_id']
    with open("../data/backup_"+name+".json", "w", encoding='utf-8') as bckp:
        bckp.write(json.dumps(data))

#%%

client.Champions.fulldata.drop()

for filename in filter(lambda x: x.endswith(".json"), os.listdir("../data/")):
    with open("../data/"+filename, "r", encoding='utf-8') as jsonFile:
        jsonobj = json.load(jsonFile) # load le json complet
        print(client.Champions.fulldata.insert_one(jsonobj)) # insere dans la db


#%%
import numpy as np
np.cos((2*np.PI)/3)