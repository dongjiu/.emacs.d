import requests
from time import gmtime, strftime

url = "https://edsjobs-wu.westus.cloudapp.azure.com/api/v1/edsjobs?isAdhoc=true"
#url = "https://localhost/api/v1/edsjobs?isAdhoc=true"
custom_headers = { "UserName": "edsjobtrigger", "Password": "e7e4c95c-ebe9-401c-b691-562ecfb4b5dc" }
time_str = strftime("%Y-%m-%d %H:%M:%S", gmtime())
payload = { 'JobName': 'SchedulerJob', 'ScheduledTime': time_str }
r = requests.post(url, headers=custom_headers, json=payload, verify=False)
print(r.status_code)
#print(r.__dict__)

