#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import subprocess
import requests
from bs4 import BeautifulSoup

URL = "https://mon-espace.izly.fr/Home/Logon"
USRNM = "" # Add your username
PSSWD = "" # Add your password
HDRS = {'User-Agent' : 'Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101 Firefox/52.0'}

payload = {'username' : USRNM, 'password' : PSSWD, 'returnUrl' : ''}

try:
  r = requests.post(URL, headers=HDRS, data=payload)
except requests.ConnectionError as e:
  subprocess.call(['notify-send', 'Solde Izly', str(e)])
  sys.exit(2)

if r.status_code == requests.codes.ok:
  soup = BeautifulSoup(r.text, 'html.parser')
  sold = soup.findAll('div', {'class' : 'sold-value'})
  sold = sold[0].text.encode('utf-8')
  subprocess.call(['notify-send', 'Solde Izly', sold])
