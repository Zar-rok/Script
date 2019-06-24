#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import subprocess
import requests
from bs4 import BeautifulSoup

URL = "https://mon-espace.izly.fr/Home/Logon"
USRNM = "" # Add your username
PSSWD = "" # Add your password
HDRS = {'User-Agent' : 'Mozilla/5.0 (X11; Linux x86_64; rv:52.0) Gecko/20100101 Firefox/52.0'}
PROX = {'http': os.environ.get('http_proxy'), 'https': os.environ.get('https_proxy')}

payload = {'username' : USRNM, 'password' : PSSWD, 'returnUrl' : '', 'proxies': PROX}

notify = lambda msg: subprocess.call(['notify-send', 'Solde Izly', msg])

try:
  r = requests.post(URL, headers=HDRS, data=payload)
except requests.ConnectionError as e:
  notify(str(e))
  sys.exit(2)

if r.status_code == requests.codes.ok:
  soup = BeautifulSoup(r.text, 'html.parser')
  sold = soup.findAll('div', {'class' : 'sold-value'})
  sold = sold[0].text.encode('utf-8')
  notify(sold)
else:
  notify('[!] Status code: {:d}'.format(r.status_code))
