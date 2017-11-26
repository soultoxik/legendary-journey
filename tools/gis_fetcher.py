#!/usr/bin/env python
import urllib
from lxml import html

url = "http://www.ilmanlaatu.fi/mittaaminen/verkot/asemat/asemat.php?nws={}"
xpath = "//*[contains(@name, 'ss')]/optgroup/option"

ss_url = "http://www.ilmanlaatu.fi/mittaaminen/verkot/asemat/asemat.php?nws={}&ss={}"
ss_N = "//*[contains(@class, 'teksti12')]/table/tbody"
ss_E = "//*[contains(@class, 'teksti12')]/table/tbody/tr[4]/td[2]"

def foo(s):
    i = 0
    while True:
        try:
            mock = float(s)
            return s, i
        except:
            pass
        print "--> {}".format(s)
        i += 1
        s = s[:-1]
        if len(s) < 3: raise RuntimeError
    raise RuntimeError


with open('./s_ssmap', 'r') as f:
    for line in f:
        sid = line.split()[0]
        ssid = line.split()[1]

        u = url.format(sid)
        page = html.fromstring(urllib.urlopen(u).read())
        ssu = ss_url.format(sid, ssid)
        t = urllib.urlopen(ssu).read()
        nn = t.find('Pohjoiskoordinaatti (&deg;N) </td><td colspan="3">') + len('Pohjoiskoordinaatti (&deg;N) </td><td colspan="3">')
        try:
            n, i = foo(t[nn:nn+8])
            e, i = foo(t[nn+76-i:nn+84-i])
        except:
            continue
        print ssid, n, e
